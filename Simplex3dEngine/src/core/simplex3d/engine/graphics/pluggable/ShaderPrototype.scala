/*
 * Simplex3dEngine - Renderer Module
 * Copyright (C) 2012, Aleksey Nikiforov
 *
 * This file is part of Simplex3dEngine.
 *
 * Simplex3dEngine is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dEngine is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.engine
package graphics.pluggable

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.types._
import simplex3d.engine.util._
import simplex3d.engine.graphics._


/** ShaderPrototype defines a shader DSL to assist with writing shaders and and linking them together.
 * 
 * Function dependencies are declared with use() declaration.
 * 
 * All dependent variables should be defined inside attribute{} or in{} blocks using declare().
 * Only VertexShaders are allowed to have attribute{} blocks. Other shaders must use named in{} block and
 * must prefix the variables with the block name in the source code.
 * 
 * Each shader should either link with the next stage using a combination of out{} block with
 * an entryPoint() declaration, or it should define a function usable within the same stage
 * using export() declaration. FragmentShaders are allowed to omit out{} block declaration.
 * 
 * Shader sources can be attached using src() declarations.
 * 
 * Only MathTypes and MathType arrays can be declared inside in{} and out{} blocks.
 * Arrays in in{} and out{} blocks must be size either to a literal value or to a size of some uniform array.
 * Uniform array sizes can be accessed using injected variables of the form: se_sizeOf_${StructType}_${ArrayName}
 * 
 * Unsized arrays are only allowed in uniform blocks, their size will be automatically resolved to
 * a size of the corresponding BindingList.
 */
sealed abstract class ShaderPrototype(val shaderType: Shader.type#Value) {
  
  final class StructBlock(val erasure: Class[_], val glslType: String, var nested: Boolean) {
    val entries = new ArrayBuffer[(String, String)] // (glslType, name) pairs
  }
  
  final class Declaration(
    val manifest: ClassManifest[_ <: TechniqueBinding],
    val qualifiers: String, val name: String
  ) {
    val isPredefined = name.startsWith("se_")
    val isReserved = name.startsWith("gl_")
    val isArray: Boolean = classOf[BindingList[_]].isAssignableFrom(manifest.erasure)
    
    private[ShaderPrototype] var arraySizeExpression: String = null
    
    /** The simple way to link input/output array sizes is using a size of a uniform array: a.length(); 
     */
    def size(arraySizeExpression: String) {
      if (!classOf[BindingList[_]].isAssignableFrom(manifest.erasure)) throw new RuntimeException(
        "Only arrays can be sized."
      )
      if (this.arraySizeExpression != null) throw new RuntimeException(
        "This array is already sized."
      )
      
      this.arraySizeExpression = arraySizeExpression
    }
    
    private[ShaderPrototype] def extractManifestTypeInfo() :String = {
      // Structs must be detected and generate an error via verifyMathType() prior to this call.
      extractManifestTypeInfo(null)
    }
    private[ShaderPrototype] def extractManifestTypeInfo(
      blocks: ArrayBuffer[StructBlock]
    ) :String = {
      extractManifestTypeInfo("", manifest, name, blocks)
    }
    
    private def resolveTextureType(className: String) :String = {
      className match {
        case "Texture2d" => "sampler2D"
        case _ => throw new RuntimeException
      }
    }
    
    private def processStruct(
      instance: Struct[_],
      blocks: ArrayBuffer[StructBlock],
      nested: Boolean
    ) :String =
    {
      val glslType = instance.getClass.getSimpleName
      val structBlock = new StructBlock(instance.getClass, glslType, nested)
      
      for (i <- 0 until instance.fieldNames.length) {
        val fieldName = instance.fieldNames(i)
        val (fieldType, arraySizeExpression) = extractInstanceTypeInfo(glslType, instance.fields(i), fieldName, blocks)
        val append = if (arraySizeExpression != "") "[" + arraySizeExpression + "]" else ""
        structBlock.entries += ((fieldType, fieldName + append))
      }
      
      val existing = blocks.find(_.glslType == glslType)
      if (existing.isDefined) {
        if (nested) existing.get.nested = true
        
        val existingClass = existing.get.erasure
        if (instance.getClass != existingClass) throw new RuntimeException(
          "Both structs '" + instance.getClass.getName + "' and '" +
          existingClass.getName + "' map to the same glsl type name."
        )
      }
      else {
        blocks += structBlock
      }
      
      glslType
    }
    
    private def mathTypeName(erasure: Class[_]) :String = {
      val className = erasure.getSimpleName.toLowerCase()
      if (className.endsWith("ref")) {
        val shorter = className.dropRight(3)
        if (shorter == "double") "float" else shorter
      }
      else className.dropRight(1)
    }
    private def extractManifestTypeInfo(
      parentType: String,
      m: ClassManifest[_], name: String,
      blocks: ArrayBuffer[StructBlock]
    ) :String = {
      
      if (classOf[MathType].isAssignableFrom(m.erasure)) {
        mathTypeName(m.erasure)
      }
      else if (classOf[TextureBinding[_]].isAssignableFrom(m.erasure)) {
        try {
          resolveTextureType(m.typeArguments.head.asInstanceOf[ClassManifest[_]].erasure.getSimpleName) 
        }
        catch {
          case e: Exception => throw new RuntimeException(
            "Undefined or unsupported texture binding type for declaration '" + name + "'."
          )
        }
      }
      else if (classOf[BindingList[_]].isAssignableFrom(m.erasure)) {
        val manifest = try {
          m.typeArguments.head.asInstanceOf[ClassManifest[_]]
        }
        catch {
          case e: Exception => throw new RuntimeException(
            "Undefined or unsupported array type for declaration '" + name + "'."
          )
        }
        
        val glslType = extractManifestTypeInfo(parentType, manifest, name, blocks)
        glslType
      }
      else if (classOf[Struct[_]].isAssignableFrom(m.erasure)) {
        val instance = try {
          m.erasure.newInstance().asInstanceOf[Struct[_]]
        }
        catch {
          case e: Exception => throw new RuntimeException(
            "Unable to create an instance of '" + m.erasure.getName +
            "'. All Struct subclasses must define a no-argument constructor."
          )
        }
        
        processStruct(instance, blocks, false)
      }
      else {
        throw new RuntimeException(
          "Unsupported type '" + m.erasure.getSimpleName + "' for declaration '" + name + "'."
        )
      }
    }
    
    private def extractInstanceTypeInfo(
      parentType: String,
      i: Object, name: String,
      blocks: ArrayBuffer[StructBlock]
    ) :(String, String) = {
      
      if (classOf[MathType].isAssignableFrom(i.getClass)) {
        (mathTypeName(i.getClass), "")
      }
      else if (classOf[TextureBinding[_]].isAssignableFrom(i.getClass)) {
        (resolveTextureType(i.asInstanceOf[TextureBinding[_]].bindingManifest.erasure.getSimpleName), "")
      }
      else if (classOf[BindingList[_]].isAssignableFrom(i.getClass)) {
        val manifest = i.asInstanceOf[BindingList[_]].elementManifest
        val glslType = extractManifestTypeInfo(parentType, manifest, name, blocks)
        
        val sizeExpression =
          if (arraySizeExpression != null) arraySizeExpression
          else arraySizeId(parentType, name)
        
        (glslType, sizeExpression)
      }
      else if (classOf[Struct[_]].isAssignableFrom(i.getClass)) {
        (processStruct(i.asInstanceOf[Struct[_]], blocks, true), "")
      }
      else {
        throw new RuntimeException(
          "Unsupported type '" + i.getClass.getSimpleName + "' for declaration '" + name + "'."
        )
      }
    }
    
    override def toString() :String = {
      def extractTypeString(m: ClassManifest[_]) :String = {
        m.erasure.getSimpleName() + {
          m.typeArguments match {
            case List(t: ClassManifest[_]) => "[" + extractTypeString(t) + "]"
            case _ => ""
          }
        }
      }
      "Declaraion(" + qualifiers + " " + extractTypeString(manifest) + " " + name + ")"
    }
  }
  
  final class DeclarationBlock(val name: String, val declarations: ReadSeq[Declaration]) {
    override def toString() :String = {
      "DeclarationBlock('" + name + "')(\n" + declarations.mkString("  ", "\n", "") + "\n)"
    }
  }
  
  
  private[this] val _functionDependencies = new ArrayBuffer[String]
  private[this] val _inputBlocks = new ArrayBuffer[DeclarationBlock]
  private[this] val _outputBlocks = new ArrayBuffer[DeclarationBlock]
  private[this] var _listDeclarationKeys = new ArrayBuffer[(String, String)]
  private[this] val _sources = new ArrayBuffer[String]
  private[this] var _uniformBlock: Option[DeclarationBlock] = None
  private[this] var _export: Option[String] = None
  private[this] var _entryPoint: Option[String] = None
  private[this] var _version: Option[String] = None
  
  
  val functionDependencies = new ReadSeq(_functionDependencies)
  val inputBlocks = new ReadSeq(_inputBlocks)
  val outputBlocks = new ReadSeq(_outputBlocks)
  val listDeclarationNameKeys = new ReadSeq(_listDeclarationKeys)
  val sources = new ReadSeq(_sources)
  def uniformBlock = _uniformBlock
  def export = _export
  def entryPoint = _entryPoint
  def version = _version

  
  private[this] var isRegistered = false
  private[this] def checkState() {
    if (isRegistered) throw new IllegalStateException("Modifying shader after it has been registered.")
  }
  
  def register() {
    // Check name uniqueness.
    val topLevelNames =
      if (isInstanceOf[VertexShader]) {
        (uniformBlock ++ inputBlocks).flatten(_.declarations).map(_.name) ++ outputBlocks.map(_.name)
      }
      else {
        uniformBlock.flatten(_.declarations).map(_.name) ++ (inputBlocks ++ outputBlocks).map(_.name)
      }
      
    val names = new HashSet[String]
    for (name <- topLevelNames) {
      if (names.contains(name)) throw new RuntimeException(
        "Duplicate top-level shader variable name: '" + name + "'."
      )
      names += name
    }
    
    // Check source is present.
    if (sources.isEmpty) throw new RuntimeException("Shader source is missing.")
    
    // Check entry point and function export.
    if (!entryPoint.isDefined && !export.isDefined) throw new RuntimeException(
      "A shader must defined an entry point or export a function."
    )
    else if (entryPoint.isDefined && export.isDefined) throw new RuntimeException(
      "A shader must either defined an entry point or export a function (but not both)."
    )
      
    
    // Check entry point and output block
    if (!entryPoint.isDefined && !outputBlocks.isEmpty) throw new RuntimeException(
      "If a shader defines an output block it must define an entry point."
    )
    else if (entryPoint.isDefined && outputBlocks.isEmpty && !this.isInstanceOf[FragmentShader])
      throw new RuntimeException(
        "If a non-fragment shader defines an entry point it must define an output block."
      )
      
    
    isRegistered = true
  }
  
    
  private[this] var declarations: ArrayBuffer[Declaration] = null
  private[this] def processBlock(name: String, blocks: ArrayBuffer[DeclarationBlock], blockBody: => Unit) {
    declarations = new ArrayBuffer[Declaration]
    blockBody
    blocks += new DeclarationBlock(name, new ReadSeq(declarations))
    declarations = null
  }
  
  protected final def version(version: String) {
    if (declarations != null) throw new IllegalStateException("entryPoint() must be declared at the top level.")
    checkState()
    if (_version != None) throw new IllegalStateException("Version is already defined.")
    _version = Some(version)
  }
  
  protected final def use(functionSignature: String) {
    if (declarations != null) throw new IllegalStateException("use() must be declared at the top level.")
    checkState()
    _functionDependencies += functionSignature
  }
  
  protected final def declare[B <: TechniqueBinding : ClassManifest](name: String) :Declaration = {
    declare("", name)
  }
  protected final def declare[B <: TechniqueBinding : ClassManifest](qualifiers: String, name: String) :Declaration = {
    checkState()
    if (declarations == null) throw new IllegalStateException("declare() must be called inside a block.")
    val declaration = new Declaration(implicitly[ClassManifest[B]], qualifiers, name)
    declarations += declaration
    declaration
  }
  
  /** All uniforms must be declared in the same block.
   * Engine structs may be translated to GLSl uniform blocks depending on the implementation.
   */
  protected final def uniform(block: => Unit) {
    if (declarations != null) throw new IllegalStateException("uniform {} must be declared at the top level.")
    checkState()
    
    declarations = new ArrayBuffer[Declaration]
    block
    _uniformBlock = Some(new DeclarationBlock(null, new ReadSeq(declarations)))
    declarations = null
    
    // Process array declarations.
    for (declaration <- uniformBlock.get.declarations) {
      val isArray = classOf[BindingList[_]].isAssignableFrom(declaration.manifest.erasure)
      val noSizeExpression = (declaration.arraySizeExpression == null)
      
      if (isArray && noSizeExpression) {
        _listDeclarationKeys += (("", declaration.name))
      }
      
      val manifest = 
        if (isArray) {
          try {
            declaration.manifest.typeArguments.head.asInstanceOf[ClassManifest[_]]
          }
          catch {
            case e: Exception => throw new RuntimeException(
              "Undefined or unsupported array type for declaration '" + declaration.name + "'."
            )
          }
        }
        else {
          declaration.manifest
        }
      
      if (classOf[Struct[_]].isAssignableFrom(manifest.erasure)) {
        val instance = manifest.erasure.newInstance().asInstanceOf[Struct[_]]
        _listDeclarationKeys ++= instance.listDeclarations.map(_.nameKey)
      }
    }
    
    val nodups = _listDeclarationKeys.distinct
    _listDeclarationKeys.clear()
    _listDeclarationKeys ++= nodups
  }
  
  private def verifyMathType(block: DeclarationBlock) {
    for (declaration <- block.declarations) {
      val isMathType = classOf[MathType].isAssignableFrom(declaration.manifest.erasure)
      val isMathTypeArray = 
        if (classOf[BindingList[_]].isAssignableFrom(declaration.manifest.erasure)) {
          classOf[MathType].isAssignableFrom(
            declaration.manifest.typeArguments.head.asInstanceOf[ClassManifest[_]].erasure
          )
        }
        else false
        
      if (!isMathType && !isMathTypeArray) throw new RuntimeException(
        "Only math types and math type arrays can be declared in input and output blocks."
      )
    }
  }
  
  private def verifyArraysAreSized(block: DeclarationBlock) {
    for (declaration <- block.declarations) {
      if (classOf[BindingList[_]].isAssignableFrom(declaration.manifest.erasure)) {
        if (declaration.arraySizeExpression == null) throw new RuntimeException(
          "Arrays  must be sized when declared inside in{} and out{} blocks."
        )
      }
    }
  }
  
  /** Vertex shader cannot have any input blocks (use attributes block instead).
   */
  protected final def in(name: String)(block: => Unit) {
    if (declarations != null) throw new IllegalStateException("in {} must be declared at the top level.")
    if (this.isInstanceOf[VertexShader]) throw new UnsupportedOperationException(
      "Vertex shader cannot have any input blocks (use attributes {} block instead)."
    )
    checkState()
    
    processBlock(name, _inputBlocks, block)
    verifyMathType(inputBlocks.last)
    verifyArraysAreSized(inputBlocks.last)
  }
  
  /** "Only a vertex shader can declare an attributes block."
   */
  protected final def attributes(block: => Unit) {
    if (declarations != null) throw new IllegalStateException("in {} must be declared at the top level.")
    if (!this.isInstanceOf[VertexShader]) throw new UnsupportedOperationException(
      "Only a vertex shader can declare an attributes block."
    )
    
    checkState()
    processBlock(null, _inputBlocks, block)
    verifyMathType(inputBlocks.last)
  }
  
  /** If a shader defines one or more output blocks it must define an entry point. Fragment shaders are allowed
   * to have an entry point without an output block.
   */
  protected final def out(name: String)(block: => Unit) {
    if (declarations != null) throw new IllegalStateException("out {} must be declared at the top level.")
    checkState()
    processBlock(name, _outputBlocks, block)
    verifyMathType(outputBlocks.last)
    verifyArraysAreSized(outputBlocks.last)
  }
  
  /** A shader must either defined an entry point or export a function (but not both).
   */
  protected final def entryPoint(code: String) {
    if (declarations != null) throw new IllegalStateException("entryPoint() must be declared at the top level.")
    checkState()
    if (entryPoint != None) throw new IllegalStateException("Entry point is already defined.")
    _entryPoint = Some(code)
  }
  
  /** A shader must either defined an entry point or export a function (but not both).
   */
  protected final def export(functionSignature: String) {
    if (declarations != null) throw new IllegalStateException("export() must be declared at the top level.")
    checkState()
    if (export != None) throw new IllegalStateException("Export is already defined.")
    _export = Some(functionSignature)
  }
  
  protected final def src(src: String) {
    if (declarations != null) throw new IllegalStateException("src() must be declared at the top level.")
    checkState()
    _sources += src
  }
  
  
  private def arraySizeId(parentType: String, name: String) :String = {
    "se_sizeOf_" + (if (parentType == "") "" else parentType + "_") + name
  }
  
  def generateSizeDelarationHeader(declarations: IndexedSeq[ListDeclarationSizeKey]) :String = {
    var header = ""
      
    var i = 0; while (i < declarations.size) { val d = declarations(i)
      header += "const int " + arraySizeId(d.parentType, d.name) + " = " + d.size + ";\n"
      
      i += 1
    }
    
    header
  }
  
  private[this] def unindent(src: String) :String = {
    val codeLines = src.split("\n")

    val lines = codeLines.map(_.replace("\t", "  "))
    val Spaces = """^(\s*).*""".r
    val Spaces(hs) = lines.head
    var min = scala.Int.MaxValue

    for (line <- lines) {
      val Spaces(s) = line
      if (s != null && !line.trim.isEmpty && s.length < min) min = s.length
    }

    lines.map(_.drop(min)).mkString("\n")
  }
  
  private[this] def format(qualifiers: String) :String = {
    if (qualifiers.isEmpty) "" else qualifiers + " "
  }
  
  lazy val src_Gl21 = generateSource_Gl21()
  private def generateSource_Gl21() :String = {
    if (!isRegistered) throw new IllegalStateException("Shader must be registered to generate source.")
    
    val structs = new ArrayBuffer[StructBlock]
    val inputQualifier = if (this.isInstanceOf[VertexShader]) "attribute" else "varying"
    
    class Remapping(val parent: String, val name: String) {
      val remapped = if (name.startsWith("gl_")) name else "tm_" + parent + "_" + name
    }
    val remappings = new ArrayBuffer[Remapping]
    def remap(src: String) :String = {
      var res = src
      for (remapping <- remappings) {
        res = res.replaceAll("\\b" + remapping.parent + "\\." + remapping.name + "\\b", remapping.remapped)
      }
      res
    }
    
    var src = ""
    
    if (!functionDependencies.isEmpty) {
      for (dep <- functionDependencies) { src += dep + ";\n" }
      src += "\n"
    }
    
    
    var declarationSrc = ""
    
    if (uniformBlock.isDefined) {
      val block = uniformBlock.get
      
      for (declaration <- block.declarations; if !declaration.isReserved) {
        val glslType = declaration.extractManifestTypeInfo(structs)
        declarationSrc += format(declaration.qualifiers) + "uniform " + glslType + " " +
        {
          if (declaration.isArray) declaration.name + "[" + arraySizeId("", declaration.name) + "]"
          else declaration.name
        } +
        ";\n"
      }
      
      declarationSrc += "\n"
    }
    
    
    for (struct <- structs) {
      src += "struct " + struct.glslType + " {\n"
      
      for ((fieldType, fieldName) <- struct.entries) {
        src += "  " + fieldType + " " + fieldName + ";\n"
      }
      
      src += "};\n\n"
    }
    
    src += declarationSrc
    
    
    for (block <- inputBlocks; if !block.declarations.isEmpty) {
      var appended = false
      for (declaration <- block.declarations) {
        val remapped =
          if (this.isInstanceOf[VertexShader]) {
            declaration.name
          } 
          else {
            val remapping = new Remapping(block.name, declaration.name)
            remappings += remapping
            remapping.remapped
          }
        if (!declaration.isReserved) {
          val glslType = declaration.extractManifestTypeInfo()
          src += format(declaration.qualifiers) + inputQualifier + " " + glslType + " " +
          {
            if (declaration.isArray) remapped + "[" + declaration.arraySizeExpression + "]"
            else remapped
          } +
          ";\n"
          appended = true
        }
      }
      if (appended) src += "\n"
    }
    
    
    for (block <- outputBlocks; if !block.declarations.isEmpty) {
      var appended = false
      for (declaration <- block.declarations) {
        val remapping = new Remapping(block.name, declaration.name)
        remappings += remapping
        if (!declaration.isReserved && this.isInstanceOf[VertexShader]) {
          val glslType = declaration.extractManifestTypeInfo()
          src += format(declaration.qualifiers) + "varying " + glslType + " " +
          {
            if (declaration.isArray) remapping.remapped + "[" + declaration.arraySizeExpression + "]"
            else remapping.remapped
          } +
          ";\n"
          appended = true
        }
      }
      if (appended) src += "\n"
    }
    
    
    for (srcChunk <- sources) {
      src += unindent(remap(srcChunk)) + "\n"
    }
    
    src
  }
  
  def fullSrc_Gl21(declarations: IndexedSeq[ListDeclarationSizeKey]) :String = {
    (if (version.isDefined) "#version " + version.get + "\n\n" else "") +
    (if (!declarations.isEmpty) generateSizeDelarationHeader(declarations) + "\n" else "") +
    src_Gl21
  }
}

abstract class VertexShader extends ShaderPrototype(Shader.Vertex)
abstract class FragmentShader extends ShaderPrototype(Shader.Fragment)
