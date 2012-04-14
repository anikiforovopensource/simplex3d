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
import scala.collection.mutable.HashMap
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.types._
import simplex3d.engine.util._
import simplex3d.engine.graphics._


/** ShaderPrototype defines a DSL to assist with writing shaders and and linking them together.
 * 
 * Glsl version can be specified using version() declaration.
 * Type remapping to square matrices can be requested with useSquareMatrices() directive.
 * 
 * Function dependencies are declared with use() declaration.
 *
 * Uniform values must be specified inside a uniform{} block using the declare() directive.
 * Glsl qualifiers can be chain using qualify() statement.
 * For example: declare[Vec2]("texCoords").qualify("smooth")
 * 
 * XXX Uniform values can be mapped to an object path on material or environment. This is useful when
 * a vertex shader consumes a part of a struct and a fragment consumes the rest.
 * Example: declare[Vec2]("offset").remap("textureUnit.offset")
 * 
 * XXX Arrays can be remapped using a wildcard array declaration [*]. Only one wildcard declaration
 * is allowed per path, other array instances must specify the exact index.
 * Example: declare[BindingList[Vec3]]("colors").remap("colorMaps[0].color[*]")
 * 
 * XXX Value path remapping can only be used on subclasses of Struct and can only navigate the fields
 * declared in Struct.fields. As a special case, texture dimensions can be accessed this way as well.
 * Example: declare[Vec2i]("dimensions").remap("textureUnit.texture.dimensions")
 * 
 * Value path remapping can impact technique resolution performance, so use it sparingly.
 * 
 * Unsized arrays are allowed only in uniform blocks, their size will be automatically resolved to
 * a size of the corresponding BindingList.
 * 
 * All dependent variables should be defined inside attribute{} or in{} blocks using declare().
 * Only VertexShaders are allowed to have attribute{} blocks. Other shaders must use named in{} block and
 * must prefix the variables with the block name in the source code.
 * 
 * Each shader should either link with the next stage using a combination of out{} block with
 * an entryPoint() declaration, or define a function usable within the same stage
 * using export() declaration. FragmentShaders are allowed to omit out{} block declaration.
 * 
 * Only MathTypes and MathType arrays can be declared inside in{} and out{} blocks.
 * Arrays in in{} and out{} blocks must be size either to a literal value or to a size of some uniform array.
 * If a uniform array is defined in the shader scope, its size can be accessed using injected variable
 * of the form: se_sizeOf_${StructType}_${ArrayName} or simply se_sizeOf_${ArrayName} when array is declared globally.
 * 
 * Shader sources can be attached using src() declarations.
 */
sealed abstract class ShaderPrototype(val shaderType: Shader.type#Value) {
  
  private[this] var _usingSquareMatrices = false
  private[this] var _version: Option[String] = None
  private[this] val _listDeclarationNameKeys = new ArrayBuffer[(String, String)]
  private[this] val _uniformBlock = new ArrayBuffer[Declaration]
  private[this] val _attributeBlock = new ArrayBuffer[Declaration]
  private[this] val _inputBlocks = new ArrayBuffer[DeclarationBlock]
  private[this] val _outputBlocks = new ArrayBuffer[DeclarationBlock]
  private[this] val _functionDependencies = new ArrayBuffer[String]
  private[this] val _sources = new ArrayBuffer[String]
  private[this] var _export: Option[String] = None
  private[this] var _entryPoint: Option[String] = None
  
  final def version = _version
  final def usingSquareMatrices = _usingSquareMatrices
  final val listDeclarationNameKeys = new ReadSeq(_listDeclarationNameKeys)
  final val uniformBlock = new ReadSeq(_uniformBlock)
  final val attributeBlock = new ReadSeq(_attributeBlock)
  final val inputBlocks = new ReadSeq(_inputBlocks)
  final val outputBlocks = new ReadSeq(_outputBlocks)
  final val functionDependencies = new ReadSeq(_functionDependencies)
  final val sources = new ReadSeq(_sources)
  final def export = _export
  final def entryPoint = _entryPoint
  
  
  private[this] var isInitialized = false
  private[this] def checkState() {
    if (isInitialized) throw new IllegalStateException("Modifying shader after it has been initialized.")
  }
  
  private[this] var _structs: ReadArray[StructDeclaration] = null
  def structs = {
    if (!isInitialized) throw new IllegalStateException(
      "This shader prototype must be initialized before accessing structs."
    )
    else _structs
  }
  
  
  final def isVertexShader = (shaderType == Shader.Vertex)
  
  final def init() {
    if (isInitialized) return
    
    // Check name uniqueness.
    val topLevelNames = (uniformBlock ++ attributeBlock).map(_.name) ++ (inputBlocks ++ outputBlocks).map(_.name)
    
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
      
    
    val structs = new HashMap[String, StructDeclaration]
    for (declaration <- uniformBlock) {
      declaration.extractManifestTypeInfo(usingSquareMatrices, structs)
    }
    _structs = new ReadArray(structs.values.toList.sortBy(_.level).reverse.toArray)
    
    isInitialized = true
  }
  
  
  private[this] var declarations: ArrayBuffer[Declaration] = null
  private[this] def processBlock(name: String, blocks: ArrayBuffer[DeclarationBlock], blockBody: => Unit) {
    declarations = new ArrayBuffer[Declaration]
    blockBody
    blocks += new DeclarationBlock(name, new ReadSeq(declarations))
    declarations = null
  }
  
  protected final def useSquareMatrices {
    if (declarations != null) throw new IllegalStateException("useSquareMatrices() must be declared at the top level.")
    checkState()
    if (_usingSquareMatrices) throw new IllegalStateException("Square matrices are already in use.")
    _usingSquareMatrices = true
  }
  
  protected final def version(version: String) {
    if (declarations != null) throw new IllegalStateException("version() must be declared at the top level.")
    checkState()
    if (_version.isDefined) throw new IllegalStateException("Version is already defined.")
    _version = Some(version)
  }
  
  protected final def use(functionSignature: String) {
    if (declarations != null) throw new IllegalStateException("use() must be declared at the top level.")
    checkState()
    _functionDependencies += functionSignature
  }
  
  protected final def declare[B <: TechniqueBinding : ClassManifest](name: String) :Declaration = {
    checkState()
    if (declarations == null) throw new IllegalStateException("declare() must be called inside a block.")
    val declaration = new Declaration(implicitly[ClassManifest[B]], name)
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
    _uniformBlock ++= declarations
    declarations = null
    
    // Process array declarations.
    for (declaration <- uniformBlock) {
      val isArray = classOf[BindingList[_]].isAssignableFrom(declaration.manifest.erasure)
      val noSizeExpression = (declaration.arraySizeExpression == null)
      
      if (isArray && noSizeExpression) {
        _listDeclarationNameKeys += (("", declaration.name))
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
        _listDeclarationNameKeys ++= instance.listDeclarations.map(_.nameKey)
      }
    }
    
    val nodups = _listDeclarationNameKeys.distinct
    _listDeclarationNameKeys.clear()
    _listDeclarationNameKeys ++= nodups
  }
  
  private def verifyMathType(declarations: Iterable[Declaration]) {
    for (declaration <- declarations) {
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
    if (isVertexShader) throw new UnsupportedOperationException(
      "Vertex shader cannot have any input blocks (use attributes {} block instead)."
    )
    checkState()
    
    processBlock(name, _inputBlocks, block)
    verifyMathType(inputBlocks.last.declarations)
    verifyArraysAreSized(inputBlocks.last)
  }
  
  /** "Only a vertex shader can declare an attributes block."
   */
  protected final def attributes(block: => Unit) {
    if (declarations != null) throw new IllegalStateException("in {} must be declared at the top level.")
    if (!isVertexShader) throw new UnsupportedOperationException(
      "Only a vertex shader can declare an attributes block."
    )
    checkState()
    
    declarations = new ArrayBuffer[Declaration]
    block
    _attributeBlock ++= declarations
    declarations = null
    
    verifyMathType(_attributeBlock)
  }
  
  /** If a shader defines one or more output blocks it must define an entry point. Fragment shaders are allowed
   * to have an entry point without an output block.
   */
  protected final def out(name: String)(block: => Unit) {
    if (declarations != null) throw new IllegalStateException("out {} must be declared at the top level.")
    checkState()
    processBlock(name, _outputBlocks, block)
    verifyMathType(outputBlocks.last.declarations)
    verifyArraysAreSized(outputBlocks.last)
  }
  
  /** A shader must either defined an entry point or export a function (but not both).
   */
  protected final def entryPoint(code: String) {
    if (declarations != null) throw new IllegalStateException("entryPoint() must be declared at the top level.")
    checkState()
    if (entryPoint.isDefined) throw new IllegalStateException("Entry point is already defined.")
    _entryPoint = Some(code)
  }
  
  /** A shader must either defined an entry point or export a function (but not both).
   */
  protected final def export(functionSignature: String) {
    if (declarations != null) throw new IllegalStateException("export() must be declared at the top level.")
    checkState()
    if (export.isDefined) throw new IllegalStateException("Export is already defined.")
    _export = Some(functionSignature)
  }
  
  protected final def src(src: String) {
    if (declarations != null) throw new IllegalStateException("src() must be declared at the top level.")
    checkState()
    _sources += src
  }
}

object ShaderPrototype {
  private[this] def format(qualifiers: String) :String = {
    if (qualifiers.isEmpty) "" else qualifiers + " "
  }
  
  private[pluggable] def arraySizeId(parentType: String, name: String) :String = {
    "se_sizeOf_" + (if (parentType == "") "" else parentType + "_") + name
  }
  
  def genSizeDelarationHeader(arrayDeclarations: Iterable[ListDeclarationSizeKey]) :String = {
    var src = ""
      
    for (d <- arrayDeclarations) {
      src += "const int " + arraySizeId(d.parentType, d.name) + " = " + d.size + ";\n"
    }
    
    if (src.isEmpty) src else src + "\n"
  }
  
  def genStructDeclarationHeader_Glsl120(structs: Iterable[StructDeclaration]) :String = {
    var src = ""
      
    for (struct <- structs) {
      src += "struct " + struct.glslType + " {\n"
      
      for ((fieldType, fieldName) <- struct.entries) {
        src += "  " + fieldType + " " + fieldName + ";\n"
      }
      
      if (struct.containsSamplers) src += "  float nvidiaBugWorkaround;\n"
      src += "};\n\n"
    }
    
    src
  }
  
  /** Remapping is a tuple2 (From, To).
   */
  def genGlobalDeclarationHeader_Glsl120(
    isVertexShader: Boolean,
    squareMatrices: Boolean,
    attributeBlock: Iterable[Declaration],
    uniformBlock: Iterable[Declaration],
    inputBlocks: Iterable[DeclarationBlock],
    outputBlocks: Iterable[DeclarationBlock]
  ) :(Seq[(String, String)], String) = {
    val inputQualifier = if (isVertexShader) "attribute" else "varying"
    
    val remappings = new ArrayBuffer[(String, String)]
    def mkRemapping(parent: String, name: String) :(String, String) = {
      val remapped = if (name.startsWith("gl_")) name else "tm_" + parent + "_" + name
      (parent + "." + name, remapped)
    }
    
    
    var src = ""
    
    
    if (!uniformBlock.isEmpty) {
      for (declaration <- uniformBlock; if !declaration.isReserved) {
        val glslType = declaration.extractManifestTypeInfo(squareMatrices)
        src += format(declaration.qualifiers) + "uniform " + glslType + " " +
        {
          if (declaration.isArray) declaration.name + "[" + ShaderPrototype.arraySizeId("", declaration.name) + "]"
          else declaration.name
        } +
        ";\n"
      }
      
      src += "\n"
    }
    
    if (!attributeBlock.isEmpty) {
      var appended = false
      for (declaration <- attributeBlock; if !declaration.isReserved) {
        val glslType = declaration.extractManifestTypeInfo(squareMatrices)
        src += format(declaration.qualifiers) + inputQualifier + " " + glslType + " " +
        {
          if (declaration.isArray) declaration.name + "[" + declaration.arraySizeExpression + "]"
          else declaration.name
        } +
        ";\n"
        appended = true
      }
      if (appended) src += "\n"
    }
    
    
    for (block <- inputBlocks; if !block.declarations.isEmpty) {
      var appended = false
      for (declaration <- block.declarations) {
        val remapping = mkRemapping(block.name, declaration.name)
        remappings += remapping
        val remapped = remapping._2
        
        if (!declaration.isReserved) {
          val glslType = declaration.extractManifestTypeInfo(squareMatrices)
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
        val remapping = mkRemapping(block.name, declaration.name)
        remappings += remapping
        if (!declaration.isReserved && isVertexShader) {
          val glslType = declaration.extractManifestTypeInfo(squareMatrices)
          src += format(declaration.qualifiers) + "varying " + glslType + " " +
          {
            if (declaration.isArray) remapping._2 + "[" + declaration.arraySizeExpression + "]"
            else remapping._2
          } +
          ";\n"
          appended = true
        }
      }
      if (appended) src += "\n"
    }
    
    (remappings, src)
  }
  
  def genFunctionDeclarationHeader(functions: Iterable[String]) :String = {
    if (functions.isEmpty) "" else functions.mkString("", ";\n", ";\n")
  }
  
  private[this] def remap(remapping: Seq[(String, String)], src: String) :String = {
    var res = src
    for (rm <- remapping) {
      res = res.replaceAll("\\b" + rm._1 + "\\b", rm._2)
    }
    res
  }
  
  def genBody(remapping: Seq[(String, String)], sources: Iterable[String]) :String = {
    var src = ""
      
    for (srcChunk <- sources) {
      src += unindent(remap(remapping, srcChunk)) + "\n"
    }
    
    src
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
  
  def genSrc_Glsl120(shader: ShaderPrototype, arrayDeclarations: IndexedSeq[ListDeclarationSizeKey]) :String = {
    val (remapping, globalDeclarations) = genGlobalDeclarationHeader_Glsl120(
      shader.isVertexShader, shader.usingSquareMatrices,
      shader.attributeBlock, shader.uniformBlock, shader.inputBlocks, shader.outputBlocks
    )
    
    (if (shader.version.isDefined) "#version " + shader.version.get + "\n\n" else "") +
    genSizeDelarationHeader(arrayDeclarations) +
    genStructDeclarationHeader_Glsl120(shader.structs) +
    globalDeclarations +
    genFunctionDeclarationHeader(shader.functionDependencies) +
    genBody(remapping, shader.sources)
  }
  
  /** Shaders must precede their dependencies in the chain.
   */
  def genCombinedSrc_Glsl120(chain: Seq[(ShaderPrototype, Seq[ListDeclarationSizeKey])]) :String = {
    if (chain.isEmpty) return "";
    
    val shaders = chain.reverse
    
    var version: Option[String] = None
    var squareMatrices = shaders.head._1.usingSquareMatrices
    val arrayDeclarationSet = new HashSet[ListDeclarationSizeKey]
    val structDeclarationMap = new HashMap[String, StructDeclaration]
    val uniformMap = new HashMap[String, Declaration]
    val attributeMap = new HashMap[String, Declaration]
    val inputMap = new HashMap[String, DeclarationBlock]
    val outputMap = new HashMap[String, DeclarationBlock]
    val sources = new ArrayBuffer[String]
    val entryPoints = new ArrayBuffer[String]
    
    val shaderType = shaders.head._1.shaderType
    val isVertexShader = (shaderType == Shader.Vertex)
    
    for ((shader, arrayDeclarations) <- shaders) {
      if (shader.shaderType != shaderType) throw new IllegalArgumentException(
        "All shaders must be the same type (e.g. all vertex shaders or all fragment shaders)."
      )
      
      if (shader.usingSquareMatrices != squareMatrices) {
        squareMatrices = squareMatrices | shader.usingSquareMatrices
        println(//XXX log warn
          "Combining sources with and without remapping to square matrices."
        )
      }
      if (version.isDefined && shader.version.isDefined) {
        if (version.get != shader.version.get) println(//XXX log
          "Combining shaders with different versions: '" + version.get + "' and '" + shader.version.get + "'."
    		)
    		if (version.get < shader.version.get) version = shader.version
      }
      else {
        version = shader.version
      }
      
      arrayDeclarationSet ++= arrayDeclarations
      structDeclarationMap ++= shader.structs.map(s => (s.glslType, s))
      attributeMap ++= shader.attributeBlock.map(d => (d.name, d))
      uniformMap ++= shader.uniformBlock.map(d => (d.name, d))
      inputMap ++= shader.inputBlocks.map(b => (b.name, b))
      outputMap ++= shader.outputBlocks.map(b => (b.name, b))
      sources ++= shader.sources
      entryPoints ++= shader.entryPoint
    }
    
    
    val (remapping, globalDeclarations) = genGlobalDeclarationHeader_Glsl120(
      isVertexShader, squareMatrices,
      attributeMap.values, uniformMap.values, inputMap.values, outputMap.values
    )
    
    (if (version.isDefined) "#version " + version.get + "\n\n" else "") +
    genSizeDelarationHeader(arrayDeclarationSet) +
    genStructDeclarationHeader_Glsl120(structDeclarationMap.values) +
    globalDeclarations +
    genBody(remapping, sources) +
    "void main() {\n" + entryPoints.map("  " + _).mkString("", "();\n", "();\n") + "}"
  }
}


abstract class VertexShader extends ShaderPrototype(Shader.Vertex)
abstract class FragmentShader extends ShaderPrototype(Shader.Fragment)
