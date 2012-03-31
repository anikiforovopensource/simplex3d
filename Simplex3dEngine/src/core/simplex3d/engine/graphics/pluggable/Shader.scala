/*
 * Simplex3dEngine - Renderer Module
 * Copyright (C) 2011, Aleksey Nikiforov
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


sealed abstract class Shader {
  
  final class Declaration(
    val manifest: ClassManifest[_ <: TechniqueBinding],
    val qualifiers: String, val name: String
  ) {
    
    private[Shader] var arraySizeExpression: String = null
    def size(arraySizeExpression: String) {
      if (!classOf[BindingArray[_]].isAssignableFrom(manifest.erasure)) throw new RuntimeException(
        "Only arrays can be sized."
      )
      if (this.arraySizeExpression != null) throw new RuntimeException(
        "This array is already sized."
      )
      
      this.arraySizeExpression = arraySizeExpression
    }
    
    private[Shader] def extractManifestTypeInfo(unsizedArrays: HashSet[(String, String)]) :String = {
      // Structs must be detected and generate an error via verifyMathType() prior to this call.
      extractManifestTypeInfo(null, unsizedArrays)
    }
    private[Shader] def extractManifestTypeInfo(
      blocks: ArrayBuffer[(String, Class[_], ArrayBuffer[(String, String)])],
      unsizedArrays: HashSet[(String, String)]
    ) :String = {
      extractManifestTypeInfo(manifest, name, blocks, unsizedArrays)
    }
    
    private def resolveTextureType(className: String) :String = {
      className match {
        case "Texture2d" => "sampler2d"
        case _ => throw new RuntimeException
      }
    }
    
    private def processStruct(
      instance: Struct[_],
      blocks: ArrayBuffer[(String, Class[_], ArrayBuffer[(String, String)])],
      unsizedArrays: HashSet[(String, String)]
    ) :String =
    {
      val glslType = instance.getClass.getSimpleName
      
      val blockEntries = new ArrayBuffer[(String, String)]
      for (i <- 0 until instance.fieldNames.length) {
        val fieldName = instance.fieldNames(i)
        val fieldType = extractInstanceTypeInfo(glslType, instance.fields(i), fieldName, blocks, unsizedArrays)
        blockEntries += ((fieldType, fieldName))
      }
      
      val existing = blocks.find(_._1 == glslType)
      if (existing.isDefined) {
        val existingClass = existing.get._2
        if (instance.getClass != existingClass) throw new RuntimeException(
          "Both structs '" + instance.getClass.getName + "' and '" +
          existingClass.getName + "' map to the same glsl type name."
        )
      }
      else {
        blocks += ((glslType, instance.getClass, blockEntries))
      }
      
      glslType
    }
    
    private def extractManifestTypeInfo(
      m: ClassManifest[_], name: String,
      blocks: ArrayBuffer[(String, Class[_], ArrayBuffer[(String, String)])],
      unsizedArrays: HashSet[(String, String)]
    ) :String = {
      
      if (classOf[MathType].isAssignableFrom(m.erasure)) {
        m.erasure.getSimpleName.toLowerCase().dropRight(1)
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
      else if (classOf[BindingArray[_]].isAssignableFrom(m.erasure)) {
        val manifest = try {
          m.typeArguments.head.asInstanceOf[ClassManifest[_]]
        }
        catch {
          case e: Exception => throw new RuntimeException(
            "Undefined or unsupported array type for declaration '" + name + "'."
          )
        }
        
        val arrayType = extractManifestTypeInfo(manifest, name, blocks, unsizedArrays)
        val sizeExpression =
          if (arraySizeExpression != null) arraySizeExpression
          else {
            unsizedArrays += ((null, name)) // That's right, it's a null not an option.
            "se_sizeOf_" + name
          }
        
        val glslType = arrayType + "[" + sizeExpression +  "]"
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
        
        processStruct(instance, blocks, unsizedArrays)
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
      blocks: ArrayBuffer[(String, Class[_], ArrayBuffer[(String, String)])],
      unsizedArrays: HashSet[(String, String)]
    ) :String = {
      
      if (classOf[MathType].isAssignableFrom(i.getClass)) {
        i.getClass.getSimpleName.toLowerCase().dropRight(1)
      }
      else if (classOf[TextureBinding[_]].isAssignableFrom(i.getClass)) {
        resolveTextureType(i.asInstanceOf[TextureBinding[_]].bindingManifest.erasure.getSimpleName)
      }
      else if (classOf[BindingArray[_]].isAssignableFrom(i.getClass)) {
        val instance = i.asInstanceOf[BindingArray[_]].bindingInstance().asInstanceOf[Object]
        val arrayType = extractInstanceTypeInfo(parentType, instance, name, blocks, unsizedArrays)
        unsizedArrays += ((parentType, name))
        val glslType = arrayType + "[" + "se_sizeOf_" + parentType + "_" + name +  "]"
        glslType
      }
      else if (classOf[Struct[_]].isAssignableFrom(i.getClass)) {
        processStruct(i.asInstanceOf[Struct[_]], blocks, unsizedArrays)
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
  
  final class Block(val name: String, val declarations: ReadSeq[Declaration]) {
    override def toString() :String = {
      "Block('" + name + "')(\n" + declarations.mkString("  ", "\n", "") + "\n)"
    }
  }
  
  
  private[this] val _functionDependencies = new ArrayBuffer[String]
  private[this] val _inputBlocks = new ArrayBuffer[Block]
  private[this] val _outputBlocks = new ArrayBuffer[Block]
  private[this] val _sources = new ArrayBuffer[String]
  private[this] var _uniformBlock: Option[Block] = None
  private[this] var _export: Option[String] = None
  private[this] var _entryPoint: Option[String] = None
  
  val functionDependencies = new ReadSeq(_functionDependencies)
  val inputBlocks = new ReadSeq(_inputBlocks)
  val outputBlocks = new ReadSeq(_outputBlocks)
  val sources = new ReadSeq(_sources)
  def uniformBlock = _uniformBlock
  def export = _export
  def entryPoint = _entryPoint

  
  private[this] var isRegistered = false
  private[this] def checkState() {
    if (isRegistered) throw new IllegalStateException("Modifying shader after it has been registered.")
  }
  
  def register() {
    // Check name uniqueness.
    val names = new HashSet[String]
    for (block <- uniformBlock ++ inputBlocks ++ outputBlocks; declaration <- block.declarations) {
      val name = declaration.name
      if (names.contains(name)) throw new RuntimeException("Duplicate variable name: '" + name + "'.")
      names += name
    }
    
    // Check source is present.
    if (sources.isEmpty) throw new RuntimeException("Shader source is missing.")
    
    // Check entry point and function export (no, i will not do xor here).
    if ((!entryPoint.isDefined && !export.isDefined) || (entryPoint.isDefined && export.isDefined))
      throw new RuntimeException(
        "A shader must either defined an entry point or export a function (but not both)."
      )
    
    // Check entry point and output block
    if ((!entryPoint.isDefined && !outputBlocks.isEmpty) || (entryPoint.isDefined && outputBlocks.isEmpty))
      throw new RuntimeException(
        "If a shader defines one or more output blocks it must define an entry point."
      )
    
    isRegistered = true
  }
  
    
  private[this] var declarations: ArrayBuffer[Declaration] = null
  private[this] def processBlock(name: String, blocks: ArrayBuffer[Block], blockBody: => Unit) {
    declarations = new ArrayBuffer[Declaration]
    blockBody
    blocks += new Block(name, new ReadSeq(declarations))
    declarations = null
  }
  
  protected final def uses(functionSignature: String) {
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
    checkState()
    
    declarations = new ArrayBuffer[Declaration]
    block
    _uniformBlock = Some(new Block("uniform", new ReadSeq(declarations)))
    declarations = null
  }
  
  private def verifyMathType(block: Block) {
    for (declaration <- block.declarations) {
      val isMathType = classOf[MathType].isAssignableFrom(declaration.manifest.erasure)
      val isMathTypeArray = 
        if (classOf[BindingArray[_]].isAssignableFrom(declaration.manifest.erasure)) {
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
  
  /** Vertex shader: if the name of the inBlock cannot be resolved then attributes will be used as input.
   */
  protected final def in(name: String)(block: => Unit) {
    checkState()
    processBlock(name, _inputBlocks, block)
    verifyMathType(inputBlocks.last)
  }
  
  /** If a shader defines one or more output blocks it must define an entry point.
   * Fragment shader: the name of the outBlock will be ignored if there are no further shader stages.
   */
  protected final def out(name: String)(block: => Unit) {
    checkState()
    processBlock(name, _outputBlocks, block)
    verifyMathType(outputBlocks.last)
  }
  
  /** A shader must either defined an entry point or export a function (but not both).
   */
  protected final def entryPoint(code: String) {
    checkState()
    if (entryPoint != None) throw new IllegalStateException("Entry point is already defined.")
    _entryPoint = Some(code)
  }
  
  /** A shader must either defined an entry point or export a function (but not both).
   */
  protected final def exports(functionSignature: String) {
    checkState()
    if (export != None) throw new IllegalStateException("Export is already defined.")
    _export = Some(functionSignature)
  }
  
  protected final def src(src: String) {
    checkState()
    _sources += src
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
  
  def generateFullSource_Gl21() :String = {
    if (!isRegistered) throw new IllegalStateException("Shader must be registered to generate source.")
    
    val structs = new ArrayBuffer[(String, Class[_], ArrayBuffer[(String, String)])]
    val unsizedArrays = new HashSet[(String, String)]
    
    val inputQualifier = if (this.isInstanceOf[VertexShader]) "attribute" else "varying"
    var src = "\n"
    
    
    if (!functionDependencies.isEmpty) {
      for (dep <- functionDependencies) { src += dep + ";\n" }
      src += "\n"
    }
    
    
    var declarationSrc = ""
    
    if (uniformBlock.isDefined) {
      val block = uniformBlock.get
      
      for (declaration <- block.declarations) {
        val glslType = declaration.extractManifestTypeInfo(structs, unsizedArrays)
        declarationSrc += format(declaration.qualifiers) + "uniform " + glslType + " " + declaration.name + ";\n"
      }
      
      declarationSrc += "\n\n"
    }
    
    
    if (!inputBlocks.isEmpty) {
      for (block <- inputBlocks; if !block.declarations.isEmpty) {
        for (declaration <- block.declarations) {
          val glslType = declaration.extractManifestTypeInfo(unsizedArrays)
          declarationSrc += format(declaration.qualifiers) + inputQualifier + " " + glslType + " " + declaration.name + ";\n"
        }
        declarationSrc += "\n"
      }
      declarationSrc += "\n"
    }
    
    
    if (!outputBlocks.isEmpty && this.isInstanceOf[VertexShader]) {
      for (block <- outputBlocks; if !block.declarations.isEmpty) {
        for (declaration <- block.declarations) {
          val glslType = declaration.extractManifestTypeInfo(unsizedArrays)
          declarationSrc += format(declaration.qualifiers) + "varying " + glslType + " " + declaration.name + ";\n"
        }
        declarationSrc += "\n"
      }
    }
    
    
    if (!unsizedArrays.isEmpty) {
      for ((parentType, name) <- unsizedArrays.toSeq.sortBy(t => t._1 + t._2)) {
        val sizeId = (if (parentType == null) "" else parentType + "_") + name
        src += "const int se_sizeOf_" + sizeId + " = " + "$" + sizeId + "$;\n"
      }
      src += "\n"
    }
    
    for (struct <- structs) {
      val name = struct._1
      val fields = struct._3
      
      src += "struct " + name + " {\n"
      
      for ((fieldType, fieldName) <- fields) {
        src += "  " + fieldType + " " + fieldName + ";\n"
      }
      
      src += "}\n\n"
    }
    
    src += declarationSrc
    
    for (srcChunk <- sources) {
      src += unindent(srcChunk) + "\n"
    }
    
    src
  }
}

abstract class VertexShader extends Shader
abstract class FragmentShader extends Shader
