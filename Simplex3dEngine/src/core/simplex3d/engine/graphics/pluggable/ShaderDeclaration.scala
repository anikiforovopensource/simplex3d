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

import java.util.logging._
import scala.collection.immutable._
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.types._
import simplex3d.engine.util._
import simplex3d.engine.graphics._


/** ShaderPrototype defines a DSL to assist with writing shaders and and linking them together.
 * 
 * Glsl version is inferred based on profile. For Glsl 1.2 non-suqare matrices are re-mapped to square matrices.
 * 
 * Function dependencies are declared with use() declaration.
 *
 * Uniform values must be specified inside a uniform{} block using the declare() directive.
 * Glsl qualifiers can be chained using qualify() statement.
 * For example: declare[Vec2]("texCoords").qualify("smooth")
 * 
 * Unsized arrays are allowed only in uniform blocks, their size will be automatically resolved to
 * a size of the corresponding BindingList.
 * 
 * All dependent variables should be defined inside attribute{} or in{} blocks using declare().
 * Only VertexShaders are allowed to have attribute{} blocks. Other shaders must use named in{} block and
 * must prefix the variables with the block name in the source code.
 * Only MathTypes and MathType arrays can be declared inside in{} and out{} blocks.
 * 
 * Arrays in in{} and out{} blocks must be sized either to a literal value or to a size of some uniform array.
 * If a uniform array is defined in the shader scope, its size can be accessed using injected variable
 * of the form: se_sizeOf_${StructType}_${ArrayName} or simply se_sizeOf_${ArrayName} when array is declared globally.
 * 
 * Each shader can link with the next stage using a combination of out{} block with an entryPoint() declaration.
 * A shader with an entry point can share one computed value using entryPoint().out(). Additionally,
 * shared pre-computed values can be requested within entryPoint().in{} block.
 * If a shader does not provide an entryPoint(), then it must define a function usable within the same stage
 * using export() declaration.
 * 
 * Shader sources can be attached using src() declarations.
 */
sealed abstract class ShaderDeclaration(val shaderType: Shader.type#Value) {
  
  final class Declaration private[ShaderDeclaration] (val manifest: ClassManifest[_ <: Binding], val name: String)
  {
    private[ShaderDeclaration] var qualifiers = ""
    private[ShaderDeclaration] var arraySizeExpression = ""
    
    /** The simple way to link input/output array sizes is using a size of a uniform array: a.length(); 
     */
    def size(arraySizeExpression: String) :this.type = {
      if (!classOf[BindingList[_]].isAssignableFrom(manifest.erasure)) throw new RuntimeException(
        "Only arrays can be sized."
      )
      if (!this.arraySizeExpression.isEmpty) throw new RuntimeException(
        "This array is already sized."
      )
      
      val expr = arraySizeExpression.trim
      if (expr.isEmpty) throw new IllegalArgumentException("Array size expression must not be empty.")
      
      this.arraySizeExpression = expr
      this
    }
    
    def qualify(qualifiers: String) :this.type = {
      if (qualifiers.trim.isEmpty) new IllegalArgumentException("Qualifier must not be empty.")
      
      this.qualifiers = (this.qualifiers + " " + qualifiers).trim
      this
    }
    
    override def toString() :String = {
      def extractTypeString(m: ClassManifest[_]) :String = {
        ClassUtil.simpleName(m.erasure) + {
          m.typeArguments match {
            case List(t: ClassManifest[_]) => "[" + extractTypeString(t) + "]"
            case _ => ""
          }
        }
      }
      "Declaraion(" + qualifiers + " " + extractTypeString(manifest) + " " + name + ")"
    }
  }
  
  
  private[this] var exportSignature: Option[String] = None
  private[this] var entryPointSignature: Option[String] = None
  private[this] var conditions = Set[(String, AnyRef => Boolean)]()
  
  private[this] var uniformBlock: Set[Declaration] = null
  private[this] var boundUniforms = Map[String, Property[UncheckedBinding]]()
  private[this] var unsizedArrayKeys = Set[(String, String)]()//(structName, arrayName)
  
  private[this] var attributeBlock: Set[Declaration] = null
  
  private[this] var inputBlocks = Set[DeclarationBlock]()
  private[this] var outputBlocks = Set[DeclarationBlock]()
  
  private[this] var functionDependencies = Set[String]()
  
  private[this] var sources = Set[String]()
  
  
  private[this] var processingUniforms = false
  private[this] var declarations: Set[Declaration] = null
  private[this] def processBlock(blockBody: () => Unit) :Set[Declaration] = {
    declarations = Set[Declaration]()
    blockBody()
    val res = declarations; declarations = null
    res
  }
  private[this] def atTopLevel = (declarations == null)
  private[this] def atUniformBlock = (!atTopLevel && processingUniforms)
  
  private[this] def remapDeclarations(declarations: Set[Declaration]) = {
    if (declarations == null) ReadArray.Empty
    else {
      type Dec = simplex3d.engine.graphics.pluggable.Declaration
      val array = declarations.map(d => new Dec(d.qualifiers, d.manifest, d.name, d.arraySizeExpression)).toArray
      new ReadArray(array)
    }
  }
  
  
  protected var name = this.hashCode.toString
  protected var logRejected = false
  protected var logAccepted = false
  
  protected final def bind[T <: Accessible with Binding](name: String, binding: Property[T]) {
    if (!atUniformBlock) throw new IllegalStateException("bind() must be declared in a uniform{} block.")
    boundUniforms += ((name, binding.asInstanceOf[Property[UncheckedBinding]]))
    declarations += new Declaration(ClassUtil.rebuildManifest(binding.get), name)
  }
  
  protected final def use(functionSignature: String) {
    if (!atTopLevel) throw new IllegalStateException("use() must be declared at the top level.")
    functionDependencies += functionSignature
  }
  
  protected final def declare[B <: Binding : ClassManifest](name: String) :Declaration = {
    if (atTopLevel) throw new IllegalStateException("declare() must be called inside a block.")
    val declaration = new Declaration(implicitly[ClassManifest[B]], name)
    declarations += declaration
    declaration
  }
  
  /** All uniforms must be declared in the same block.
   * Engine structs may be translated to GLSl uniform blocks depending on the implementation.
   */
  protected final def uniform(block: => Unit) {
    if (!atTopLevel) throw new IllegalStateException("uniform{} must be declared at the top level.")
    if (uniformBlock != null) throw new IllegalStateException("Only one uniform{} block can be declared.")
    
    processingUniforms = true
    uniformBlock = processBlock(() => block)
    processingUniforms = false
    
    // Process array declarations.
    for (declaration <- uniformBlock) {
      val isArray = classOf[BindingList[_]].isAssignableFrom(declaration.manifest.erasure)
      val noSizeExpression = (declaration.arraySizeExpression.isEmpty)
      
      if (isArray && noSizeExpression) {
        unsizedArrayKeys += (("", declaration.name))
      }
      
      val manifest = 
        if (isArray) {
          try {
            declaration.manifest.typeArguments.head.asInstanceOf[ClassManifest[_]]
          }
          catch {
            case e: Exception => throw new RuntimeException(
              "Undefined or unsupported array type for declaration '" + declaration.name + "'.")
          }
        }
        else {
          declaration.manifest
        }
      
      if (classOf[Struct].isAssignableFrom(manifest.erasure)) {
        val instance = manifest.erasure.newInstance().asInstanceOf[Struct]
        unsizedArrayKeys ++= instance.listDeclarations.map(_.nameKey)
      }
    }
    
    //XXX special handling for bound arrays or structs containing arrays
  }
  
  private[this] def enforceMathTypes(declarations: Iterable[Declaration]) {
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
        "Declaration '" + declaration.name + "' has an incompatible type (only MathTypes are allowed in this block)."
      )
    }
  }
  
  private[this] def enforceSizedArrays(declarations: Iterable[Declaration]) {
    for (declaration <- declarations) {
      if (classOf[BindingList[_]].isAssignableFrom(declaration.manifest.erasure)) {
        if (declaration.arraySizeExpression.isEmpty) throw new RuntimeException(
          "Array '" + declaration.name + "' must be sized (all arrays must be sized in this block)."
        )
      }
    }
  }
  
  /** "Only a vertex shader can declare an attributes block."
   */
  protected final def attributes(block: => Unit) {
    if (!atTopLevel) throw new IllegalStateException("in{} block must be declared at the top level.")
    if (shaderType != Shader.Vertex) throw new UnsupportedOperationException(
      "Only a vertex shader can declare attributes{} block."
    )
    if (attributeBlock != null) throw new IllegalStateException("Only one attributes{} block can be declared.")
    
    attributeBlock = processBlock(() => block)
    enforceMathTypes(attributeBlock)
  }
  
  /** Vertex shader cannot have any input blocks (use attributes block instead).
   */
  protected final def in(name: String)(block: => Unit) {
    if (!atTopLevel) throw new IllegalStateException("in {} must be declared at the top level.")
    if (shaderType == Shader.Vertex) throw new UnsupportedOperationException(
      "Vertex shader cannot have any in{} blocks (use attributes{} block instead)."
    )
    
    val declarations = processBlock(() => block)
    inputBlocks += new DeclarationBlock(name, remapDeclarations(declarations).toSet)
    enforceMathTypes(declarations)
    enforceSizedArrays(declarations)
  }
  
  /** If a shader defines one or more output blocks it must define an entry point. Fragment shaders are allowed
   * to have an entry point without an output block.
   */
  protected final def out(name: String)(block: => Unit) {
    if (!atTopLevel) throw new IllegalStateException("out{} block must be declared at the top level.")

    val declarations = processBlock(() => block)
    outputBlocks += new DeclarationBlock(name, remapDeclarations(declarations).toSet)
    enforceMathTypes(declarations)
    enforceSizedArrays(declarations)
  }
  
  /** A shader must either defined an entry point or export a function (but not both).
   */
  protected final def entryPoint(code: String) {
    if (!atTopLevel) throw new IllegalStateException("entryPoint() must be declared at the top level.")
    if (entryPointSignature.isDefined) throw new IllegalStateException("Entry point is already defined.")
    if (exportSignature.isDefined) throw new IllegalStateException(
      "A shader must either defined an entry point or export a function (but not both)."
    )
    entryPointSignature = Some(code)
  }
  
  /** A shader must either defined an entry point or export a function (but not both).
   */
  protected final def export(functionSignature: String) {
    if (!atTopLevel) throw new IllegalStateException("export() must be declared at the top level.")
    if (exportSignature.isDefined) throw new IllegalStateException("Export is already defined.")
    if (entryPointSignature.isDefined) throw new IllegalStateException(
      "A shader must either defined an entry point or export a function (but not both)."
    )
    exportSignature = Some(functionSignature)
  }
  
  protected final def condition[T](path: String)(f: T => Boolean) {
    if (!atTopLevel) throw new IllegalStateException("condition() must be declared at the top level.")
    conditions += ((path, f.asInstanceOf[AnyRef => Boolean]))
  }
  
  protected final def src(src: String) {
    if (!atTopLevel) throw new IllegalStateException("src() must be declared at the top level.")
    sources += src
  }
  
  
  def toPrototype() = new ShaderPrototype(
    name,
    logAccepted,
    logRejected,
    shaderType,
    "120",//XXX version must come from profile
    exportSignature,
    entryPointSignature,
    new ReadArray(conditions.toArray),
    remapDeclarations(uniformBlock),
    boundUniforms,
    new ReadArray(unsizedArrayKeys.toArray),
    remapDeclarations(attributeBlock),
    new ReadArray(inputBlocks.toArray),
    new ReadArray(outputBlocks.toArray),
    new ReadArray(functionDependencies.toArray),
    new ReadArray(sources.toArray)
  )
}


abstract class VertexShader extends ShaderDeclaration(Shader.Vertex)
abstract class FragmentShader extends ShaderDeclaration(Shader.Fragment)
