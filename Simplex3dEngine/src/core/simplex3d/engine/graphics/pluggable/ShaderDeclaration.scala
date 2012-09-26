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
import scala.collection.mutable.ArrayBuilder
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
  
  protected final class Logging(var name: String, var logAccepted: Boolean, var logRejected: Boolean)
  
  protected final class Declaration private[ShaderDeclaration] (
    val manifest: ClassManifest[_ <: Binding], val name: String
  )
  {
    private[ShaderDeclaration] var qualifiers: Option[String] = None
    private[ShaderDeclaration] var arraySizeExpression: Option[String] = None
    
    /** The simple way to link input/output array sizes is using a size of a uniform array: a.length(); 
     */
    def size(arraySizeExpression: String) :this.type = {
      if (!classOf[BindingList[_]].isAssignableFrom(manifest.erasure)) throw new RuntimeException(
        "Only arrays can be sized."
      )
      if (this.arraySizeExpression.isDefined) throw new RuntimeException(
        "This array is already sized."
      )
      
      val expr = arraySizeExpression.trim
      if (expr.isEmpty) throw new IllegalArgumentException("Array size expression must not be empty.")
      
      this.arraySizeExpression = Some(expr)
      this
    }
    
    def qualify(qualifiers: String) :this.type = {
      if (qualifiers.trim.isEmpty) new IllegalArgumentException("Qualifier must not be empty.")
      if (this.qualifiers.isDefined) throw new RuntimeException(
        "Qualifiers are already defined."
      )
      
      this.qualifiers = Some(qualifiers.trim)
      this
    }
    
    private[ShaderDeclaration] def extractGlslTypes(squareMatrices: Boolean)
    :(String, ReadArray[StructSignature]) = // (glslType, structureSignatuers)
    {
      val builder = ArrayBuilder.make[StructSignature]
      val glslType = glslTypeFromManifest(squareMatrices, 0, "", manifest, name, builder)
      val oragnized = StructSignature.organizeDependencies(builder.result)
      
      (glslType, oragnized)
    }
    
    private def resolveMathType(squareMatrices: Boolean, erasure: Class[_]) :String = {
      val className = ClassUtil.simpleName(erasure).toLowerCase()
      val unprefixedName = 
        if (className.startsWith("const")) className.drop(5)
        else if (className.startsWith("read")) className.drop(4)
        else className
      
      if (unprefixedName.endsWith("ref")) {
        val shorterName = unprefixedName.dropRight(3)
        if (shorterName == "double") "float" else shorterName
      }
      else if (unprefixedName.startsWith("vec") && unprefixedName.endsWith("i")) {
        "i" + unprefixedName.dropRight(1)
      }
      else if (unprefixedName.startsWith("mat") && squareMatrices) {
        unprefixedName.dropRight(1) match {
          case "mat2" => "mat2"
          case "mat2x3" | "mat3x2" | "mat3" => "mat3"
          case _ => "mat4"
        }
      }
      else {
        unprefixedName.dropRight(1)
      }
    }
    private def resolveTextureType(
      erasure: Class[_],
      level: Int, dependenciesResult: ArrayBuilder[StructSignature]
    ) :String =
    {
      val className = ClassUtil.simpleName(erasure)
      
      className match {
        
        case "Texture2d" => {
          val glslType = "Se_Texture2d"
            
          dependenciesResult += new StructSignature(
            level, classOf[Texture2d[_]], glslType,
            new ReadArray(Array(
              ("sampler2D", "sampler"),
              ("ivec2", "dimensions"))),
            true
          )
          
          glslType
        }
        
        case _ => throw new RuntimeException
      }
    }
    
    
    private def glslTypeFromManifest(
      squareMatrices: Boolean,
      level: Int,
      parentType: String,
      manifest: ClassManifest[_], name: String,
      dependenciesResult: ArrayBuilder[StructSignature]
    ) :String = { // glslType
      
      if (classOf[MathType].isAssignableFrom(manifest.erasure)) {
        resolveMathType(squareMatrices, manifest.erasure)
      }
      else if (classOf[TextureBinding[_]].isAssignableFrom(manifest.erasure)) {
        try {
          val erasure = manifest.typeArguments.head.asInstanceOf[ClassManifest[_]].erasure
          resolveTextureType(erasure, level, dependenciesResult) 
        }
        catch {
          case e: Exception => throw new RuntimeException(
            "Undefined or unsupported texture binding type for declaration '" + name + "'."
          )
        }
      }
      else if (classOf[BindingList[_]].isAssignableFrom(manifest.erasure)) {
        val listManifest = try {
          manifest.typeArguments.head.asInstanceOf[ClassManifest[_]]
        }
        catch {
          case e: Exception => throw new RuntimeException(
            "Undefined or unsupported array type for declaration '" + name + "'."
          )
        }
        
        val glslType = glslTypeFromManifest(squareMatrices, level, parentType, listManifest, name, dependenciesResult)
        glslType
      }
      else if (classOf[Struct].isAssignableFrom(manifest.erasure)) {
        val instance = try {
          manifest.erasure.newInstance().asInstanceOf[Struct]
        }
        catch {
          case e: Exception => throw new RuntimeException(
            "Unable to create an instance of '" + manifest.erasure.getName +
            "'. All Struct subclasses must define a no-argument constructor."
          )
        }
        
        glslTypeFromStruct(squareMatrices, level, instance, dependenciesResult)
      }
      else {
        throw new RuntimeException(
          "Unsupported type '" + ClassUtil.simpleName(manifest.erasure) + "' for declaration '" + name + "'."
        )
      }
    }
    
    private def glslTypeFromStruct(
      squareMatrices: Boolean,
      level: Int,
      instance: Struct,
      dependenciesResult: ArrayBuilder[StructSignature]
    ) :String =
    {
      val glslType = ClassUtil.simpleName(instance.getClass)
      if (dependenciesResult == null) return glslType
  
      val subStructs = ArrayBuilder.make[StructSignature]
      val entries = ArrayBuilder.make[(String, String)]
      var containsSamplers = false
      
      for (i <- 0 until instance.fieldNames.length) {
        val fieldName = instance.fieldNames(i)
        val field = instance.fields(i)
        
        val (fieldType, arraySizeExpression) = glslTypeFromInstance(
          squareMatrices, level + 1, glslType, field, fieldName, subStructs
        )
        
        val append = if (arraySizeExpression != "") "[" + arraySizeExpression + "]" else ""
        entries += ((fieldType, fieldName + append))
        
        if (classOf[TextureBinding[_]].isAssignableFrom(field.getClass)) containsSamplers = true
      }
      
      val subStructsArray = subStructs.result()
      if (subStructsArray.find(_.containsSamplers).isDefined) containsSamplers = true
      dependenciesResult ++= subStructsArray
      
      val readEntries = new ReadArray(entries.result)
      dependenciesResult += new StructSignature(level, instance.getClass, glslType, readEntries, containsSamplers)
      
      glslType
    }
    
    private def glslTypeFromInstance(
      squareMatrices: Boolean,
      level: Int,
      parentType: String,
      instance: Object, name: String,
      dependenciesResult: ArrayBuilder[StructSignature]
    ) :(String, String) = {//(glslType, sizeExpression)
      
      if (classOf[MathType].isAssignableFrom(instance.getClass)) {
        (resolveMathType(squareMatrices, instance.getClass), "")
      }
      else if (classOf[TextureBinding[_]].isAssignableFrom(instance.getClass)) {
        val erasure = instance.asInstanceOf[TextureBinding[_]].bindingManifest.erasure
        (resolveTextureType(erasure, level, dependenciesResult), "")
      }
      else if (classOf[BindingList[_]].isAssignableFrom(instance.getClass)) {
        val manifest = instance.asInstanceOf[BindingList[_]].elementManifest
        val glslType = glslTypeFromManifest(squareMatrices, level, parentType, manifest, name, dependenciesResult)
        
        val sizeExpression =
          if (arraySizeExpression.isDefined) arraySizeExpression.get
          else ShaderPrototype.arraySizeId(new ListNameKey(parentType, name))
        
        (glslType, sizeExpression)
      }
      else if (classOf[Struct].isAssignableFrom(instance.getClass)) {
        (glslTypeFromStruct(squareMatrices, level, instance.asInstanceOf[Struct], dependenciesResult), "")
      }
      else {
        throw new RuntimeException(
          "Unsupported type '" + ClassUtil.simpleName(instance.getClass) + "' for declaration '" + name + "'."
        )
      }
    }
  }
  
  
  private[this] var exportSignature: Option[String] = None
  private[this] var entryPointSignature: Option[String] = None
  private[this] var conditions = Set[(String, AnyRef => Boolean)]()
  
  private[this] var uniformBlock: Set[Declaration] = null
  private[this] var boundUniforms = Map[String, Property[UncheckedBinding]]()
  private[this] var unsizedArrayKeys = Set[ListNameKey]()
  
  private[this] var attributeBlock: Set[Declaration] = null
  
  private[this] var inputBlocks = Set[(String, Set[Declaration])]()//(blockName, declarations)
  private[this] var outputBlocks = Set[(String, Set[Declaration])]()//(blockName, declarations)
  
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
  
  private[this] def remapDeclarations(squarMatrices: Boolean, declarations: Set[Declaration]) = {
    type Dec = simplex3d.engine.graphics.pluggable.Declaration
    
    if (declarations == null) Set.empty[Dec]
    else {
      declarations.map { d =>
        val (glslType, sturctSignatures) = d.extractGlslTypes(squarMatrices)
        new Dec(d.qualifiers, d.manifest, glslType, d.name, d.arraySizeExpression, sturctSignatures)
      }
    }
  }
  
  
  protected val logging = new Logging(this.hashCode.toString, false, false)
  
  protected final def bind[T <: Accessible with Binding](name: String, binding: Property[T]) {
    if (!atUniformBlock) throw new IllegalStateException("bind() must be declared in a uniform{} block.")
    boundUniforms += ((name, binding.asInstanceOf[Property[UncheckedBinding]]))
    declarations += new Declaration(ClassUtil.rebuildManifest(binding.get), name)
    
    //XXX register ShaderPropertyContext with property
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
        unsizedArrayKeys += new ListNameKey("", declaration.name)
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
    inputBlocks += ((name, declarations))
    enforceMathTypes(declarations)
    enforceSizedArrays(declarations)
  }
  
  /** If a shader defines one or more output blocks it must define an entry point. Fragment shaders are allowed
   * to have an entry point without an output block.
   */
  protected final def out(name: String)(block: => Unit) {
    if (!atTopLevel) throw new IllegalStateException("out{} block must be declared at the top level.")

    val declarations = processBlock(() => block)
    outputBlocks += ((name, declarations))
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
  
  
  def toPrototype(version: String) = {
    val squareMat = (version == "120")//XXX this should come from profile enum rather than version string
    
    new ShaderPrototype(
      new ShaderLogging(logging.name, logging.logAccepted, logging.logRejected),
      shaderType,
      version,
      squareMat,
      exportSignature,
      entryPointSignature,
      new ReadArray(conditions.toArray),
      new ReadArray(remapDeclarations(squareMat, uniformBlock).toArray),
      boundUniforms,
      new ReadArray(unsizedArrayKeys.toArray),
      new ReadArray(remapDeclarations(squareMat, attributeBlock).toArray),
      new ReadArray(inputBlocks.map(b => new DeclarationBlock(b._1, remapDeclarations(squareMat, b._2))).toArray),
      new ReadArray(outputBlocks.map(b => new DeclarationBlock(b._1, remapDeclarations(squareMat, b._2))).toArray),
      new ReadArray(functionDependencies.toArray),
      new ReadArray(sources.toArray)
    )
  }
}


abstract class VertexShader extends ShaderDeclaration(Shader.Vertex)
abstract class FragmentShader extends ShaderDeclaration(Shader.Fragment)
