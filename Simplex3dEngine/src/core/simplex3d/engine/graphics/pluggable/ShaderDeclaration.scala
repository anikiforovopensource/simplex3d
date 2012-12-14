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
 * a size of the corresponding BindingSeq.
 * 
 * All dependent variables should be defined inside attribute{} or in{} blocks using declare().
 * Only VertexShaders are allowed to have attribute{} blocks. Other shaders must use named in{} block and
 * must prefix the variables with the block name in the source code.
 *
 * Each shader can link with the next stage using an out{} block. In this case shader must also declare main().
 * Multiple main declarations are allowed within the same program.
 * Alternatively, a shader with main() declaration can share a named set of values using main(){ out{} }.
 * Only one out{} block is allowed per shader, either stage interface out{} or main{ out{} }. Fragment
 * shaders cannot define interface out{}.
 * Shared pre-computed values within the same stage can be requested within main(){ in{} } blocks.
 * 
 * Arrays in in{} and out{} blocks must be sized either to a literal value or to a size of some uniform array.
 * If a uniform array is defined in the shader scope, its size can be accessed using injected variable
 * of the form: se_sizeOf_${StructType}_${ArrayName} or simply se_sizeOf_${ArrayName} when array is
 * declared at the top level.
 * 
 * If a shader does not provide a main(), then it must define a function(){}. Functions can be used within
 * the same stage. Functions can have multiple in{} blocks, but cannot have any out{} blocks.
 * 
 * Additional shader sources can be attached using src() declarations. They will be inserted into the shader
 * code before main() or function() in the order they appear.
 * 
 * A path that resolves to enumeration and a condition on the result can be specified to control shader
 * selection using enums.
 * 
 * Samplers are extracted from structs by the texture manager and then injected at the top level in the shader.
 * This is done to allow property-to-UBO mapping in the future.
 * As a consequence, texture names have to be unique not just inside the parent Struct,
 * but among all the Structs, and all the top-level declarations in Material and Environment.
 * 
 * Texture dimensions can be accessed by declaring integer verctor "se_dimsOf_textureName", for example:
 * declare[Vec2i]("se_dimsOf_myTexture")
 */
sealed abstract class ShaderDeclaration(val shaderType: Shader.type#Value) {
  
  protected final class Declaration private[ShaderDeclaration] (
    val manifest: ClassManifest[_ <: Binding], val name: String
  )
  {
    private[ShaderDeclaration] var qualifiers: Option[String] = None
    private[ShaderDeclaration] var arraySizeExpression: Option[String] = None
    private[ShaderDeclaration] def isArray = classOf[BindingSeq[_]].isAssignableFrom(manifest.erasure)
    private[ShaderDeclaration] def isMathType = classOf[MathType].isAssignableFrom(manifest.erasure)
    private[ShaderDeclaration] def isMathTypeArray = {
      isArray && {
        try {
          classOf[MathType].isAssignableFrom(
            manifest.typeArguments.head.asInstanceOf[ClassManifest[_]].erasure
          )
        } catch {
          case e: Exception => false
        }
      }
    }
    
    /** The simple way to link input/output array sizes is using a size of a uniform array: a.length(); 
     */
    def size(arraySizeExpression: String) :this.type = {
      if (!isArray) throw new RuntimeException("Only arrays can be sized.")
      if (this.arraySizeExpression.isDefined) throw new RuntimeException("This array is already sized.")
      
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
    :(String, ReadArray[StructSignature], ReadArray[NestedSampler]) = // (glslType, structureSignatuers, nestedSamplers)
    {
      val dependencies = ArrayBuilder.make[StructSignature]
      val nestedSamplers = ArrayBuilder.make[NestedSampler]
      
      val glslType = glslTypeFromManifest(
          squareMatrices, 0, arraySizeExpression, "", null, manifest, name, dependencies, nestedSamplers)
      
      val dependenciesRes = StructSignature.organizeDependencies(dependencies.result)
      val nestedSamplersRes = new ReadArray(nestedSamplers.result)
      
      (glslType, dependenciesRes, nestedSamplersRes)
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
      else if (unprefixedName.startsWith("vec")) {
        if (unprefixedName.endsWith("i")) "i" + unprefixedName.dropRight(1)
        else unprefixedName.dropRight(1)
      }
      else if (unprefixedName.startsWith("mat") && squareMatrices) {
        unprefixedName.dropRight(1) match {
          case "mat2" => "mat2"
          case "mat2x3" | "mat3x2" | "mat3" => "mat3"
          case _ => "mat4"
        }
      }
      else {
        unprefixedName match {
          case _ => "???"
        }
      }
    }
    private def resolveTextureType(
      parentErasure: Class[_],
      erasure: Class[_], name: String, arraySizeExpression: Option[String],
      nestedSamplers: ArrayBuilder[NestedSampler]
    ) :String = {
      
      val glslType = ClassUtil.simpleName(erasure) match {
        case "Texture2d" => "sampler2D"
        // TODO more texture types
      }
      
      if (parentErasure != null) nestedSamplers += new NestedSampler(parentErasure, glslType, name, arraySizeExpression)
      glslType
    }
    
    
    private def glslTypeFromManifest(
      squareMatrices: Boolean,
      level: Int,
      firstSizeExpression: Option[String],// the very first size expression in the path, used with nestedSamplers
      parentType: String,
      parentErasure: Class[_],
      manifest: ClassManifest[_], name: String,
      dependencies: ArrayBuilder[StructSignature],
      nestedSamplers: ArrayBuilder[NestedSampler]
    ) :String = { // glslType
      
      if (classOf[MathType].isAssignableFrom(manifest.erasure)) {
        resolveMathType(squareMatrices, manifest.erasure)
      }
      else if (classOf[TextureBinding[_]].isAssignableFrom(manifest.erasure)) {
        try {
          val erasure = manifest.typeArguments.head.asInstanceOf[ClassManifest[_]].erasure
          resolveTextureType(null, erasure, name, firstSizeExpression, nestedSamplers) 
        }
        catch {
          case e: Exception => throw new RuntimeException(
            "Undefined or unsupported texture binding type for declaration '" + name + "'."
          )
        }
      }
      else if (classOf[BindingSeq[_]].isAssignableFrom(manifest.erasure)) {
        val listManifest = try {
          manifest.typeArguments.head.asInstanceOf[ClassManifest[_]]
        }
        catch {
          case e: Exception => throw new RuntimeException(
            "Undefined or unsupported array type for declaration '" + name + "'."
          )
        }
        
        val firstSize =
          if (firstSizeExpression.isDefined) firstSizeExpression
          else Some(ShaderPrototype.arraySizeId(new ListNameKey(parentType, name)))
        
        val glslType = glslTypeFromManifest(
            squareMatrices, level, firstSize, parentType, parentErasure, listManifest, name, dependencies, nestedSamplers)
            
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
        
        glslTypeFromStruct(squareMatrices, level, firstSizeExpression, instance, dependencies, nestedSamplers)
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
      firstSizeExpression: Option[String],
      instance: Struct,
      dependencies: ArrayBuilder[StructSignature],
      nestedSamplers: ArrayBuilder[NestedSampler]
    ) :String =
    {
      val glslType = ClassUtil.simpleName(instance.getClass)
      if (dependencies == null) return glslType
  
      val entries = ArrayBuilder.make[(String, String)]
      
      for (i <- 0 until instance.fieldNames.length) {
        val fieldName = instance.fieldNames(i)
        val field = instance.fields(i)
        
        val (fieldType, arraySizeExpression, isSampler) = glslTypeFromInstance(
          squareMatrices, level + 1, firstSizeExpression, glslType, instance.getClass,
          field, fieldName, dependencies, nestedSamplers
        )
        
        if (!isSampler) {
          val append = if (arraySizeExpression != "") "[" + arraySizeExpression + "]" else ""
          entries += ((fieldType, fieldName + append))
        }
      }
      
      //XXX do this only with nvidia drivers.
      if (firstSizeExpression.isDefined) entries += (("float", "nvidia_drivers_are_very_buggy"))
      
      val readEntries = new ReadArray(entries.result)
      dependencies += new StructSignature(level, instance.getClass, glslType, readEntries)
      
      glslType
    }
    
    private def glslTypeFromInstance(
      squareMatrices: Boolean,
      level: Int,
      firstSizeExpression: Option[String],
      parentType: String,
      parentErasure: Class[_],
      instance: Object, name: String,
      dependencies: ArrayBuilder[StructSignature],
      nestedSamplers: ArrayBuilder[NestedSampler]
    ) :(String, String, Boolean) = {//(glslType, sizeExpression, isSampler)
      
      if (classOf[MathType].isAssignableFrom(instance.getClass)) {
        (resolveMathType(squareMatrices, instance.getClass), "", false)
      }
      else if (classOf[TextureBinding[_]].isAssignableFrom(instance.getClass)) {
        val erasure = instance.asInstanceOf[TextureBinding[_]].bindingManifest.erasure
        val glslType = resolveTextureType(parentErasure, erasure, name, firstSizeExpression, nestedSamplers)
        (glslType, "", true)
      }
      else if (classOf[BindingSeq[_]].isAssignableFrom(instance.getClass)) {
        val manifest = instance.asInstanceOf[BindingSeq[_]].elementManifest
        val firstSize =
          if (firstSizeExpression.isDefined) firstSizeExpression
          else Some(ShaderPrototype.arraySizeId(new ListNameKey(parentType, name)))
        
        val glslType = glslTypeFromManifest(
            squareMatrices, level, firstSize, parentType, parentErasure, manifest, name, dependencies, nestedSamplers)
        
        val sizeExpression =
          if (arraySizeExpression.isDefined) arraySizeExpression.get
          else ShaderPrototype.arraySizeId(new ListNameKey(parentType, name))
        
        (glslType, sizeExpression, false)
      }
      else if (classOf[Struct].isAssignableFrom(instance.getClass)) {
        val glslType = glslTypeFromStruct(
            squareMatrices, level, firstSizeExpression, instance.asInstanceOf[Struct], dependencies, nestedSamplers)
            
        (glslType, "", false)
      }
      else {
        throw new RuntimeException(
          "Unsupported type '" + ClassUtil.simpleName(instance.getClass) + "' for declaration '" + name + "'."
        )
      }
    }
  }
  
  
  private[this] var functionSignature: Option[String] = None
  
  private[this] var mainLabel: Option[String] = None
  private[this] var mainInputs = Set[(String, Set[Declaration])]()//(blockName, declarations)
  private[this] var mainOutput: Option[(String, Set[Declaration])] = None
  
  private[this] var conditions = Set[(String, AnyRef => Boolean)]()
  
  private[this] var uniformBlock: Option[Set[Declaration]] = None
  private[this] var boundUniforms = Map[String, Value[UncheckedBinding]]()
  private[this] var unsizedArrayKeys = Set[ListNameKey]()
  private[this] var sizedArrayKeys = Set[ListSizeKey]()
  
  private[this] var attributeBlock: Option[Set[Declaration]] = None
  
  private[this] var inputBlocks = Set[(String, Set[Declaration])]()//(blockName, declarations)
  private[this] var outputBlock: Option[(String, Set[Declaration])] = None
  
  private[this] var functionDependencies = Set[String]()
  
  private[this] var body: String = null
  private[this] var sources: List[String] = Nil
  
  
  private[this] var declarations: Set[Declaration] = null
  private[this] def processBlock(blockBody: () => Unit) :Set[Declaration] = {
    declarations = Set[Declaration]()
    blockBody()
    val res = declarations; declarations = null
    res
  }
  
  private[this] var processingUniforms = false
  private[this] var processingMainBody = false
  
  private[this] def atBlockLevel = (declarations == null)
  private[this] def atUniformBlock = processingUniforms
  private[this] def atMainBody = processingMainBody
  
  
  private type PublicDeclaration = simplex3d.engine.graphics.pluggable.Declaration
  
  private[this] def buildDeclarations(squarMatrices: Boolean, block: Option[Set[Declaration]]) :Set[PublicDeclaration] = {
    if (block.isDefined) buildDeclarations(squarMatrices, block.get)
    else Set.empty[PublicDeclaration]
  }
  private[this] def buildDeclarations(squarMatrices: Boolean, declarations: Set[Declaration]) :Set[PublicDeclaration] = {
    declarations.map { d =>
      val (glslType, sturctSignatures, nestedSamplers) = d.extractGlslTypes(squarMatrices)

      new PublicDeclaration(
          d.qualifiers,
          d.manifest,
          glslType, d.name, d.arraySizeExpression,
          sturctSignatures, nestedSamplers)
    }
  }
  
  
  protected val debugging = new ShaderDebugging
  
  protected final def bind[T <: Accessible with Binding](name: String, binding: Value[T]) {
    if (!atUniformBlock) throw new IllegalStateException("bind() must be declared in a uniform{} block.")
    
    boundUniforms += ((name, binding.asInstanceOf[Value[UncheckedBinding]]))
    binding.register(ShaderPropertyContext)
    
    declarations += new Declaration(ClassUtil.rebuildManifest(binding.get), name)
    
    binding.get match {
      case seq: BindingSeq[_] => sizedArrayKeys += new ListSizeKey(new ListNameKey("", name), seq.size)
      case struct: Struct => sizedArrayKeys ++= struct.getSizeKeys()
      case _ => // ignore
    }
  }
  
  protected final def use(functionSignature: String) {
    if (!atBlockLevel) throw new IllegalStateException("use() must be declared at the top level.")
    functionDependencies += functionSignature
  }
  
  protected final def declare[B <: Binding : ClassManifest](name: String) :Declaration = {
    if (atBlockLevel) throw new IllegalStateException("declare() must be called inside a block.")
    val declaration = new Declaration(implicitly[ClassManifest[B]], name)
    declarations += declaration
    declaration
  }
  
  /** All uniforms must be declared in the same block.
   * Engine structs may be translated to GLSl uniform blocks depending on the implementation.
   */
  protected final def uniform(block: => Unit) {
    if (!atBlockLevel) throw new IllegalStateException("uniform{} must be declared at the top level.")
    if (atMainBody) throw new IllegalStateException("uniform{} must be declared at the top level.")
    if (uniformBlock.isDefined) throw new IllegalStateException("Only one uniform{} block can be declared.")
    
    processingUniforms = true
    uniformBlock = Some(processBlock(() => block))
    processingUniforms = false
    
    // Process array declarations.
    for (declaration <- uniformBlock.get) {
      val isArray = declaration.isArray
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
        unsizedArrayKeys ++= instance.getUnsizedListKeys()
      }
    }
  }
  
  private[this] def enforceMathTypes(declarations: Iterable[Declaration], allowArrays: Boolean = true) {
    for (declaration <- declarations) {
      if (!declaration.isMathType && !(allowArrays && declaration.isMathTypeArray)) throw new RuntimeException(
        "Declaration '" + declaration.name + "' has an incompatible type (only MathTypes are allowed in this block)."
      )
    }
  }
  
  private[this] def enforceSizedArrays(declarations: Iterable[Declaration]) {
    for (declaration <- declarations) {
      if (declaration.isArray) {
        if (declaration.arraySizeExpression.isEmpty) throw new RuntimeException(
          "Array '" + declaration.name + "' must be sized (all arrays must be sized in this block)."
        )
      }
    }
  }
  
  private[this] def enforceUniqueBlockNames(name: String) {
    val equalsName = (tuple: (String, Any)) => tuple._1 == name
    if (mainInputs.exists(equalsName) || mainOutput.exists(equalsName) ||
        inputBlocks.exists(equalsName) || outputBlock.exists(equalsName))
    {
      throw new RuntimeException("Block with name '" + name + "' is already defined.")
    }
  }
  
  /** "Only a vertex shader can declare an attributes block."
   */
  protected final def attributes(block: => Unit) {
    if (!atBlockLevel) throw new IllegalStateException("attributes{} block must be declared at the top level.")
    if (atMainBody) throw new IllegalStateException("attributes{} block must be declared at the top level.")
    if (shaderType != Shader.Vertex) throw new UnsupportedOperationException(
      "Only a vertex shader can declare attributes{} block."
    )
    if (attributeBlock.isDefined) throw new IllegalStateException("Only one attributes{} block can be declared.")
    
    attributeBlock = Some(processBlock(() => block))
    enforceMathTypes(attributeBlock.get, allowArrays = false)
  }
  
  /** Vertex shader cannot have any input blocks (use attributes block instead).
   */
  protected final def in(name: String)(block: => Unit) {
    if (!atBlockLevel) throw new IllegalStateException("in{} must be declared at the top level.")
    if (!atMainBody && shaderType == Shader.Vertex) throw new UnsupportedOperationException(
      "in{} blocks cannot be declared in the vertex shader, use attributes{} block instead."
    )
    
    enforceUniqueBlockNames(name)
    
    
    val declarations = processBlock(() => block)
    
    enforceSizedArrays(declarations)
    
    if (atMainBody) mainInputs += ((name, declarations))
    else inputBlocks += ((name, declarations))
  }
  
  /** If a shader defines one or more output blocks it must define main(){}. Fragment shaders are allowed
   * to have main(){} without an output block.
   */
  protected final def out(name: String)(block: => Unit) {
    if (!atBlockLevel) throw new IllegalStateException("out{} block cannot be declared here.")
    
    if (atMainBody) {
      if (mainOutput.isDefined) throw new IllegalStateException("out{} is already defined.")
      if (outputBlock.isDefined) throw new IllegalStateException("out{} is already defined as shader interface.")
    }
    else {
      if (shaderType == Shader.Fragment) throw new UnsupportedOperationException(
        "out{} blocks cannot be declared in the fragment shader."
      )
      if (outputBlock.isDefined) throw new IllegalStateException("out{} is already defined.")
      if (mainOutput.isDefined) throw new IllegalStateException("out{} is already defined as main argument.")
    }
    
    enforceUniqueBlockNames(name)

    
    val declarations = processBlock(() => block)
    
    enforceSizedArrays(declarations)
    
    if (atMainBody) mainOutput = Some(name, declarations)
    else outputBlock = Some(name, declarations)
  }
  
  /** A shader must either defined main(){} or function(){} (but not both).
   */
  protected final def main(name: String)(block: => Unit)(body: String) {
    if (!atBlockLevel) throw new IllegalStateException("main(){} must be declared at the top level.")
    if (atMainBody) throw new IllegalStateException("main(){} cannot be nested.")
    if (mainLabel.isDefined) throw new IllegalStateException("main(){} is already defined.")
    if (functionSignature.isDefined) throw new IllegalStateException(
      "A shader must either defined main(){} or function(){} (but not both)."
    )
    
    processingMainBody = true
    block
    processingMainBody = false
    
    mainLabel = Some(name)
    this.body = body
  }
  
  /** A shader must either defined main(){} or function(){} (but not both)
   */
  protected final def function(signature: String)(body: String) {
    if (!atBlockLevel) throw new IllegalStateException("function(){} must be declared at the top level.")
    if (functionSignature.isDefined) throw new IllegalStateException("function(){} is already defined.")
    if (mainLabel.isDefined) throw new IllegalStateException(
      "A shader must either defined main(){} or function(){} (but not both)"
    )
    functionSignature = Some(signature)
    this.body = body
  }
  
  protected final def condition(path: String)(f: AnyRef => Boolean) {
    if (!atBlockLevel) throw new IllegalStateException("condition() must be declared at the top level.")
    conditions += ((path, f.asInstanceOf[AnyRef => Boolean]))
  }
  
  protected final def src(src: String) {
    if (!atBlockLevel) throw new IllegalStateException("src{} must be declared at the top level.")
    
    sources ::= src
  }
  
  
  def toPrototype(profile: Profile.type#Value) = {
    val squareMat = (profile == Profile.Gl2)
    
    if (outputBlock.isDefined && !mainLabel.isDefined) throw new IllegalStateException(
        "Only shaders with main(){} can define out{} blocks.")
    
    
    new ShaderPrototype(
      debugging,
      shaderType,
      profile,
      squareMat,
      functionSignature,
      mainLabel,
      new ReadArray(mainInputs.map(b => new DeclarationBlock(b._1, buildDeclarations(squareMat, b._2))).toArray),
      mainOutput.map(b => new DeclarationBlock(b._1, buildDeclarations(squareMat, b._2))),
      body,
      new ReadArray(conditions.toArray),
      new ReadArray(buildDeclarations(squareMat, uniformBlock).toArray),
      boundUniforms,
      new ReadArray(unsizedArrayKeys.toArray),
      new ReadArray(sizedArrayKeys.toArray),
      new ReadArray(buildDeclarations(squareMat, attributeBlock).toArray),
      new ReadArray(inputBlocks.map(b => new DeclarationBlock(b._1, buildDeclarations(squareMat, b._2))).toArray),
      outputBlock.map(b => new DeclarationBlock(b._1, buildDeclarations(squareMat, b._2))),
      new ReadArray(functionDependencies.toArray),
      sources.reverse
    )
  }
}


abstract class VertexShader extends ShaderDeclaration(Shader.Vertex)
abstract class FragmentShader extends ShaderDeclaration(Shader.Fragment)
