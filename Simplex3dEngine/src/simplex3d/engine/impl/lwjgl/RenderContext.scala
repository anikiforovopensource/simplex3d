/*
 * Simplex3dEngine - LWJGL Module
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

package simplex3d
package engine
package impl.lwjgl

import java.util.logging._
import java.nio._
import java.util.HashMap
import scala.annotation._
import scala.collection.mutable.ArrayBuffer
import org.lwjgl.opengl._
import simplex3d.math.types._
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.engine.scene._
import simplex3d.engine.graphics._
import simplex3d.engine.impl.gl._


private[lwjgl] class ActiveAttribute(var id: Int)
private[lwjgl] class ActiveTexture(var unit: Int, var id: Int)


private[lwjgl] object RenderContext {
  private final val logger = Logger.getLogger(classOf[RenderContext].getName)
}


private[lwjgl] final class RenderContext(val capabilities: GraphicsCapabilities, val settings: AdvancedSettings)
extends engine.RenderContext with ImplAccess {
  import GL11._; import GL12._; import GL13._; import GL14._; import GL15._; import org.lwjgl.util.glu.MipMap._
  import GL20._; import GL21._; import EXTTextureFilterAnisotropic._; import EXTFramebufferObject._
  import RenderContext.logger._


  private val defaultTexture2d = Texture2d(Vec2i(4), DataBuffer[Vec3, UByte](4*4));
  {
    val dims = defaultTexture2d.dimensions
    val data = defaultTexture2d.write
    
    for (y <- 0 until dims.y; x <- 0 until dims.x) {
      data(x + y*dims.x) = Vec3(1, 0, 1)
    }
  }
  
  
  private val resourceManager = new GlResourceManager(
    attributeManager = new IdManager(100)(glGenBuffers(_), glDeleteBuffers(_)),
    textureManager = new IdManager(20)(glGenTextures(_), glDeleteTextures(_)),
    glDeleteShader(_),
    glDeleteProgram(_)
  )
  
  
  // ******************************************************************************************************************
  
  private var invalidateState = false

  private var faceCullingState = -1
  private var faceCullingValue: FaceCulling.type#Value = null
  
  private var mipmapHintEnabled = false
  
  private final var activeProgramId = 0
  private final var boundBufferId = 0
  private final var boundIndexId = 0
  
  private final var activeTextureUnit = 0
  
  private final def textureUnits = new Array[Int](32)
  private final def boundTexture = textureUnits(activeTextureUnit)
  private final def boundTexture_=(id: Int) { textureUnits(activeTextureUnit) = id }
  
  private final val activeAttributes = new HashMap[Int, ActiveAttribute]() //XXX possibly use different data structures
  private final val activeTextures = new HashMap[Int, ActiveTexture]()
  
  final def requiresReset = invalidateState
  
  final def setFaceCulling(faceCulling: ValueProperty[ReadEnumRef[FaceCulling.type]]) {
    def resolve(value: FaceCulling.type#Value) :Int = {
      value match {
        case FaceCulling.Front => GL_FRONT
        case FaceCulling.Back => GL_BACK
      }
    }
    
    if (faceCulling.isDefined) {
      if (faceCullingState != 1) {
        glEnable(GL_CULL_FACE)
        faceCullingState = 1
      }
      if (faceCullingValue != faceCulling.defined.toConst) {
        glCullFace(resolve(faceCulling.defined.toConst))
        faceCullingValue = faceCulling.defined.toConst
      }
    }
    else {
      if (faceCullingState != 0) {
        glDisable(GL_CULL_FACE)
        faceCullingState = 0
      }
    }
  }
  
  final def mipmapHint() {
    if (!mipmapHintEnabled) { glHint(GL_GENERATE_MIPMAP_HINT, GL_NICEST); mipmapHintEnabled = true }
  }
  
  final def useProgram(id: Int) {
    if (activeProgramId != id) {
      glUseProgram(id)
      activeProgramId = id
    }
  }
  
  final def bindBuffer(id: Int) {
    if (boundBufferId != id) {
      glBindBuffer(GL_ARRAY_BUFFER, id)
      boundBufferId = id
    }
  }
  
  final def bindIndex(id: Int) {
    if (boundIndexId != id) {
      glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, id)
      boundIndexId = id
    }
  }
  
  final def activeTextureUnit(textureUnit: Int) {
    if (activeTextureUnit != textureUnit) {
      glActiveTexture(GL_TEXTURE0 + textureUnit)
      activeTextureUnit = textureUnit
    }
  }
  
  final def bindTexture(glTarget: Int, id: Int) {
    if (boundTexture != id) {
      glBindTexture(glTarget, id)
      boundTexture = id
    }
  }
  
  final def resetState() {
    faceCullingState = -1
    faceCullingValue = null
  
    mipmapHintEnabled = false
    
    activeProgramId = 0
    boundBufferId = 0
    boundIndexId = 0
    
    activeTextureUnit = -1
    
    var i = 0; while (i < textureUnits.length) {
      textureUnits(i) = 0
      i += 1
    }
    
    activeAttributes.clear()
    activeTextures.clear()
    
    invalidateState = false
  }
  
  
  // ******************************************************************************************************************
  
  def init(attributes: Attributes[_, _]) {
    initUpdateAttributes(attributes)
  }
  
  private def initUpdateAttributes(attributes: Attributes[_, _]) :Int = {
    var id = attributes.managedFields.id
    if (id == 0) id = initialize(attributes)
    else if (attributes.sharedState.hasDataChanges) update(id, attributes)
    
    id
  }
  
  private def initialize(attributes: Attributes[_, _]) :Int = {
    resourceManager.allocate(attributes)
    val id = attributes.managedFields.id
      
    bindBuffer(id)
    glBufferData(
      GL_ARRAY_BUFFER,
      attributes.src.bindingBuffer(),
      attributes.sharedState.caching match {
        case Caching.Dynamic => GL_DYNAMIC_DRAW
        case Caching.Static => GL_STATIC_DRAW
        case Caching.Stream => GL_STREAM_DRAW
      }
    )
    
    attributes.sharedState.clearDataChanges()
    id
  }
  
  private def update(id: Int, attributes: Attributes[_, _]) {
    val data = attributes.asInstanceOf[Attributes[_ <: Format with MathType, Raw]].read
    
    bindBuffer(id)
    
    val regions = attributes.sharedState.updatedRegions
    var i = 0; while (i < regions.size) { val region = regions(i)
      
      glBufferSubData(
        GL_ARRAY_BUFFER,
        region.first*data.byteStride,
        data.bindingBufferSubData(region.first, region.count)
      )
      
      i += 1
    }
    
    attributes.sharedState.clearDataChanges()
  }
  
  def bind(location: Int, attributes: Attributes[_ <: Format with MathType, Raw]) {
    val id = initUpdateAttributes(attributes)
    
    
    var activeAttribute = activeAttributes.get(location)
    val disabled = (activeAttribute == null)
    if (disabled) {
      activeAttribute = new ActiveAttribute(0)
      activeAttributes.put(location, activeAttribute)
      
      glEnableVertexAttribArray(location)
    }
    
    val src = attributes.src
    bindBuffer(id)
    
    if (activeAttribute.id != id) {
      glVertexAttribPointer(location, src.components, src.rawType, src.isNormalized, src.byteStride, src.byteOffset)
      activeAttribute.id = id
    }
  }
  
  private def legacyMipMapGeneration(texture: Texture2d[_]) {
    if (!texture.supressLog && (!isPowerOfTwo(texture.dimensions.x) || !isPowerOfTwo(texture.dimensions.y))) {
      log(
        Level.WARNING,
        "Using 'settings.advanced.legacyMipMapGeneration = true' with non-power-of-two textures. " +
        "The resulting texture will be resized."
      )
      texture.supressLog = true
    }
    
    val src = texture.src
    val internalFormat = resolveInternalFormat(src.formatManifest)
    val format = resolveFormat(src.accessorManifest)
    val ftype = resolveType(src.formatManifest, src.rawType)
    
    gluBuild2DMipmaps(
      GL_TEXTURE_2D,
      internalFormat, texture.dimensions.x, texture.dimensions.y,
      format, ftype, texture.src.bindingBuffer()
    )
    
    texture.hasMatchingMipmaps = true
  }
  
  private def initialize(texture: Texture2d[_ <: Accessor with AnyVec[Double]]) :Int = {
    resourceManager.allocate(texture)
    val id = texture.managedFields.id
      
    bindTexture(GL_TEXTURE_2D, id)
    
    val generateMipmap = (texture.mipMapFilter != MipMapFilter.Disabled)
    
    if (generateMipmap && settings.legacyMipMapGeneration) legacyMipMapGeneration(texture)
    else {
      if (generateMipmap) glEnable(GL_TEXTURE_2D) // FIX for ATI's glGenerateMipmapEXT() bug.
      
      val src = texture.src
      val internalFormat = resolveInternalFormat(src.formatManifest)
      val format = resolveFormat(src.accessorManifest)
      val ftype = resolveType(src.formatManifest, src.rawType)
      
      glTexImage2D(
        GL_TEXTURE_2D, 0, //level
        internalFormat, texture.dimensions.x, texture.dimensions.y, 0, //border
        format, ftype, texture.src.bindingBuffer()
      )
      
      if (generateMipmap) {
        glGenerateMipmapEXT(GL_TEXTURE_2D)
        texture.hasMatchingMipmaps = true
      }
    }
    
    if (true) {
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT)
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT)
    }
    else { // XXX for cube-maps
      glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
      glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)
      glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE)
    }
    
    texture.clearDataChanges()
    
    id
  }
  
  private def update(id: Int, texture: Texture2d[_ <: Accessor with AnyVec[Double]]) {
    bindTexture(GL_TEXTURE_2D, id)
    
    val generateMipmap = (texture.mipMapFilter != MipMapFilter.Disabled)
    
    if (generateMipmap && settings.legacyMipMapGeneration) legacyMipMapGeneration(texture)
    else {
      if (generateMipmap) glEnable(GL_TEXTURE_2D) // FIX for ATI's glGenerateMipmapEXT() bug.
      
      val src = texture.src
      val internalFormat = resolveInternalFormat(src.formatManifest)
      val format = resolveFormat(src.accessorManifest)
      val ftype = resolveType(src.formatManifest, src.rawType)
    
      glTexSubImage2D(
        GL_TEXTURE_2D, 0, //level
        0, 0, // offset.xy
        texture.dimensions.x, texture.dimensions.y,
        format, ftype, texture.src.bindingBuffer()
      )
      
      if (generateMipmap) {
        glGenerateMipmapEXT(GL_TEXTURE_2D)
        texture.hasMatchingMipmaps = true
      }
    }
    
    texture.clearDataChanges()
  }
  
  private def resolveInternalFormat(manifest: ClassManifest[_ <: Format]) :Int = {
    manifest match {
      case Vec3.Manifest => GL_RGB8
      case Vec4.Manifest => GL_RGBA8
    }
  }
  private def resolveFormat(manifest: ClassManifest[_ <: Accessor]) :Int = {
    manifest match {
      case PrimitiveFormat.RDouble => GL_LUMINANCE
      case Vec2.Manifest => GL_LUMINANCE_ALPHA
      case Vec3.Manifest => GL_RGB
      case Vec4.Manifest => GL_RGBA
    }
  }
  private def resolveType(manifest: ClassManifest[_ <: Format], rawType: Int) :Int = {
    // XXX Different custom gl-types depending on manifest.
    rawType match {
      case RawType.UByte => GL_UNSIGNED_BYTE
      case RawType.UShort => GL_UNSIGNED_SHORT
    }
  }
  
  private def updateMipmaps(texture: Texture[_]) {
    texture match {
      case t: Texture2d[_] =>
        if (settings.legacyMipMapGeneration) legacyMipMapGeneration(t)
        else {
          glGenerateMipmapEXT(GL_TEXTURE_2D) // TODO Test on ATI cards.
          texture.hasMatchingMipmaps = true
        }
    }
  }
  
  private def updateFilters(glTarget: Int, id: Int, texture: Texture[_]) {
    bindTexture(glTarget, id)
    
    if (texture.mipMapFilter != MipMapFilter.Disabled && !texture.hasMatchingMipmaps) {
      updateMipmaps(texture)
    }
    
    glTexParameteri(
      glTarget, GL_TEXTURE_MAG_FILTER,
      texture.magFilter match {
        case ImageFilter.Nearest => GL_NEAREST
        case ImageFilter.Linear => GL_LINEAR
      }
    )
    
    glTexParameteri(
      glTarget, GL_TEXTURE_MIN_FILTER,
      texture.minFilter match {
        case ImageFilter.Nearest => texture.mipMapFilter match {
          case MipMapFilter.Disabled => GL_NEAREST
          case MipMapFilter.Nearest => GL_NEAREST_MIPMAP_NEAREST
          case MipMapFilter.Linear => GL_NEAREST_MIPMAP_LINEAR
         }
        case ImageFilter.Linear => texture.mipMapFilter match {
          case MipMapFilter.Disabled => GL_LINEAR
          case MipMapFilter.Nearest => GL_LINEAR_MIPMAP_NEAREST
          case MipMapFilter.Linear => GL_LINEAR_MIPMAP_LINEAR
        }
      }
    )
    
    if (capabilities.maxAnisotropyLevel > 1) glTexParameterf(
      glTarget, GL_TEXTURE_MAX_ANISOTROPY_EXT,
      min(capabilities.maxAnisotropyLevel, texture.anisotropyLevel).toFloat
    )
    
    texture.clearFilterChanges()
  }
  

  def bindTex2d(location: Int, textureUnit: Int, texture: Texture2d[_ <: Accessor with AnyVec[Double]]) {
    if (texture == null) {
      bindTex2d(location, textureUnit, defaultTexture2d)
      return
    }
    
    
    activeTextureUnit(textureUnit)
    val id = initUpdateTexture2d(texture)
    
    var activeTexture = activeTextures.get(location)
    if (activeTexture == null) {
      activeTexture = new ActiveTexture(activeTextureUnit, 0)
      activeTextures.put(location, activeTexture)
      
      glUniform1i(location, activeTextureUnit)
    }
    
    bindTexture(GL_TEXTURE_2D, id)
    activeTexture.id = id
  }
  
  private def initUpdateTexture2d(texture: Texture2d[_ <: Accessor with AnyVec[Double]]) :Int = {
    var id = texture.managedFields.id
    if (id == 0) id = initialize(texture)
    else if (texture.hasDataChanges) update(id, texture)
    if (texture.hasFilterChanges) updateFilters(GL_TEXTURE_2D, id, texture)
    
    id
  }
  
  def init(texture: Texture[_]) {
    activeTextureUnit(0)
    
    texture match {
      case t: Texture2d[_] => initUpdateTexture2d(t)
    }
  }
  
  
  private def initialize(shader: Shader) :Int = {
    
    def formatLineNumber(i: Int) :String = {
      if (i < 10) "00" + i.toString
      else if (i < 100) "0" + i.toString
      else if (i >= 1000) (i%1000).toString
      else i.toString
    }
    
    def format(src: String) :String = {
      val res = new StringBuilder
      val lines: Array[String] = src.split("\n")
      
      var i = 0; while (i < lines.length) {
        val line = lines(i)
        res.append(formatLineNumber(i + 1))
        res.append(" |  ")
        res.append(line)
        res.append("\n")
        
        i += 1
      }
      
      res.toString
    }
    
    val id = glCreateShader(shader.shaderType)
    glShaderSource(id, shader.src)
    glCompileShader(id)

    if (glGetShader(id, GL_COMPILE_STATUS) == 0) throw new RuntimeException(
      "Shader compilation failed.\n\nErrors:\n" +
      glGetShaderInfoLog(id, 1000) +
      "\nSource:\n" + format(shader.src)
    )
    
    shader.managedFields.id = id
    resourceManager.register(shader)
    id
  }
  
  
  private[this] val sizeType = DataBuffer[SInt, SInt](2).buffer()
  
  private def initialize(program: Technique) :Int = {
    var id = glCreateProgram()
      
    for (shader <- program.shaders) {
      var shaderId = shader.managedFields.id
      if (shaderId == 0) shaderId = initialize(shader)
      
      glAttachShader(id, shaderId)
    }
    
    glLinkProgram(id)
    
    program.managedFields.id = id
    resourceManager.register(program)
    
    
    // Query GL for program bindings.
    val uniformBindings = ArrayBuffer[UniformBinding]()
    val attributeBindings = ArrayBuffer[AttributeBinding]()
    var textureUnitCount = 0
    
    val geom = program.graphicsContext.mkGeometry()
    val mat = program.graphicsContext.mkMaterial()
    val env = program.graphicsContext.mkEnvironment()
    
    def belongs(name: String, seq: IndexedSeq[String]) :Boolean = {
      var dotIndex = name.indexOf('.')
      if (dotIndex < 0) dotIndex = name.length
      
      var sbIndex = name.indexOf('[')
      if (sbIndex < 0) sbIndex = name.length
      
      val prefix = name.substring(0, min(dotIndex, sbIndex))
      seq.contains(prefix)
    }
    def resolveUniformBlock(name: String) :Int = {
      if (belongs(name, PredefinedUniforms.Names)) UniformBlocks.Predefined
      else if (belongs(name, mat.uniformNames)) UniformBlocks.Material
      else if (belongs(name, env.propertyNames)) UniformBlocks.Environment
      else if (belongs(name, program.uniformNames)) UniformBlocks.Program
      else -1
    }
    
    val uniformCount = glGetProgram(id, GL_ACTIVE_UNIFORMS)
    val uniformStringLength = glGetProgram(id, GL_ACTIVE_UNIFORM_MAX_LENGTH)
    var i = 0; while (i < uniformCount) {
      val name = glGetActiveUniform(id, i, uniformStringLength, sizeType)
      val size = sizeType.get(0)
      val dataType = SeBindingTypes.fromGlType(sizeType.get(1))
      
      if (SeBindingTypes.isTexture(dataType)) {
            
        val blockType = resolveUniformBlock(name)
        if (size > 1) {
          var i = 0; while (i < size) {
            val location = glGetUniformLocation(id, name + "[" + i + "]")
            uniformBindings += new UniformTexBinding(blockType, name, i, dataType, location, textureUnitCount)
            textureUnitCount += 1
            
            i += 1
          }
        }
        else {
          val location = glGetUniformLocation(id, name)
          uniformBindings += new UniformTexBinding(blockType, name, 0, dataType, location, textureUnitCount)
          textureUnitCount += 1
        }
      }
      else {
          
        val blockType = resolveUniformBlock(name)
        if (size > 1) {
          var i = 0; while (i < size) {
            val location = glGetUniformLocation(id, name + "[" + i + "]")
            uniformBindings += new UniformBinding(blockType, name, i, dataType, location)
            
            i += 1
          }
        }
        else {
          val location = glGetUniformLocation(id, name)
          uniformBindings += new UniformBinding(blockType, name, 0, dataType, location)
        }
      }
      
      i += 1
    }
    
    val unusedAttributeBindings = ArrayBuffer[AttributeBinding]()
    val attributeCount = glGetProgram(id, GL_ACTIVE_ATTRIBUTES)
    val attributeStringLength = glGetProgram(id, GL_ACTIVE_ATTRIBUTE_MAX_LENGTH)
    i = 0; while (i < attributeCount) {
      val name = glGetActiveAttrib(id, i, uniformStringLength, sizeType)
      val size = sizeType.get(0) // Not used by GL.
      val dataType = SeBindingTypes.fromGlType(sizeType.get(1))
      
      val location = glGetAttribLocation(id, name)
      
      val binding = new AttributeBinding(name, dataType, location)
      
      if (belongs(name, geom.attributeNames)) attributeBindings += binding
      else unusedAttributeBindings += binding
      
      i += 1
    }
    
    
    // Uniforms
    val unusedUniformBindings = uniformBindings.filter(_.blockType == -1)
    
    if (unusedUniformBindings.size != 0) log(
      Level.SEVERE, "Invalid glsl shader: " + 
      "The following uniforms are required by the shader but do no exist in the rednerer: " +
      unusedUniformBindings.map(_.name).mkString(", ")
    )
    
    
    // Attributes
    if (unusedAttributeBindings.size != 0) log(
      Level.SEVERE, "Invalid glsl shader: " + 
      "The following attributes are required by the shader but do no exist in the rednerer: " +
      unusedAttributeBindings.map(_.name).mkString(", ") + "."
    )

    
    program.mapping = new LwjglProgramMapping(program, this)(uniformBindings, attributeBindings)
    
    id
  }
  
  def bindProgram(program: Technique) {
    var id = program.managedFields.id
    if (id == 0) id = initialize(program)
    
    useProgram(id)
  }
  
  
  // *****************************************************************************************************************
  
  def release(texture: Texture[_]) {
    resourceManager.delete(texture)
    invalidateState = true
  }
  
  def release(attributes: Attributes[_, _]) {
    resourceManager.delete(attributes)
    invalidateState = true
  }
  
  def clearFrameBuffer() {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT)
  }
  
  def manage() {
    resourceManager.manage()
  }
  
  def cleanup() {
    resourceManager.cleanup()
  }
  
  
  // *** Util methods ************************************************************************************************
  
  private val matrixBuffer = DataBuffer[RDouble, RFloat](16).buffer()
  import simplex3d.math.Accessors._
  
  def mat2x2ToBuffer(m: AnyMat2x2[_]) :FloatBuffer = {
    matrixBuffer.limit(4)
    
    matrixBuffer.put(f00(m))
    matrixBuffer.put(f10(m))
    
    matrixBuffer.put(f01(m))
    matrixBuffer.put(f11(m))
    
    matrixBuffer.rewind()
    matrixBuffer
  }
  
  def mat2x3ToBuffer(m: AnyMat2x3[_]) :FloatBuffer = {
    matrixBuffer.limit(6)
    
    matrixBuffer.put(f00(m))
    matrixBuffer.put(f10(m))
    
    matrixBuffer.put(f01(m))
    matrixBuffer.put(f11(m))
    
    matrixBuffer.put(f02(m))
    matrixBuffer.put(f12(m))
    
    matrixBuffer.rewind()
    matrixBuffer
  }
  
  def mat2x4ToBuffer(m: AnyMat2x4[_]) :FloatBuffer = {
    matrixBuffer.limit(8)
    
    matrixBuffer.put(f00(m))
    matrixBuffer.put(f10(m))
    
    matrixBuffer.put(f01(m))
    matrixBuffer.put(f11(m))
    
    matrixBuffer.put(f02(m))
    matrixBuffer.put(f12(m))
    
    matrixBuffer.put(f03(m))
    matrixBuffer.put(f13(m))
    
    matrixBuffer.rewind()
    matrixBuffer
  }
  
  def mat3x2ToBuffer(m: AnyMat3x2[_]) :FloatBuffer = {
    matrixBuffer.limit(6)
    
    matrixBuffer.put(f00(m))
    matrixBuffer.put(f10(m))
    matrixBuffer.put(f20(m))
    
    matrixBuffer.put(f01(m))
    matrixBuffer.put(f11(m))
    matrixBuffer.put(f21(m))
    
    matrixBuffer.rewind()
    matrixBuffer
  }
  
  def mat3x3ToBuffer(m: AnyMat3x3[_]) :FloatBuffer = {
    matrixBuffer.limit(9)
    
    matrixBuffer.put(f00(m))
    matrixBuffer.put(f10(m))
    matrixBuffer.put(f20(m))
    
    matrixBuffer.put(f01(m))
    matrixBuffer.put(f11(m))
    matrixBuffer.put(f21(m))
    
    matrixBuffer.put(f02(m))
    matrixBuffer.put(f12(m))
    matrixBuffer.put(f22(m))
    
    matrixBuffer.rewind()
    matrixBuffer
  }
  
  def mat3x4ToBuffer(m: AnyMat3x4[_]) :FloatBuffer = {
    matrixBuffer.limit(12)
    
    matrixBuffer.put(f00(m))
    matrixBuffer.put(f10(m))
    matrixBuffer.put(f20(m))
    
    matrixBuffer.put(f01(m))
    matrixBuffer.put(f11(m))
    matrixBuffer.put(f21(m))
    
    matrixBuffer.put(f02(m))
    matrixBuffer.put(f12(m))
    matrixBuffer.put(f22(m))
    
    matrixBuffer.put(f03(m))
    matrixBuffer.put(f13(m))
    matrixBuffer.put(f23(m))
    
    matrixBuffer.rewind()
    matrixBuffer
  }
  
  def mat4x2ToBuffer(m: AnyMat4x2[_]) :FloatBuffer = {
    matrixBuffer.limit(8)
    
    matrixBuffer.put(f00(m))
    matrixBuffer.put(f10(m))
    matrixBuffer.put(f20(m))
    matrixBuffer.put(f30(m))
    
    matrixBuffer.put(f01(m))
    matrixBuffer.put(f11(m))
    matrixBuffer.put(f21(m))
    matrixBuffer.put(f31(m))
    
    matrixBuffer.rewind()
    matrixBuffer
  }
  
  def mat4x3ToBuffer(m: AnyMat4x3[_]) :FloatBuffer = {
    matrixBuffer.limit(12)
    
    matrixBuffer.put(f00(m))
    matrixBuffer.put(f10(m))
    matrixBuffer.put(f20(m))
    matrixBuffer.put(f30(m))
    
    matrixBuffer.put(f01(m))
    matrixBuffer.put(f11(m))
    matrixBuffer.put(f21(m))
    matrixBuffer.put(f31(m))
    
    matrixBuffer.put(f02(m))
    matrixBuffer.put(f12(m))
    matrixBuffer.put(f22(m))
    matrixBuffer.put(f32(m))
    
    matrixBuffer.rewind()
    matrixBuffer
  }
  
  def mat4x4ToBuffer(m: AnyMat4x4[_]) :FloatBuffer = {
    matrixBuffer.limit(16)
    
    matrixBuffer.put(f00(m))
    matrixBuffer.put(f10(m))
    matrixBuffer.put(f20(m))
    matrixBuffer.put(f30(m))
    
    matrixBuffer.put(f01(m))
    matrixBuffer.put(f11(m))
    matrixBuffer.put(f21(m))
    matrixBuffer.put(f31(m))
    
    matrixBuffer.put(f02(m))
    matrixBuffer.put(f12(m))
    matrixBuffer.put(f22(m))
    matrixBuffer.put(f32(m))
    
    matrixBuffer.put(f03(m))
    matrixBuffer.put(f13(m))
    matrixBuffer.put(f23(m))
    matrixBuffer.put(f33(m))
    
    matrixBuffer.rewind()
    matrixBuffer
  }
  
  
  // *** Mesh mapping *************************************************************************************************
  
  private[this] def resolveBindings(mesh: AbstractMesh) :ReadArray[TechniqueBinding] = {
    val properties = mesh.worldEnvironment.properties
    val resolvedArray = new Array[TechniqueBinding](properties.length)
    
    var i = 0; while (i < properties.length) { val property = properties(i)
      if (property.isDefined) resolvedArray(i) = property.defined.resolveBinding()
      
      i += 1
    }
    new ReadArray(resolvedArray)
  }
  
  private[this] def find(names: ReadArray[String], name: String) :Int = {
    var i = 0; while (i < names.length) {
      if (names(i) == name) return i
      
      i += 1
    }
    
    -1
  }
  
  private[this] val NameIndex = """(\w*)\[(\d+)\](.*)""".r
  private[this] val Name = """(\w*)(.*)""".r
  
  private[this] final def resolveUniform(
    programBinding: UniformBinding,
    predefined: PredefinedUniforms,
    material: Material,
    environmentNames: ReadArray[String], resolvedEnv: ReadArray[TechniqueBinding],
    program: Technique
  ) :NestedBinding = {
    
    def mkName(prefix: String, name: String) = if (prefix.isEmpty) name else prefix + "." + name

    
    def isIndexValid(index: Int, name: String) :Boolean = {
      if (index < 0) {
        log(
          Level.SEVERE, "Invalid glsl shader: Uniform property '" + name +
          "' is required by the shader but cannot be resolved."
        )
        false
      }
      else true
    }
    def checkValue(value: AnyRef, name: String) {
      if (value == null) {
        log(
          Level.SEVERE, "Invalid glsl shader: Uniform property '" + name +
          "' is required by the shader but has an undefined value."
        )
      }
      else if (value.isInstanceOf[ReadTextureBinding[_]]) {
        val textureBinding = value.asInstanceOf[ReadTextureBinding[_]]
        if (!textureBinding.isBound)
          log(
            Level.SEVERE, "Invalid glsl shader: Texture '" + name +
            "' required by the shader is not bound to the mesh, default texture will be used."
          )
      }
    }
    
    def resolveOuter(blockType: Int, name: String) :TechniqueBinding = {
      ((blockType: @switch) match {
        
        case UniformBlocks.Predefined => def resolvePredefined() :AnyRef = {
          name match {
            case "se_modelViewMatrix" => predefined.se_modelViewMatrix
            case "se_modelViewProjectionMatrix" => predefined.se_modelViewProjectionMatrix
            case "se_normalMatrix" => predefined.se_normalMatrix
          }
        }; resolvePredefined()
        
        case UniformBlocks.Material => def resolveMaterial() :AnyRef = {
          val index = find(material.uniformNames, name)
          if (isIndexValid(index, name)) {
            val binding = material.uniforms(index).defined
            checkValue(binding, name)
            binding
          }
          else null
        }; resolveMaterial()
          
        case UniformBlocks.Environment => def resolveEnvironment() :AnyRef = {
          val index = find(environmentNames, name)
          if (isIndexValid(index, name)) {
            val binding = resolvedEnv(index)
            checkValue(binding, name)
            binding
          }
          else null
        }; resolveEnvironment()
        
        case UniformBlocks.Program => def resolveProgram() :AnyRef = {
          val index = find(program.uniformNames, name)
          if (isIndexValid(index, name)) {
            val binding = program.uniforms(index).defined
            checkValue(binding, name)
            binding
          }
          else null
        }; resolveProgram()
        
      }).asInstanceOf[TechniqueBinding]
    }
    
    def resolveInner(
      names: ReadArray[String], values: ReadArray[TechniqueBinding],
      prefix: String, name: String
    ) :TechniqueBinding = {
      val index = find(names, name)
      if (isIndexValid(index, mkName(prefix, name))) {
        val binding = values(index)
        checkValue(binding, mkName(prefix, name))
        binding
      }
      else null
    }
    
    def parse(
      names: ReadArray[String], values: ReadArray[TechniqueBinding],
      blockType: Int, prefix: String, path: String
    ) :AnyRef = {
      path match {
        
        case NameIndex(name, index, rest) =>
          
          val value =
            if (prefix.isEmpty) resolveOuter(blockType, name)
            else resolveInner(names, values, prefix, name)
          
          value match {
            case array: BindingArray[_] =>
              val id = index.toInt
              if (id >= array.length) {
                log(
                  Level.SEVERE, "Invalid glsl shader: Uniform property '" + mkName(prefix, name) +
                  "'[" + index + "] is required by the shader but exceeds defined array bounds."
                )
                null
              }
              else {
                val indexed = array(id)
                if (rest.isEmpty) indexed
                else {
                  indexed match {
                    case c: CompoundType[_] =>
                      parse(c.fieldNames, c.fields, blockType, mkName(prefix, name), rest)
                    case _ =>
                      log(
                        Level.SEVERE, "Incorrect nesting: Uniform property '" + mkName(prefix, name) +
                        "'[" + index + "] does not resolve to a CompoundType."
                      )
                      null
                  }
                }
              }
            case _ =>
              log(
                Level.SEVERE, "Invalid glsl shader: Uniform property '" + mkName(prefix, name) +
                "' is defined as an array in the shader but does not resolve to an instance of BindingArray."
              )
              null
          }
          
        case Name(name, rest) =>
          
          val value =
            if (prefix.isEmpty) resolveOuter(blockType, name)
            else resolveInner(names, values, prefix, name)
          
          if (rest.isEmpty) value
          else {
            value match {
              case c: CompoundType[_] =>
                parse(c.fieldNames, c.fields, blockType, mkName(prefix, name), rest)
              case _ =>
                log(
                  Level.SEVERE, "Incorrect nesting: Uniform property '" + mkName(prefix, name) +
                  "' does not resolve to a CompoundType."
                )
                null
            }
          }
          
        case _ =>
          
          log(
            Level.SEVERE, "Unsupported shader format: Uniform property '" + mkName(prefix, path) +
            "' is required by the shader but cannot be parsed."
          )
          null
      }
    }
    
    
    val value = parse(null, null, programBinding.blockType, "", programBinding.name)
    
    if (value != null) {
      value match {
        case b: NestedBinding =>
          val correctType = programBinding.dataType match {
            case SeBindingTypes.Float => b.isInstanceOf[DoubleRef]
            case SeBindingTypes.Vec2 => b.isInstanceOf[Vec2]
            case SeBindingTypes.Vec3 => b.isInstanceOf[Vec3]
            case SeBindingTypes.Vec4 => b.isInstanceOf[Vec4]
            case SeBindingTypes.Int => b.isInstanceOf[IntRef]
            case SeBindingTypes.Vec2i => b.isInstanceOf[Vec2i]
            case SeBindingTypes.Vec3i => b.isInstanceOf[Vec3i]
            case SeBindingTypes.Vec4i => b.isInstanceOf[Vec4i]
            case SeBindingTypes.Boolean => b.isInstanceOf[Boolean]
            case SeBindingTypes.Vec2b => b.isInstanceOf[Vec2b]
            case SeBindingTypes.Vec3b => b.isInstanceOf[Vec3b]
            case SeBindingTypes.Vec4b => b.isInstanceOf[Vec4b]
            case SeBindingTypes.Mat2x2 => b.isInstanceOf[Mat2x2]
            case SeBindingTypes.Mat2x3 => b.isInstanceOf[Mat2x3]
            case SeBindingTypes.Mat2x4 => b.isInstanceOf[Mat2x4]
            case SeBindingTypes.Mat3x2 => b.isInstanceOf[Mat3x2]
            case SeBindingTypes.Mat3x3 => b.isInstanceOf[Mat3x3]
            case SeBindingTypes.Mat3x4 => b.isInstanceOf[Mat3x4]
            case SeBindingTypes.Mat4x2 => b.isInstanceOf[Mat4x2]
            case SeBindingTypes.Mat4x3 => b.isInstanceOf[Mat4x3]
            case SeBindingTypes.Mat4x4 => b.isInstanceOf[Mat4x4]
            case SeBindingTypes.Texture1d => false// XXX
            case SeBindingTypes.Texture2d => b.isInstanceOf[ReadTextureBinding[_ <: Texture[_]]] // XXX verify type somehow
            case SeBindingTypes.Texture3d => false
            case SeBindingTypes.CubeTexture => false
            case SeBindingTypes.ShadowTexture1d => false
            case SeBindingTypes.ShadowTexture2d => false
          }
          
          if (!correctType) {
            log(
              Level.SEVERE, "Invalid glsl shader: Uniform property '" + programBinding.name +
              "' is defined in the shader as '" + SeBindingTypes.toString(programBinding.dataType) +
              "' but resolves to an instance of '" + b.getClass.getSimpleName + "'."
            )
          }
        case _ => log(
          Level.SEVERE, "Incorrect nesting: Uniform property '" + programBinding.name +
          "' does not resolve to an instance of NestedBinding."
        )
      }
    }
    
    value.asInstanceOf[NestedBinding]
  }
  
  private[this] final def buildUniformMapping(
    mesh: AbstractMesh, resolvedEnv: ReadArray[TechniqueBinding],
    programBindings: ReadArray[UniformBinding]
  ) :ReadArray[NestedBinding] = {
    
    val predefined = mesh.predefinedUniforms
    val material = mesh.material
    val environmentNames = mesh.worldEnvironment.propertyNames
    val program = mesh.technique.defined
    
    val uniformMapping = new Array[NestedBinding](programBindings.length)
    
    var i = 0; while (i < programBindings.length) {
      val programBinding = programBindings(i)
      uniformMapping(i) = resolveUniform(programBinding, predefined, material, environmentNames, resolvedEnv, program)
      
      i += 1
    }
    
    new ReadArray(uniformMapping)
  }
  
  
  private[this] final def buildAttributeMapping(mesh: AbstractMesh, bindings: ReadArray[AttributeBinding])
  :ReadArray[Attributes[_, _]] = {
    
    def extract(
      names: ReadArray[String], properties: ReadArray[SharedAttributes[_, _]],
      name: String
    ) :AnyRef = {
      val index = find(names, name)
      if (index < 0) {
        log(
          Level.SEVERE, "Invalid glsl shader: Attributes property '" + name +
          "' is required by the shader but cannot be resolved."
        )
        null
      }
      else {
        val shared = properties(index)
        if (!shared.isDefined) {
          log(
            Level.SEVERE, "Invalid glsl shader: Attributes property '" + name +
            "' is required by the shader but has an undefined value."
          )
        }
        shared.defined.asInstanceOf[AnyRef]
      }
    }
    
    val mapping = new Array[Attributes[_, _]](bindings.length)
    val geom = mesh.geometry
    
    var i = 0; while (i < bindings.length) {
      val binding = bindings(i)
      mapping(i) = extract(geom.attributeNames, geom.attributes, binding.name).asInstanceOf[Attributes[_, _]]
      
      i += 1
    }
    
    new ReadArray(mapping)
  }
  
  
  final def rebuildMeshMapping(
    mesh: AbstractMesh,
    programMapping: GlslProgramMapping
  ) {
    val meshMapping = mesh.mapping
    val resolvedEnv = resolveBindings(mesh)
    
    meshMapping.uniformVectors = buildUniformMapping(
      mesh, resolvedEnv, programMapping.uniformVectors
    ).asInstanceOf[ReadArray[VectorBinding]]
    
    meshMapping.uniformMatrices = buildUniformMapping(
      mesh, resolvedEnv, programMapping.uniformMatrices
    ).asInstanceOf[ReadArray[AnyMat[_]]]
    
    meshMapping.uniformTextures = buildUniformMapping(
      mesh, resolvedEnv, programMapping.uniformTextures
    ).asInstanceOf[ReadArray[ReadTextureBinding[_]]]
    
    meshMapping.attributes = buildAttributeMapping(mesh, programMapping.attributes)
  }
}
