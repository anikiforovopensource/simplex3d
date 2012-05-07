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

package simplex3d.engine
package backend.lwjgl

import java.util.logging._
import java.nio._
import java.util.HashMap
import scala.annotation._
import scala.collection.mutable.ArrayBuffer
import org.lwjgl.opengl._
import ArbEquivalents.{ GL15 ,GL20 }
import simplex3d.math.types._
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.engine.util._
import simplex3d.engine.scene._
import simplex3d.engine.graphics._
import simplex3d.engine.backend.opengl._


private[lwjgl] class ActiveAttributeId(var id: Int)
private[lwjgl] class ActiveTextureId(var unit: Int, var id: Int)


private[lwjgl] object RenderContext {
  private final val logger = Logger.getLogger(classOf[RenderContext].getName)
}


private[lwjgl] final class RenderContext(val capabilities: GraphicsCapabilities, val settings: AdvancedSettings)
extends graphics.RenderContext with GlAccess {
  import GL11._; import GL12._; import GL13._; import GL14._; import GL15._
  import GL20._; import GL21._; import EXTTextureFilterAnisotropic._; import EXTFramebufferObject._
  import RenderContext.logger._
  import AccessScene._; import AccessChanges._
  

  private val defaultTexture2d = {
    val dims = ConstVec2i(4)
    val data = DataBuffer[Vec3, UByte](dims.x*dims.y)
    
    for (y <- 0 until dims.y; x <- 0 until dims.x) {
      data(x + y*dims.x) = Vec3(1, 0, 1)
    }
    
    Texture2d.fromData(dims, data.asReadOnly())
  }
  
  val defaultProgram = {
    val vertexShader = """
      uniform mat4 se_modelViewProjectionMatrix;
      attribute vec3 vertices;
      
      void main() {
        gl_Position = se_modelViewProjectionMatrix*vec4(vertices, 1.0);
      }
    """
    
    val fragmentShader = """
      void main() {
        gl_FragColor = vec4(1.0, 0.0, 1.0, 1.0);
      }
    """
    
    val shaders = Set[Shader](
      new Shader(Shader.Vertex, vertexShader),
      new Shader(Shader.Fragment, fragmentShader)
    )
    
    new Technique(MinimalGraphicsContext, shaders)
  }
  
  
  private val resourceManager = new GlResourceManager(
    attributeManager = new IdManager(100)(glGenBuffers(_), glDeleteBuffers(_)),
    textureManager = new IdManager(20)(glGenTextures(_), glDeleteTextures(_)),
    glDeleteShader(_),
    glDeleteProgram(_)
  )
  
  val predefinedUniforms = new PredefinedUniforms
  
  
  // ******************************************************************************************************************
  
  private var invalidateState = false

  private var faceCullingState: Int = 0
  private var mipmapHintEnabled = false
  
  private final var activeProgramId = 0
  private final var boundBufferId = 0
  private final var boundIndexId = 0
  
  private final var activeTextureUnit = 0
  
  private final def textureUnits = new Array[Int](32)
  private final def boundTexture = textureUnits(activeTextureUnit)
  private final def boundTexture_=(id: Int) { textureUnits(activeTextureUnit) = id }
  
  private final val activeAttributes = new HashMap[Int, ActiveAttributeId] //XXX possibly use different data structures
  private final val activeTextures = new HashMap[Int, ActiveTextureId]
  
  final def requiresReset = invalidateState
  
  final def setFaceCulling(faceCulling: FaceCulling.type#Value) {
    val resolved = {
      faceCulling match {
        case FaceCulling.Disabled => 0
        case FaceCulling.Front => GL_FRONT
        case FaceCulling.Back => GL_BACK
      }
    }
    
    if (resolved != faceCullingState) {
      if (resolved > 0) {
        if (faceCullingState <= 0) glEnable(GL_CULL_FACE)
        glCullFace(resolved)
      }
      else glDisable(GL_CULL_FACE)
      faceCullingState = resolved
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
    regions.merge()
    
    var i = 0; while (i < regions.size) {
      
      val first = regions.first(i)
      val count = regions.count(i)
      
      glBufferSubData(
        GL_ARRAY_BUFFER,
        first*data.byteStride,
        data.bindingBufferSubData(first, count)
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
      activeAttribute = new ActiveAttributeId(0)
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
    if (!texture.suspendLogging && (!isPowerOfTwo(texture.dimensions.x) || !isPowerOfTwo(texture.dimensions.y))) {
      log(
        Level.WARNING,
        "Using 'settings.advanced.legacyMipMapGeneration = true' with non-power-of-two textures. " +
        "The resulting texture will be resized."
      )
      texture.suspendLogging = true
    }
    
    val src = texture.src
    val internalFormat = resolveInternalFormat(src.formatManifest)
    val format = resolveFormat(src.accessorManifest)
    val ftype = resolveType(src.formatManifest, src.rawType)
    
    org.lwjgl.util.glu.MipMap.gluBuild2DMipmaps(
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
        glEnable(GL_TEXTURE_2D) // FIX for ATI's glGenerateMipmapEXT() bug.
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
        glEnable(GL_TEXTURE_2D) // FIX for ATI's glGenerateMipmapEXT() bug.
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
      activeTexture = new ActiveTextureId(activeTextureUnit, 0)
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
    if (shader.compilationFailed) return 0
    
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
    
    val id = glCreateShader(shader.shaderType match {
      case Shader.Vertex => GL_VERTEX_SHADER
      case Shader.Fragment => GL_FRAGMENT_SHADER
    })
    glShaderSource(id, shader.src)
    glCompileShader(id)

    val logLength = glGetShader(id, GL_INFO_LOG_LENGTH)
    val shaderLog = glGetShaderInfoLog(id, logLength)
    
    if (glGetShader(id, GL_COMPILE_STATUS) == 0) {
      log(
        Level.SEVERE,
        "Shader compilation failed with the following errors:\n\n" + shaderLog + "\nSource:\n" + format(shader.src)
      )
      
      glDeleteShader(id)
      shader.compilationFailed = true
      return 0
    }
    
    if (settings.logShaderWarnings && shaderLog.length() > 0) {
      log(
        Level.WARNING,
        "Shader compilation succeeded with some warnings:\n\n" + shaderLog + "\nSource:\n" + format(shader.src)
      )
    }
    
    shader.managedFields.id = id
    resourceManager.register(shader)
    id
  }
  
  
  private[this] val sizeType = DataBuffer[SInt, SInt](2).buffer()
  
  private def initialize(program: Technique) :Int = {
    if (program.compilationFailed) return 0
    
    var progId = glCreateProgram()
    var compilationFailed = false
    
    for (shader <- program.shaders) {
      var shaderId = shader.managedFields.id
      if (shaderId == 0) shaderId = initialize(shader)
      
      if (shaderId != 0) glAttachShader(progId, shaderId)
      else compilationFailed = true
    }
    
    if (compilationFailed) {
      glDeleteProgram(progId)
      program.compilationFailed = true
      return 0
    }

    
    glLinkProgram(progId)
    
    if (glGetProgram(progId, GL_LINK_STATUS) == 0) {
      val logInfo = glGetProgramInfoLog(progId, 64*1024)
      log(Level.SEVERE, "The program failed to link:\n" + logInfo)
      glDeleteProgram(progId)
      return 0
    }
    
    
    program.managedFields.id = progId
    resourceManager.register(program)
    
    
    // Query GL for program bindings.
    var uniformBindings = ArrayBuffer[ActiveUniform]()
    val attributeBindings = ArrayBuffer[ActiveAttribute]()
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
    
    val uniformCount = glGetProgram(progId, GL_ACTIVE_UNIFORMS)
    val uniformStringLength = glGetProgram(progId, GL_ACTIVE_UNIFORM_MAX_LENGTH)
    var i = 0; while (i < uniformCount) {
      val glName = glGetActiveUniform(progId, i, uniformStringLength, sizeType)
      val isArray = glName.endsWith("[0]")
      val name = if (isArray) glName.dropRight(3) else glName
      val size = sizeType.get(0)
      val dataType = EngineBindingTypes.fromGlType(sizeType.get(1))
      
      if (EngineBindingTypes.isTexture(dataType)) {

        if (isArray) {
          var i = 0; while (i < size) {
            val arrayName = name + "[" + i + "]"
            val location = glGetUniformLocation(progId, arrayName)
            uniformBindings += new ActiveTexture(arrayName, dataType, location, textureUnitCount)
            textureUnitCount += 1
            
            i += 1
          }
        }
        else {
          val location = glGetUniformLocation(progId, name)
          uniformBindings += new ActiveTexture(name, dataType, location, textureUnitCount)
          textureUnitCount += 1
        }
      }
      else {
          
        if (isArray) {
          var i = 0; while (i < size) {
            val arrayName = name + "[" + i + "]"
            val location = glGetUniformLocation(progId, arrayName)
            uniformBindings += new ActiveUniform(arrayName, dataType, location)
            
            i += 1
          }
        }
        else {
          val location = glGetUniformLocation(progId, name)
          uniformBindings += new ActiveUniform(name, dataType, location)
        }
      }
      
      i += 1
    }
    
    val attributeCount = glGetProgram(progId, GL_ACTIVE_ATTRIBUTES)
    val attributeStringLength = glGetProgram(progId, GL_ACTIVE_ATTRIBUTE_MAX_LENGTH)
    i = 0; while (i < attributeCount) {
      val name = glGetActiveAttrib(progId, i, attributeStringLength, sizeType)
      val size = sizeType.get(0) // Not used by GL.
      val dataType = EngineBindingTypes.fromGlType(sizeType.get(1))
      val location = glGetAttribLocation(progId, name)
      
      val binding = new ActiveAttribute(name, dataType, location)
      
      if (belongs(name, geom.attributeNames)) attributeBindings += binding
      
      i += 1
    }
    

    program.mapping = new ProgramMapping(program, this)(uniformBindings, attributeBindings)
    
    progId
  }
  
  def bindProgram(program: Technique) :Boolean = {
    var id = program.managedFields.id
    if (id == 0) id = initialize(program)
    
    if (id != 0) {
      useProgram(id)
      true
    }
    else false
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
  
  def viewportDimensions() :ConstVec2i = {
    // XXX this data must come from bound FrameBuffer (scala object).
    glGetInteger(GL_VIEWPORT, intBuffer)
    ConstVec2i(intBuffer.get(2), intBuffer.get(3))
  }

  
  def manage() {
    resourceManager.manage()
  }
  
  def cleanup() {
    resourceManager.cleanup()
  }
  
  
  // *** Util methods ************************************************************************************************
  
  private val intBuffer = DataBuffer[SInt, SInt](16).buffer()
  private val floatBuffer = DataBuffer[RDouble, RFloat](16).buffer()
  
  import simplex3d.math.Accessors._
  
  def mat2x2ToBuffer(m: AnyMat[_]) :FloatBuffer = {
    floatBuffer.limit(4)
    
    floatBuffer.put(f00(m)); floatBuffer.put(f01(m))
    floatBuffer.put(f10(m)); floatBuffer.put(f11(m))
    
    floatBuffer.rewind()
    floatBuffer
  }
  
  def mat2x3ToBuffer(m: AnyMat2x3[_]) :FloatBuffer = {
    floatBuffer.limit(6)
    
    floatBuffer.put(f00(m)); floatBuffer.put(f01(m)); floatBuffer.put(f02(m))
    floatBuffer.put(f10(m)); floatBuffer.put(f11(m)); floatBuffer.put(f12(m))
    
    floatBuffer.rewind()
    floatBuffer
  }
  
  def mat2x4ToBuffer(m: AnyMat2x4[_]) :FloatBuffer = {
    floatBuffer.limit(8)
    
    floatBuffer.put(f00(m)); floatBuffer.put(f01(m)); floatBuffer.put(f02(m)); floatBuffer.put(f03(m))
    floatBuffer.put(f10(m)); floatBuffer.put(f11(m)); floatBuffer.put(f12(m)); floatBuffer.put(f13(m))
    
    floatBuffer.rewind()
    floatBuffer
  }
  
  def mat3x2ToBuffer(m: AnyMat3x2[_]) :FloatBuffer = {
    floatBuffer.limit(6)
    
    floatBuffer.put(f00(m)); floatBuffer.put(f01(m))
    floatBuffer.put(f10(m)); floatBuffer.put(f11(m))
    floatBuffer.put(f20(m)); floatBuffer.put(f21(m))
    
    floatBuffer.rewind()
    floatBuffer
  }
  
  def mat3x3ToBuffer(m: AnyMat[_]) :FloatBuffer = {
    floatBuffer.limit(9)
    
    floatBuffer.put(f00(m)); floatBuffer.put(f01(m)); floatBuffer.put(f02(m))
    floatBuffer.put(f10(m)); floatBuffer.put(f11(m)); floatBuffer.put(f12(m))
    floatBuffer.put(f20(m)); floatBuffer.put(f21(m)); floatBuffer.put(f22(m))
    
    floatBuffer.rewind()
    floatBuffer
  }
  
  def mat3x4ToBuffer(m: AnyMat3x4[_]) :FloatBuffer = {
    floatBuffer.limit(12)
    
    floatBuffer.put(f00(m)); floatBuffer.put(f01(m)); floatBuffer.put(f02(m)); floatBuffer.put(f03(m))
    floatBuffer.put(f10(m)); floatBuffer.put(f11(m)); floatBuffer.put(f12(m)); floatBuffer.put(f13(m))
    floatBuffer.put(f20(m)); floatBuffer.put(f21(m)); floatBuffer.put(f22(m)); floatBuffer.put(f23(m))
    
    floatBuffer.rewind()
    floatBuffer
  }
  
  def mat4x2ToBuffer(m: AnyMat4x2[_]) :FloatBuffer = {
    floatBuffer.limit(8)
    
    floatBuffer.put(f00(m)); floatBuffer.put(f01(m))
    floatBuffer.put(f10(m)); floatBuffer.put(f11(m))
    floatBuffer.put(f20(m)); floatBuffer.put(f21(m))
    floatBuffer.put(f30(m)); floatBuffer.put(f31(m))
    
    floatBuffer.rewind()
    floatBuffer
  }
  
  def mat4x3ToBuffer(m: AnyMat4x3[_]) :FloatBuffer = {
    floatBuffer.limit(12)
    
    floatBuffer.put(f00(m)); floatBuffer.put(f01(m)); floatBuffer.put(f02(m))
    floatBuffer.put(f10(m)); floatBuffer.put(f11(m)); floatBuffer.put(f12(m))
    floatBuffer.put(f20(m)); floatBuffer.put(f21(m)); floatBuffer.put(f22(m))
    floatBuffer.put(f30(m)); floatBuffer.put(f31(m)); floatBuffer.put(f32(m))
    
    floatBuffer.rewind()
    floatBuffer
  }
  
  def mat4x4ToBuffer(m: AnyMat[_]) :FloatBuffer = {
    floatBuffer.limit(16)
    
    floatBuffer.put(f00(m)); floatBuffer.put(f01(m)); floatBuffer.put(f02(m)); floatBuffer.put(f03(m))
    floatBuffer.put(f10(m)); floatBuffer.put(f11(m)); floatBuffer.put(f12(m)); floatBuffer.put(f13(m))
    floatBuffer.put(f20(m)); floatBuffer.put(f21(m)); floatBuffer.put(f22(m)); floatBuffer.put(f23(m))
    floatBuffer.put(f30(m)); floatBuffer.put(f31(m)); floatBuffer.put(f32(m)); floatBuffer.put(f33(m))
    
    floatBuffer.rewind()
    floatBuffer
  }
  
  
  // *** Mesh mapping *************************************************************************************************
  
  private[this] def find(names: ReadArray[String], name: String) :Int = {
    var i = 0; while (i < names.length) {
      if (names(i) == name) return i
      
      i += 1
    }
    
    -1
  }
  
  
  private[this] final def resolveUniform(
    meshName: String,
    programBinding: ActiveUniform,
    predefined: PredefinedUniforms,
    material: Material,
    environment: Environment,
    program: Technique
  ) :Binding = {
    
    val binding = program.graphicsContext.resolveUniformPath(
      programBinding.name, predefined, material, environment, program.programUniforms//XXX possibly better naming for programUniforms, shaderUniforms
    ).asInstanceOf[Binding]
    
    if (binding != null) {
      val correctType = checkUniformType(binding, programBinding.dataType)
      
      if (!correctType) {
        
        /* Replace when 2.10 is out.
        val resolved = binding match {
          case tb: ReadTextureBinding[_] => ClassUtil.simpleName(tb.bindingManifest.erasure)
          case _ => ClassUtil.simpleName(binding.getClass)
        }*/
        val resolved =
          if (TextureBinding.avoidCompilerCrashB(binding))
            ClassUtil.simpleName(TextureBinding.avoidCompilerCrash(binding).bindingManifest.erasure)
          else
            ClassUtil.simpleName(binding.getClass)
        
        log(
          Level.SEVERE, "Uniform '" + programBinding.name +
          "' is defined as '" + EngineBindingTypes.toString(programBinding.dataType) +
          "' but resolves to an instance of '" + resolved + "' for mesh '" + meshName +
          "'. This uniform will have an undefined value in the shader."
        )
        return null
      }
    }
    
    if (binding == null && !programBinding.name.endsWith("nvidiaBugWorkaround")) log(
      Level.SEVERE, "Uniform '" + programBinding.name + "' could not be resolved for mesh '" + meshName + "'."
    )
    else if (binding.isInstanceOf[ReadTextureBinding[_]]) {
      val textureBinding = TextureBinding.avoidCompilerCrash(binding)
      if (!textureBinding.isBound) log(
        Level.SEVERE, "Texture '" + programBinding.name + "' is not defined for mesh '" +
        meshName + "'. Default texture will be used."
      )
    }
    
    binding.asInstanceOf[Binding]
  }
  
  private[this] def checkAttributeType(m: ClassManifest[_], dtype: Int) :Boolean = {
     dtype match {
      case EngineBindingTypes.Float => m == PrimitiveFormat.RFloat
      case EngineBindingTypes.Vec2 => m == Vec2.Manifest
      case EngineBindingTypes.Vec3 => m == Vec3.Manifest
      case EngineBindingTypes.Vec4 => m == Vec4.Manifest
      case EngineBindingTypes.Int => m == PrimitiveFormat.SInt
      case EngineBindingTypes.Vec2i => m == Vec2i.Manifest
      case EngineBindingTypes.Vec3i => m == Vec3i.Manifest
      case EngineBindingTypes.Vec4i => m == Vec4i.Manifest
      case EngineBindingTypes.Mat2x2 => m == Mat2x2.Manifest
      case EngineBindingTypes.Mat2x3 => m == Mat2x3.Manifest
      case EngineBindingTypes.Mat2x4 => m == Mat2x4.Manifest
      case EngineBindingTypes.Mat3x2 => m == Mat3x2.Manifest
      case EngineBindingTypes.Mat3x3 => m == Mat3x3.Manifest
      case EngineBindingTypes.Mat3x4 => m == Mat3x4.Manifest
      case EngineBindingTypes.Mat4x2 => m == Mat4x2.Manifest
      case EngineBindingTypes.Mat4x3 => m == Mat4x3.Manifest
      case EngineBindingTypes.Mat4x4 => m == Mat4x4.Manifest
      case _ => false
    }
  }
  
  private[this] def checkUniformType(binding: Binding, dtype: Int) :Boolean = {
    dtype match {
      case EngineBindingTypes.Float => binding.isInstanceOf[DoubleRef]
      case EngineBindingTypes.Vec2 => binding.isInstanceOf[Vec2]
      case EngineBindingTypes.Vec3 => binding.isInstanceOf[Vec3]
      case EngineBindingTypes.Vec4 => binding.isInstanceOf[Vec4]
      case EngineBindingTypes.Int => binding.isInstanceOf[IntRef]
      case EngineBindingTypes.Vec2i => binding.isInstanceOf[Vec2i]
      case EngineBindingTypes.Vec3i => binding.isInstanceOf[Vec3i]
      case EngineBindingTypes.Vec4i => binding.isInstanceOf[Vec4i]
      case EngineBindingTypes.Boolean => binding.isInstanceOf[Boolean]
      case EngineBindingTypes.Vec2b => binding.isInstanceOf[Vec2b]
      case EngineBindingTypes.Vec3b => binding.isInstanceOf[Vec3b]
      case EngineBindingTypes.Vec4b => binding.isInstanceOf[Vec4b]
      case EngineBindingTypes.Mat2x2 => binding.isInstanceOf[Mat2x2]
      case EngineBindingTypes.Mat2x3 => binding.isInstanceOf[Mat2x3]
      case EngineBindingTypes.Mat2x4 => binding.isInstanceOf[Mat2x4]
      case EngineBindingTypes.Mat3x2 => binding.isInstanceOf[Mat3x2]
      case EngineBindingTypes.Mat3x3 =>
        binding.isInstanceOf[Mat2x3] || binding.isInstanceOf[Mat3x2] || binding.isInstanceOf[Mat3x3]
      case EngineBindingTypes.Mat3x4 => binding.isInstanceOf[Mat3x4]
      case EngineBindingTypes.Mat4x2 => binding.isInstanceOf[Mat4x2]
      case EngineBindingTypes.Mat4x3 => binding.isInstanceOf[Mat4x3]
      case EngineBindingTypes.Mat4x4 =>
        binding.isInstanceOf[Mat2x4] || binding.isInstanceOf[Mat4x2] ||
        binding.isInstanceOf[Mat3x4] || binding.isInstanceOf[Mat4x3] || binding.isInstanceOf[Mat4x4]
      case EngineBindingTypes.Texture1d => false// XXX
      
      /* Replace when 2.10 is out
      case EngineBindingTypes.Texture2d => binding match {
          case tb: ReadTextureBinding[_] => Texture2d.Manifest.erasure.isAssignableFrom(tb.bindingManifest.erasure)
          case _ => false
        }*/
      case EngineBindingTypes.Texture2d =>
        if (TextureBinding.avoidCompilerCrashB(binding)) {
          val erasure = TextureBinding.avoidCompilerCrash(binding).bindingManifest.erasure
          Texture2d.Manifest.erasure.isAssignableFrom(erasure)
        }
        else false
      
      case EngineBindingTypes.Texture3d => false
      case EngineBindingTypes.CubeTexture => false
      case EngineBindingTypes.ShadowTexture1d => false
      case EngineBindingTypes.ShadowTexture2d => false
      case _ => false
    }
  }
  
  private[this] final def buildUniformMapping(
    programBindings: ReadArray[ActiveUniform],
    predefined: PredefinedUniforms,
    mesh: AbstractMesh
  ) :ReadArray[Binding] = {
    
    val program = mesh.technique.get
    val uniformMapping = new Array[Binding](programBindings.length)
    
    var i = 0; while (i < programBindings.length) {
      val programBinding = programBindings(i)
      
      uniformMapping(i) = resolveUniform(
        mesh.name, programBinding,
        predefined, mesh.material, mesh.worldEnvironment, program
      )
      
      i += 1
    }
    
    new ReadArray(uniformMapping)
  }
  
  
  private[this] final def buildAttributeMapping(mesh: AbstractMesh, bindings: ReadArray[ActiveAttribute])
  :ReadArray[Attributes[_, _]] = {
    
    val mapping = new Array[Attributes[_, _]](bindings.length)
    val geom = mesh.geometry
    val graphicsContext = mesh.technique.get.graphicsContext
    
    var i = 0; while (i < bindings.length) {
      val binding = bindings(i)
      var attrib = graphicsContext.resolveAttributePath(binding.name, geom)
      
      if (attrib == null) log(
        Level.SEVERE,
        "Attributes '" + binding.name + "' cannot be resolved for mesh '" + mesh.name + "'."
      )
      else {
        val correctType = checkAttributeType(attrib.src.accessorManifest, binding.dataType)
        
        if (!correctType) {
          val resolved = ClassUtil.simpleName(binding.getClass)
              
          log(
            Level.SEVERE, "Attributes '" + binding.name +
            "' are defined as a sequence of '" + EngineBindingTypes.toString(binding.dataType) +
            "' but resolve to a sequence of '" + resolved + "' for mesh '" + mesh.name +
            "'. These attributes will have undefined values in the shader."
          )
        }
        else {
          mapping(i) = attrib.asInstanceOf[Attributes[_, _]]
        }
      }
      
      i += 1
    }
    
    new ReadArray(mapping)
  }
  
  
  final def rebuildMeshMapping(
    mesh: AbstractMesh,
    programMapping: backend.opengl.ProgramMapping
  ) {
    val uniformVectors = buildUniformMapping(
      programMapping.uniformVectors, predefinedUniforms, mesh
    ).asInstanceOf[ReadArray[VectorLike]]
    
    val uniformMatrices = buildUniformMapping(
      programMapping.uniformMatrices, predefinedUniforms, mesh
    ).asInstanceOf[ReadArray[AnyMat[_]]]
    
    val uniformTextures = TextureBinding.avoidCompilerCrash(buildUniformMapping(
      programMapping.uniformTextures, predefinedUniforms, mesh
    ))
    
    val attributes = buildAttributeMapping(mesh, programMapping.attributes)
    
    mesh.mapping = new MeshMapping(uniformVectors, uniformMatrices, uniformTextures, attributes)
  }
}
