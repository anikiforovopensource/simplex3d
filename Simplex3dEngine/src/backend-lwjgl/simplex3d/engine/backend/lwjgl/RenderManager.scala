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
import scala.annotation._
import scala.collection.mutable.ArrayBuffer
import org.lwjgl.opengl._
import ArbEquivalents.{ GL15 ,GL20 }
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.engine.util._
import simplex3d.engine.scene._
import simplex3d.engine.graphics._
import simplex3d.engine.backend.opengl._
import simplex3d.engine.graphics.MinimalGraphicsContext


private[lwjgl] object RenderManager {
  private final val logger = Logger.getLogger(classOf[RenderManager].getName)
}


final class RenderManager extends graphics.RenderManager {
  import GL11._; import GL12._; import GL13._; import GL14._; import GL15._;
  import GL20._; import GL21._
  import RenderManager.logger._
  import AccessScene._; import AccessChanges._
  import AccessGlUnsafe._
  
  val driver = "lwjgl"
  
  private var _renderContext: RenderContext = null
  def renderContext = _renderContext
  def init(graphicsCapabilities: GraphicsCapabilities, settings: AdvancedSettings) {
    _renderContext = new RenderContext(graphicsCapabilities, settings)
  }
  
  
  private val elementRange = new ElementRange()
  
  //XXX this belongs in path manager
  class DebugLinesManager(val name: String, val indicesPerMesh: Int = 65536, val verticesPerMesh: Int = 65536) {
    private val meshes = new ArrayBuffer[AbstractMesh](1)
    newMesh()
    
    private var currentMesh = 0
    private var currentIndex = 0
    private var currentVertex = 0
    
    def clear() {
      currentMesh = 0
      currentIndex = 0
      currentVertex = 0
    }
    
    private def newMesh() {
      val mesh = new BaseMesh(name + " Debug " + (currentMesh + 1))
      mesh.geometry.indices := Attributes.fromData(IndexBuffer(verticesPerMesh - 1, indicesPerMesh))
      mesh.geometry.vertices := Attributes[Vec3, RFloat](verticesPerMesh)
      mesh.geometry.primitive.update.mode := VertexMode.Lines
      mesh.geometry.primitive.update.lineWidth := 2
      meshes += mesh
    }
    
    def append(indices: inIndex, vertices: inDataSeq[Vec3, Raw]) {
      require(indices.size < indicesPerMesh && vertices.size < verticesPerMesh)
      
      if (currentIndex + indices.size > indicesPerMesh || currentVertex + vertices.size > verticesPerMesh) {
        currentMesh += 1
        currentIndex = 0
        currentVertex = 0
        
        if (currentMesh >= meshes.size) newMesh()
      }
      
      val mesh = meshes(currentMesh)
      
      val destIndices = mesh.geometry.indices.write
      var i = 0; while (i < indices.size) {
        destIndices(currentIndex + i) = indices(i) + currentVertex
        i += 1
      }
      mesh.geometry.vertices.write.put(currentVertex, vertices)
      
      currentIndex += indices.size
      currentVertex += vertices.size
      
      mesh.elementRange.update.count := currentIndex
    }
    
    def renderArray: Seq[AbstractMesh] = meshes
  }
  
  object Box {
    private val boxIndices = DataArray[SInt, UByte](
      0, 1,
      1, 2,
      2, 3,
      3, 0,
      4, 5,
      5, 6,
      6, 7,
      7, 4,
      0, 4,
      1, 5,
      3, 7,
      2, 6
    )
    private val boxVertices = DataArray[Vec3, RFloat](
      Vec3(0, 0, 0),
      Vec3(1, 0, 0),
      Vec3(1, 1, 0),
      Vec3(0, 1, 0),
      Vec3(0, 0, 1),
      Vec3(1, 0, 1),
      Vec3(1, 1, 1),
      Vec3(0, 1, 1)
    )
    
    private val transformedVertices = DataArray[Vec3, RFloat](boxVertices.size)
    
    def append(debugManager: DebugLinesManager)(min: inVec3, max: inVec3, transformation: inMat4x3) {
      val t = Mat4x3 scale(max - min) translate(min) concat(transformation)
      
      var i = 0; while (i < boxVertices.size) {
        transformedVertices(i) = t.transformPoint(boxVertices(i))
        
        i += 1
      }
      
      debugManager.append(boxIndices, transformedVertices)
    }
  }
  
  val showBoundingVolumes = true
  val debugBoundingVolumes = new DebugLinesManager("Bounding Volume")
    
  
  def render(time: TimeStamp, camera: AbstractCamera, renderArray: SortBuffer[AbstractMesh]) {
    if (renderContext.requiresReset) renderContext.resetState()
    
    // XXX these should come from the path, and get activated via context
    glEnable(GL_DEPTH_TEST)
    glDepthFunc(GL_LESS)
    
    
    val predefinedUniforms = renderContext.predefinedUniforms
    predefinedUniforms.se_projectionMatrix := camera.projection
    predefinedUniforms.se_viewMatrix := camera.view
    predefinedUniforms.se_viewDimensions := renderContext.viewportDimensions()
    predefinedUniforms.se_timeTotal := time.total
    predefinedUniforms.se_timeInterval := time.interval
    
    var i = 0; while (i < renderArray.size) {
      val mesh = renderArray(i)
      render(camera, mesh)
      
      i += 1
    }
    
    //XXX this belongs in path manager
    if (showBoundingVolumes) {
      import simplex3d.engine.bounding._
      
      debugBoundingVolumes.clear()
      
      var indexCount = 0
      var vertexCount = 0
      
      var i = 0; while (i < renderArray.size) {
        val mesh = renderArray(i)
        
        mesh.debugBoundingVolume match {
          case b: Aabb =>
            Box.append(debugBoundingVolumes)(b.min, b.max, Mat4x3.Identity)
          case b: Oabb =>
            Box.append(debugBoundingVolumes)(b.min, b.max, mesh.worldMatrix)
          case b: Obb =>
            Box.append(debugBoundingVolumes)(b.min, b.max, b.transformation concat mesh.worldMatrix)
          case _ =>
            // ignore
        }
        
        i += 1
      }
      
      i = 0; while (i < debugBoundingVolumes.renderArray.size) {
        val mesh = debugBoundingVolumes.renderArray(i)
        render(camera, mesh)
        
        i += 1
      }
    }
  }

  
  private def render(camera: AbstractCamera, mesh: AbstractMesh) {
    
    
    var useDefaultProgram = false
    
    if (!mesh.technique.isDefined) {
      useDefaultProgram = true
      log(Level.SEVERE, "Mesh '" + mesh.name + "' does not have a technique assigned. Default technique will be used.")
    }
    else {
      useDefaultProgram = !renderContext.bindProgram(mesh.technique.get)
      
      if (useDefaultProgram) log(
        Level.SEVERE, "Unable to use technique provided for mesh '" + mesh.name +
        "'. Default technique will be used."
      )
    }
    
    if (useDefaultProgram) {
      mesh.technique := renderContext.defaultProgram
      return render(camera, mesh)
    }
    
    val program = mesh.technique.get
    val programMapping = program.mapping
    
    if (mesh.technique.hasRefChanges) {
      renderContext.rebuildMeshMapping(mesh, programMapping)
      mesh.technique.clearRefChanges()
      
      def resolveUpdatableEffects() {
        var updatables = new ArrayBuffer[AnyRef]
        val properties = mesh.worldEnvironment.properties
        var i = 0; while (i < properties.length) { val property = properties(i)
          if (property.isDefined && property.get.isInstanceOf[UpdatableEnvironmentalEffect]) {
            updatables += property.get
          }
          
          i += 1
        }
        
        mesh.updatableEffects = new ReadArray(updatables.toArray).asInstanceOf[ReadArray[UpdatableEnvironmentalEffect]]
      }; resolveUpdatableEffects()
    }
    
    val transformation = mesh.worldMatrix
    val geometry = mesh.geometry
    val material = mesh.material
    
    mesh.resolveElementRange(elementRange)
    renderContext.setFaceCulling(material.faceCulling.get.toConst)
    
    val predefinedUniforms = renderContext.predefinedUniforms
    predefinedUniforms.se_modelViewMatrix := transformation concat camera.view
    predefinedUniforms.se_modelViewProjectionMatrix := camera.projection*Mat4(predefinedUniforms.se_modelViewMatrix)
    predefinedUniforms.se_normalMatrix := transpose(inverse(Mat3(predefinedUniforms.se_modelViewMatrix)))
    
    // Update bindings using predefined uniforms.
    val effects = mesh.updatableEffects
    var i = 0; while (i < effects.length) { val property = effects(i)
      property.updateBinding(predefinedUniforms)
      
      i += 1
    }
    
//    val directionalLightPos = Vec3(-0.25, 0.5, 1)
//    val ecLightDir = -normalize(camera.view.transformVector(directionalLightPos))
//    program("ecLightDir") = ecLightDir
    
    
    // XXX cache state in render context.
    val primitive = geometry.primitive.get
    val vertexMode = primitive.mode.toConst match {
      case VertexMode.Points =>
        //XXX disable point sprites
        
        glPointSize(primitive.pointSize.toFloat)
        
        GL_POINTS
        
      case VertexMode.PointSprites =>
        glEnable(GL_POINT_SPRITE)
        glPointParameteri(GL_POINT_SPRITE_COORD_ORIGIN, GL_LOWER_LEFT)
        glEnable(GL_VERTEX_PROGRAM_POINT_SIZE)
        
        // This prevents gl from culling sprites when their center is not visible.
        glPointSize(primitive.pointSize.toFloat)
        
        predefinedUniforms.se_pointSize := primitive.pointSpriteSize//XXX rename se_pointSize to se_pointSpriteSize for consistency.
        
        GL_POINTS
        
      case VertexMode.Lines =>
        glLineWidth(primitive.lineWidth.toFloat)
        GL_LINES
        
      case VertexMode.LineStrip =>
        glLineWidth(primitive.lineWidth.toFloat)
        GL_LINE_STRIP
      
      case VertexMode.LineLoop =>
        glLineWidth(primitive.lineWidth.toFloat)
        GL_LINE_LOOP
        
      case VertexMode.Triangles =>
        GL_TRIANGLES
        
      case VertexMode.TriangleStrip =>
        GL_TRIANGLE_STRIP
        
      case VertexMode.TriangleFan =>
        GL_TRIANGLE_FAN
    }
    
    
    programMapping.bind(mesh.mapping)
    
    
    /* These events must be consumed by the render manager to prevent triggering related updates every frame.
     * These should be consumed by VAO if enabled or cleared manually otherwise.
     */
    val attributes = mesh.geometry.attributes
    i = 0; while (i < attributes.size) {
      attributes(i).clearRefChanges()
      
      i += 1
    }
    
    
    if (geometry.indices.isDefined) {
      renderContext.init(geometry.indices.get)
      glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, geometry.indices.get.managedFields.id)
      glDrawElements(vertexMode, elementRange.count, geometry.indices.get.src.rawType, elementRange.first.toConst)
    }
    else {
      glDrawArrays(vertexMode, elementRange.first, elementRange.count)
    }
  }
  
  // XXX must be updated with new attributes
  private val comparator = new java.util.Comparator[AbstractMesh] {
    import AccessEngine._
    
    def compare(a: AbstractMesh, b: AbstractMesh) :Int = {
      // XXX sort by textures: if (at0id < bt0id) -1 else if (at0id > bt0id)  1 else 0
      // XXX also sort by parent's environment
      
      if (a.technique.isDefined && b.technique.isDefined) {
        val ainfo = getEngineInfo(a.technique.get).asInstanceOf[ProgramInfo]
        val apid = if (ainfo != null) ainfo.managedFields.id else 0
        
        val binfo = getEngineInfo(b.technique.get).asInstanceOf[ProgramInfo]
        val bpid = if (binfo != null) binfo.managedFields.id else 0
        
        if (apid < bpid) -1 else if (apid > bpid)  1 else 0
      }
      else {
        0
      }
    }
  }
  
  def sortRenderArray(pass: Pass, renderArray: SortBuffer[AbstractMesh]) {
    renderArray.inplaceSort(comparator)
  }
}
