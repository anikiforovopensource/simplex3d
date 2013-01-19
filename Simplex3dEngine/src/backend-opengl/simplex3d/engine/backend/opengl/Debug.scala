/*
 * Simplex3dEngine - GL Module
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
package backend.opengl

import scala.collection.mutable.ArrayBuffer
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.engine.scene._
import simplex3d.engine.graphics._
import simplex3d.engine.util._


object DebugGraphics {
  
  class Geometry extends prototype.Geometry {
    final val colors = AttributeBinding[Vec3, UByte]
    
    init(classOf[Geometry])
  }
  
  object GraphicsContext extends graphics.GraphicsContext {
    type Geometry = DebugGraphics.Geometry
    type Material = MinimalGraphicsContext.Material
    type Environment = MinimalGraphicsContext.Environment
    
    def mkGeometry() = new Geometry
    def mkMaterial(controllerContext: ControllerContext) = MinimalGraphicsContext.mkMaterial(controllerContext)
    def mkEnvironment(controllerContext: ControllerContext) = MinimalGraphicsContext.mkEnvironment(controllerContext)
    
    init()
  }
  
  class Mesh(val name: String) extends scene.AbstractMesh {
    val geometry: Geometry = GraphicsContext.mkGeometry()
    val material: Material = GraphicsContext.mkMaterial(null)
    val worldEnvironment: Environment = GraphicsContext.mkEnvironment(null)
    protected def worldMatrix: ReadMat4x3 = Mat4x3.Identity
    protected def debugBoundingVolume = null
    
    import AccessScene._
    AccessScene.meshSubtext(this).technique := LineProgram//XXX must work with implicit
  }
  
  private val LineProgram = {//XXX somehow make this Profile-dependent
    val vertexShader = """
      uniform mat4 se_modelViewProjectionMatrix;
      attribute vec3 vertices;
      attribute vec3 colors;
      
      varying vec3 color;
      
      void main() {
        gl_Position = se_modelViewProjectionMatrix*vec4(vertices, 1.0);
        color = colors;
      }
    """
    
    val fragmentShader = """
      varying vec3 color;
      
      void main() {
        gl_FragColor = vec4(color, 1.0);
      }
    """
    
    val shaders = Set(
      new Shader(Shader.Vertex, vertexShader),
      new Shader(Shader.Fragment, fragmentShader)
    )
    
    new Technique(GraphicsContext, shaders)
  }
}


class DebugLinesManager(val name: String, val indicesPerMesh: Int = 65536, val verticesPerMesh: Int = 65536) {
  private val meshes = new ArrayBuffer[DebugGraphics.Mesh](1)
  newMesh()
  
  private var currentMesh = 0
  private var currentIndex = 0
  private var currentVertex = 0
  
  def clear() {
    currentMesh = 0
    currentIndex = 0
    currentVertex = 0
    meshes(0).elementRange.update.count := 0
  }
  
  private def newMesh() {
    val mesh = new DebugGraphics.Mesh(name + " Debug " + (currentMesh + 1))
    
    mesh.geometry.indices := Attributes.fromData(IndexBuffer(verticesPerMesh - 1, indicesPerMesh))
    mesh.geometry.vertices := Attributes[Vec3, RFloat](verticesPerMesh)
    mesh.geometry.colors := Attributes[Vec3, UByte](verticesPerMesh)
    
    mesh.geometry.primitive.update.mode := VertexMode.Lines
    mesh.geometry.primitive.update.lineWidth := 2
    
    meshes += mesh
  }
  
  def append(
    indices: inIndex, indexCount: Int,
    vertices: inDataSeq[Vec3, Raw], vertexCount: Int,
    colors: inDataSeq[Vec3, Raw]
  ) {
    require(indexCount < indicesPerMesh && vertexCount < verticesPerMesh)
    
    def nextMesh() {
      currentMesh += 1
      currentIndex = 0
      currentVertex = 0
      
      if (currentMesh >= meshes.size) newMesh()
    }
    
    if (currentIndex + indexCount > indicesPerMesh || currentVertex + vertexCount > verticesPerMesh) nextMesh()
    
    val mesh = meshes(currentMesh)
    
    val destIndices = mesh.geometry.indices.write(0, currentIndex + indexCount)
    var i = 0; while (i < indexCount) {
      destIndices(currentIndex + i) = indices(i) + currentVertex
      i += 1
    }
    mesh.geometry.vertices.write(0, currentVertex + vertexCount).put(currentVertex, vertices, 0, vertexCount)
    mesh.geometry.colors.write(0, currentVertex + vertexCount).put(currentVertex, colors, 0, vertexCount)
    
    currentIndex += indexCount
    currentVertex += vertexCount
    
    mesh.elementRange.update.count := currentIndex
  }
  
  def buildRenderArray(renderArray: ArrayBuffer[AbstractMesh]) {
    var i = 0; while (i <= currentMesh) {
      renderArray += meshes(i)
      
      i += 1
    }
  }
}


object DebugBounding {
  private val indices = DataArray[SInt, UByte](
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
  private val vertices = DataArray[Vec3, RFloat](
    Vec3(0, 0, 0),
    Vec3(1, 0, 0),
    Vec3(1, 1, 0),
    Vec3(0, 1, 0),
    Vec3(0, 0, 1),
    Vec3(1, 0, 1),
    Vec3(1, 1, 1),
    Vec3(0, 1, 1)
  )
  
  private val colors = DataArray[Vec3, UByte](vertices.size)
  private val transformedVertices = DataArray[Vec3, RFloat](vertices.size)
  
  def appendBox(debugManager: DebugLinesManager)(color: inVec3, min: inVec3, max: inVec3, transformation: inMat4x3) {
    val t = Mat4x3 scale(max - min) translate(min) concat(transformation)
    
    var i = 0; while (i < vertices.size) {
      transformedVertices(i) = t.transformPoint(vertices(i))
      colors(i) = color
      
      i += 1
    }
    
    debugManager.append(indices, indices.size, transformedVertices, transformedVertices.size, colors)
  }
}


object DebugNormals {
  
  private val tbnBatchSize = 2000
  private val lineIndexSize = 6*tbnBatchSize
  private val lineVertexSize = 4*tbnBatchSize; assert(lineVertexSize < 65536)
  
  private val lineIndices = DataArray[SInt, UShort](lineIndexSize)
  private val lineVertices = DataArray[Vec3, RFloat](lineVertexSize)
  private val lineColors = DataArray[Vec3, UByte](lineVertices.size)
  
  val vertexColor = ConstVec3(1)
  val normalColor = ConstVec3(0, 0, 1)
  val tangentColor = ConstVec3(1, 0, 0)
  val binormalColor = ConstVec3(0, 1, 0)
  
  val normalLengthRatio = 0.05
  
  def append(debugManager: DebugLinesManager)(mesh: AbstractMesh, objectSize: Double) {
    if (!mesh.geometry.vertices.isAccessible) return
    if (!mesh.geometry.normals.isAccessible) return
    
    val vertices = mesh.geometry.vertices.read
    if (vertices.size == 0) return
    
    val normals = mesh.geometry.normals.read
    
    
    import AccessScene._
    val transformation = mesh.worldMatrix
    val normalMatrix = normalMat(transformation)
    
    val objectScale = length(transformation.transformVector(Vec3.One))/length(Vec3.One)
    val normalScale = objectScale*objectScale*objectSize*normalLengthRatio
    
    
    var currentLineIndex = 0
    var currentLineVertex = 0
    
    def flush() {
      debugManager.append(lineIndices, currentLineIndex, lineVertices, currentLineVertex, lineColors)
      currentLineIndex = 0
      currentLineVertex = 0
    }
    
    def appendNormal(vertex: inVec3, normal: inVec3) {
      if (currentLineIndex + 2 > lineIndices.size || currentLineVertex + 2 > lineVertices.size) flush()
      
      val vertex1 = transformation.transformPoint(vertex)
      val normal1 = normalMatrix*normal*normalScale
      
      lineIndices(currentLineIndex) = currentLineVertex
      lineIndices(currentLineIndex + 1) = currentLineVertex + 1
      
      lineVertices(currentLineVertex) = vertex1
      lineVertices(currentLineVertex + 1) = vertex1 + normal1
      
      lineColors(currentLineVertex) = vertexColor
      lineColors(currentLineVertex + 1) = normalColor
      
      currentLineIndex += 2
      currentLineVertex += 2
    }
    
    
    if (mesh.geometry.indices.isAccessible) {
      def buildWithIndex() {
        val indices = mesh. geometry.indices.get.read
        
        var first = 0
        var count = indices.size
        if (mesh.elementRange.isDefined) {
          first = mesh.elementRange.get.first
          count = mesh.elementRange.get.count
        }
        
        var i = first; while (i < count) {
          val index = indices(i)
          val vertex = vertices(index)
          val normal = normals(index)
          
          appendNormal(vertex, normal)
          
          i += 1
        }
        
        flush()
      }; buildWithIndex()
    }
    else {
      def buildNoIndex() {
        var first = 0
        var count = vertices.size
        if (mesh.elementRange.isDefined) {
          first = mesh.elementRange.get.first
          count = mesh.elementRange.get.count
        }
        
        var i = first; while (i < count) {
          val vertex = vertices(i)
          val normal = normals(i)
          
          appendNormal(vertex, normal)
          
          i += 1
        }
        
        flush()
      }; buildNoIndex()
    }
  }
}
