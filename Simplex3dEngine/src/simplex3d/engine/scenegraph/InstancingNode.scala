/*
 * Simplex3dEngine - SceneGraph Module
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
package scenegraph

import scala.collection.mutable.ArrayBuffer
import simplex3d.math.types._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.engine.bounding._
import simplex3d.engine.transformation._
import simplex3d.engine.graphics._
import simplex3d.engine.scene._


final class InstancingNode[T <: TransformationContext, G <: GraphicsContext](
  val cullingEnabled: Boolean = true
)(implicit transformationContext: T, graphicsContext: G)
extends Entity[T, G] {
  
  import SubtextAccess._
  
  private final class BoundedInstance(implicit transformationContext: T) extends Bounded[T] {
    private[scenegraph] def updateAutoBound() :Boolean = boundingUpdated
  }
  
  private final class Instance(implicit transformationContext: T) extends SceneElement[T]
  
  
  private val srcMesh = new Mesh
  final val instanceBoundingVolume = srcMesh.customBoundingVolume
  final def geometry: G#Geometry = srcMesh.geometry
  final def material: G#Material = srcMesh.material
  override def environment = super.environment
  
  private val displayMesh = new Mesh(this, graphicsContext.mkGeometry(), material)
  private val localRenderArray = new ArrayBuffer[SceneElement[T]](16)
  
  private val indexVertices = displayMesh.geometry.attributeNames.indexWhere(_ == "vertices")
  private val indexNormals = displayMesh.geometry.attributeNames.indexWhere(_ == "normals")
  
  private var rebuild = true
  private var srcVerticesSize = 0
  private var srcIndicesSize = 0
  
  private var boundingUpdated = true
  
  
  private def rebuildAttributes() {
    val childrenCount = children.length
    srcVerticesSize = geometry.vertices.read.size
    val destVertexSize = childrenCount*srcVerticesSize
    
    def copyAttributes(dest: DataView[Format with MathType, Raw], src: inDataView[Format with MathType, Raw]) {
      
      var i = 0; while (i < childrenCount) {
        
        dest.put(i*srcVerticesSize, src)
        
        i += 1
      }
    }
    
    val srcIndex = geometry.indices.read
    if (srcIndex != null) {
      srcIndicesSize = srcIndex.size
      val destIndexSize = childrenCount*srcIndex.size
      displayMesh.geometry.indices.defineAs(Attributes(DataBuffer[SInt, UInt](destIndexSize)))
    }
    else {
      srcIndicesSize = 0
    }
    
    var i = 0; while (i < geometry.attributes.length) {
      
      val srcAttribs = geometry.attributes(i).read
      if (srcAttribs != null) {
        val destAttribs = srcAttribs.mkDataBuffer(destVertexSize)
        displayMesh.geometry.attributes(i).defineAs(Attributes(destAttribs))
        if (i != indexVertices && i != indexNormals) {
          copyAttributes(destAttribs, srcAttribs)
        }
      }
      
      i += 1
    }
    
    rebuild = false
  }
  
  private def rebuildBounding() {
    boundingUpdated = srcMesh.updateAutoBound()
    
    if (boundingUpdated) {
      if (srcMesh.customBoundingVolume.isDefined) {
        val size = children.size
        var i = 0; while (i < size) {
          val instance = children(i).asInstanceOf[BoundedInstance]
          instance.customBoundingVolume.defineAs(srcMesh.customBoundingVolume.defined)
          instance.autoBoundingVolume = null
          
          i += 1
        }
      }
      else {
        val size = children.size
        var i = 0; while (i < size) {
          val instance = children(i).asInstanceOf[BoundedInstance]
          instance.customBoundingVolume.undefine()
          instance.autoBoundingVolume = srcMesh.autoBoundingVolume
          
          i += 1
        }
      }
    }
      
    srcMesh.geometry.indices.clearRefChanges()
    if (srcMesh.geometry.indices.isDefined) srcMesh.geometry.indices.defined.sharedState.clearDataChanges()
    
    srcMesh.geometry.vertices.clearRefChanges()
    if (srcMesh.geometry.vertices.isDefined) srcMesh.geometry.vertices.defined.sharedState.clearDataChanges()
  }
  
  def appendInstance() :Spatial[T] = {
    val instance = if (cullingEnabled) new BoundedInstance else new Instance
    appendChild(instance)
    rebuild = true
    instance
  }
  
  def removeInstance(instance: Spatial[T]) :Boolean = {
    val removed = instance match {
      case e: SceneElement[_] => removeChild(e)
      case _ => false
    }
    if (removed) rebuild = true
    removed
  }
  
  
  private[scenegraph] override def conditionalCull(
    cullSelf: Boolean,
    version: Long, time: TimeStamp, view: View, renderArray: ArrayBuffer[SceneElement[T]]
  ) {
    if (geometry.vertices.read == null) return
    
    
    if (cullingEnabled) {
      if (rebuild || srcMesh.geometry.hasShapeChanges()) {
        rebuildBounding()
      }
    }
    
    
    val srcIndices = geometry.indices.read
    val srcVertices = geometry.vertices.read
    
    if (srcIndices != null) rebuild = rebuild || (srcIndices.size != srcIndicesSize)
    rebuild = rebuild || (srcVertices.size != srcVerticesSize)
    
    if (rebuild) rebuildAttributes()
    
    
    localRenderArray.clear()
    super.conditionalCull(cullSelf, version, time, view, localRenderArray)
    boundingUpdated = false

    val instanceArray = if (cullingEnabled) localRenderArray else children
    
    val srcNormals = geometry.normals.read
    val destIndices = displayMesh.geometry.indices.write(0, instanceArray.size*srcIndicesSize)
    val destVertices = displayMesh.geometry.vertices.write(0, instanceArray.size*srcVerticesSize)
    val destNormals = displayMesh.geometry.normals.write(0, instanceArray.size*srcVerticesSize)
    
    
    (0 until instanceArray.size).par.foreach(i => processChild(i, instanceArray(i)))
    
    def processChild(childIndex: Int, child: SceneElement[T]) {
      if (!cullingEnabled) child.updateWorldTransformation()
      
      val vertexOffset = childIndex*srcVerticesSize
      val indexOffset = childIndex*srcIndicesSize
      
      def transformData(transformation: inMat3x4, normalMatrix: inMat3) {
        var i = 0; while (i < srcVertices.size) {
          
          destVertices(vertexOffset + i) = transformation.transformPoint(srcVertices(i))
          if (srcNormals != null) destNormals(vertexOffset + i) = normalMatrix*srcNormals(i)
          
          i += 1
        }
      }
      def copyIndex() {
        var i = 0; while (i < srcIndices.size) {
          
          destIndices(indexOffset + i) = srcIndices(i) + vertexOffset
          
          i += 1
        }
      }
      
      val transformation = child.uncheckedWorldTransformation.matrix
      val normalMatrix = if (srcNormals != null) transpose(inverse(Mat3(transformation))) else null
      transformData(transformation, normalMatrix)
      
      if (srcIndices != null) {
        copyIndex()
      }
    }
    
    displayMesh.elementRange.mutable.first := 0
    displayMesh.elementRange.mutable.count := instanceArray.size*srcIndicesSize
    
    renderArray += displayMesh
  }
}
