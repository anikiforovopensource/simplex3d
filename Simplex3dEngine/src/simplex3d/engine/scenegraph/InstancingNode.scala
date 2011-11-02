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

import simplex3d.math.types._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.algorithm.intersection.Collision
import simplex3d.engine.bounding._
import simplex3d.engine.transformation._
import simplex3d.engine.graphics._
import simplex3d.engine.scene._


// XXX limit based on MAX_VBO_SIZE
final class InstancingNode[T <: TransformationContext, G <: GraphicsContext] (
  name: String,
  val cullingEnabled: Boolean = true
)(implicit transformationContext: T, graphicsContext: G)
extends Entity[T, G](name) {
  
  import ClearChangesAccess._
  
  private final class BoundedInstance(name: String)(
    implicit transformationContext: T, graphicsContext: G
  ) extends Bounded[T, G](name) {
    private[scenegraph] override def update(version: Long) :Boolean = {
      if (updateVersion != version) {
        propagateWorldTransformation()
        
        updateVersion = version
        uncheckedWorldTransformation.clearDataChanges()
      }
      boundingUpdated
    }
    
    private[scenegraph] override def updateCull(
      version: Long, enableCulling: Boolean, time: TimeStamp, view: View, renderArray: SortBuffer[AbstractMesh]
    ) {
      throw new UnsupportedOperationException
    }
    
    private[scenegraph] def instanceUpdateCull(
      version: Long, enableCulling: Boolean, time: TimeStamp, view: View, renderArray: SortBuffer[SceneElement[T, G]]
    ) {
      if (enableCulling) update(version)
      else updateWorldTransformation(version)
      
      val res =
        if (enableCulling) BoundingVolume.intersect(view.frustum, resolveBoundingVolume, uncheckedWorldTransformation)
        else Collision.Inside
        
      if (res == Collision.Outside) return
      
      def process() {
        if (animators != null && shouldRunAnimators) {
          runUpdaters(animators, time)
          shouldRunAnimators = false
        }
        
        renderArray += this
      }; process()
    }
  }
  
  private final class Instance(name: String)(
    implicit transformationContext: T, graphicsContext: G
  ) extends SceneElement[T, G](name)
  
  
  private val srcMesh = new Mesh(name + " - Source Mesh")
  srcMesh.uncheckedWorldTransformation.clearDataChanges()
  
  final val instanceBoundingVolume = srcMesh.customBoundingVolume
  final def geometry: G#Geometry = srcMesh.geometry
  final def material: G#Material = srcMesh.material
  override def environment = super.environment
  
  private val displayMesh = new Mesh(name + " - Display Mesh", this, graphicsContext.mkGeometry(), material)
  private val localRenderArray = new ConcurrentSortBuffer[SceneElement[T, G]]
  
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
  }
  
  private def rebuildBounding() {
    boundingUpdated = srcMesh.update(srcMesh.updateVersion + 1)
    
    if (boundingUpdated) {
      if (srcMesh.customBoundingVolume.isDefined) {
        def process() {
          val size = children.size; var i = 0; while (i < size) {
            val instance = children(i).asInstanceOf[BoundedInstance]
            instance.customBoundingVolume.defineAs(srcMesh.customBoundingVolume.defined)
            instance.autoBoundingVolume = null
            
            i += 1
          }
        }; process()
      }
      else {
        def process() {
          val size = children.size; var i = 0; while (i < size) {
            val instance = children(i).asInstanceOf[BoundedInstance]
            instance.customBoundingVolume.undefine()
            instance.autoBoundingVolume = srcMesh.autoBoundingVolume
            
            i += 1
          }
        }; process()
      }
    }
    
    srcMesh.geometry.indices.clearRefChanges()
    if (srcMesh.geometry.indices.isDefined) srcMesh.geometry.indices.defined.sharedState.clearDataChanges()
    
    srcMesh.geometry.vertices.clearRefChanges()
    if (srcMesh.geometry.vertices.isDefined) srcMesh.geometry.vertices.defined.sharedState.clearDataChanges()
  }
  
  def appendInstance(name: String) :Spatial[T] = {
    val instance = if (cullingEnabled) new BoundedInstance(name) else new Instance(name)
    appendChild(instance)
    rebuild = true
    instance
  }
  
  def removeInstance(instance: Spatial[T]) :Boolean = {
    val removed = instance match {
      case e: SceneElement[_, _] => removeChild(e)
      case _ => false
    }
    if (removed) rebuild = true
    removed
  }
  
  private[this] final def instancingCull(
    enableCulling: Boolean,
    version: Long, time: TimeStamp, view: View, renderArray: SortBuffer[SceneElement[T, G]]
  )(allowMultithreading: Boolean, minChildren: Int) {
    
    if (enableCulling) entityUpdate(version)(allowMultithreading, minChildren)
    else updateWorldTransformation(version)
    
    
    val frustumTest = 
      if (!enableCulling) Collision.Inside
      else BoundingVolume.intersect(view.frustum, resolveBoundingVolume, uncheckedWorldTransformation)
    
    val cullChildren =
      if (frustumTest == Collision.Outside) return
      else if (frustumTest == Collision.Inside) false
      else true
    
    if (animators != null && shouldRunAnimators) {
      runUpdaters(animators, time)
      shouldRunAnimators = false
    }
    
    
    var multithreaded = (allowMultithreading && this.children.size >= minChildren)
    
    def processChild(child: SceneElement[T, G]) {
      child match {
        case bounded: Bounded[_, _] =>
          bounded.asInstanceOf[BoundedInstance].instanceUpdateCull(version, cullChildren, time, view, renderArray)
        case _ =>
          child.update(version)
      }
    }
    
    if (multithreaded) {
      val children = this.children
      (0 until children.size).par.foreach(i => processChild(children(i)))
    }
    else {
      val size = children.size; var i = 0; while (i < size) {
        processChild(children(i))
        i += 1
      }
    }
  }
      
  private[scenegraph] override def entityUpdateCull(
    version: Long, enableCulling: Boolean, time: TimeStamp, view: View, renderArray: SortBuffer[AbstractMesh]
  )(
    allowMultithreading: Boolean, minChildren: Int,
    batchArray: SortBuffer[SceneElement[T, G]], maxDepth: Int, currentDepth: Int
  ) {
    if (geometry.vertices.read == null) return
    
    
    if (cullingEnabled) {
      if (rebuild || srcMesh.geometry.hasShapeChanges()) {
        rebuildBounding()
      }
    }
    
    
    val srcIndices = geometry.indices.read
    val srcVertices = geometry.vertices.read
    
    var geometryChanges = false
    if (srcIndices != null) geometryChanges = geometryChanges || (srcIndices.size != srcIndicesSize)
    geometryChanges = geometryChanges || (srcVertices.size != srcVerticesSize)
    // XXX rebuild on changes to other non-vertex and non-normal attributes
    
    if (rebuild || geometryChanges) {
      rebuildAttributes()
      rebuild = false
    }
    
    displayMesh.geometry.copyNonattributes(geometry)
    
    
    localRenderArray.clear()
    instancingCull(enableCulling, version, time, view, localRenderArray)(allowMultithreading, minChildren)
    
    boundingUpdated = false
    
    val instanceArray = if (cullingEnabled) { localRenderArray } else children
    
    val srcNormals = geometry.normals.read
    val destIndices = displayMesh.geometry.indices.write(0, instanceArray.size*srcIndicesSize)
    val destVertices = displayMesh.geometry.vertices.write(0, instanceArray.size*srcVerticesSize)
    val destNormals = displayMesh.geometry.normals.write(0, instanceArray.size*srcVerticesSize)
    
    def processChild(childIndex: Int, child: SceneElement[T, G]) {
      
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
    
    if (allowMultithreading) {
      (0 until instanceArray.size).par.foreach(i => processChild(i, instanceArray(i)))
    }
    else {
      val size = instanceArray.size; var i = 0; while (i < size) {
        processChild(i, instanceArray(i))
        i += 1
      }
    }
    
    displayMesh.elementRange.mutable.first := 0
    displayMesh.elementRange.mutable.count := instanceArray.size*srcIndicesSize
    
    renderArray += displayMesh
  }
}
