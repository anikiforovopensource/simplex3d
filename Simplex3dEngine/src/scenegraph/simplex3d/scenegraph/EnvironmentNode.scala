/*
 * Simplex3dEngine - SceneGraph Module
 * Copyright (C) 2011-2012, Aleksey Nikiforov
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

import simplex3d.algorithm.intersection.{ Frustum, Collision }
import simplex3d.engine.util._
import simplex3d.engine.bounding._
import simplex3d.engine.scene._
import simplex3d.engine.transformation._
import simplex3d.engine.graphics._


class EnvrionmentNode[T <: TransformationContext, G <: GraphicsContext](
  name: String, val combineEnvironment: Boolean = true
)(
  implicit transformationContext: T, graphicsContext: G
)
extends AbstractNode[T, G](name) {
  
  import simplex3d.engine.access.AccessChanges._
  
  
  private[this] final val env = graphicsContext.mkEnvironment(this)
  def environment: G#Environment = env
  override private[scenegraph] final val worldEnvironment: G#Environment = graphicsContext.mkEnvironment(null)
  
  
  override def parent = super.parent
  override def children = super.children
  
  def appendChild(element: SceneElement[T, G]) { appendAnyChild(element) }
  override def removeChild(element: SceneElement[_, _]) :Boolean = super.removeChild(element)
  override def removeNestedChild(element: SceneElement[_, _]) :Boolean = super.removeNestedChild(element)
  
  
  private[scenegraph] override def nodeCull(
    updateChildren: Boolean, cullChildren:Boolean, batchChildren: Boolean,
    allowMultithreading: Boolean, currentDepth: Int,
    cullContext: CullContext[T, G]
  ) {
    
    if (parent == null) { 
      def propagateRoot() { // special treatment for the root node
        val rootProps = environment.properties
        val resultProps = worldEnvironment.properties
        
        val size = rootProps.length; var i = 0; while (i < size) {
          
          val rootProp = rootProps(i)
          val resultProp = resultProps(i)
          
          if (rootProp.hasDataChanges) {
            resultProp := rootProp
            rootProp.clearDataChanges()
          }
          
          i += 1
        }
      }; propagateRoot()
    }
    
    val children = this.children
    val size = children.size; var i = 0; while (i < size) { val current = children(i)
      
      current match {
        case envNode: EnvrionmentNode[_, _] =>
          
          def propagateEnvironment() {
            val parentProps = worldEnvironment.properties
            val childProps = envNode.environment.properties
            val resultProps = envNode.worldEnvironment.properties
            
            val size = parentProps.length; var i = 0; while (i < size) {
              
              val parentProp = parentProps(i)
              val childProp = childProps(i)
              val resultProp = resultProps(i)
              
              if (parentProp.hasDataChanges || childProp.hasDataChanges) {
                if (parentProp.isDefined) {
                  if (childProp.isDefined) {
                    val c = childProp.get
                    c.propagate(parentProp.get.asInstanceOf[c.Read], resultProp.update.asInstanceOf[c.Mutable])
                  }
                  else {
                    val r = resultProp.update
                    r := parentProp.get.asInstanceOf[r.Read]
                  }
                }
                else {
                  resultProp := childProp
                }

                childProp.clearDataChanges()
              }
              
              i += 1
            }
          }; if (envNode.combineEnvironment) propagateEnvironment()
          
          if (batchChildren) cullContext.batchArray += envNode
          else envNode.cull(updateChildren, cullChildren, allowMultithreading, currentDepth + 1, cullContext)

        case bounded: Bounded[_, _] =>
          if (batchChildren) cullContext.batchArray += bounded
          else bounded.cull(updateChildren, cullChildren, allowMultithreading, currentDepth + 1, cullContext)
        
        case _ =>
          current.updateWorldTransformation()
      }
      
      i += 1
    }
  }
}
