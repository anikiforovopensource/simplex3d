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

import simplex3d.engine.bounding._
import simplex3d.engine.scene._
import simplex3d.engine.transformation._
import simplex3d.engine.graphics._


final class Node(implicit transformationContext: TransformationContext, graphicsContext: GraphicsContext)
extends Entity {
  
  override def environment = super.environment
  
  override def parent = super.parent
  override def children = super.children
  
  override def appendChild(elem: SceneElement) { super.appendChild(elem) }
  override def removeChild(elem: SceneElement) = super.removeChild(elem)
  override def removeNestedChild(elem: SceneElement) = super.removeNestedChild(elem)
}
