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

import scala.collection.mutable.ArrayBuffer
import simplex3d.algorithm.intersection.{ Frustum, Collision }
import simplex3d.engine.util._
import simplex3d.engine.bounding._
import simplex3d.engine.scene._
import simplex3d.engine.graphics._
import simplex3d.engine.transformation._


abstract class Entity[T <: TransformationContext, G <: GraphicsContext] (name: String)(
  implicit transformationCtx: T, graphicsCtx: G
) extends AbstractNode[T, G](name) with InheritedEnvironment {
  
  protected def appendChild(element: SceneElement[T, G] with InheritedEnvironment) {
    element.asInstanceOf[InheritedEnvironment]
    appendAnyChild(element)
  }
}
