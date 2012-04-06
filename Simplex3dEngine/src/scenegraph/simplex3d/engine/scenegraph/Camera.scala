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

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.engine.util._
import simplex3d.engine.scene._
import simplex3d.engine.graphics._
import simplex3d.engine.transformation._


class Camera[T <: TransformationContext, G <: GraphicsContext](
  name: String,
  val projection: Mat4 = orthoProj(-100, 100, -100, 100, 10, 200)
)(implicit transformationContext: T, graphicsContext: G)
extends SceneElement[T, G](name) with AbstractCamera {
  
  protected val _view = Mat4x3(1)
  protected val _viewProjection = Mat4(1)
  protected val _inverseViewProjection = Mat4(1)
  
  def view: ReadMat4x3 = {
    if (worldTransformation.hasDataChanges) sync()
    _view
  }
  def viewProjection: ReadMat4 = {
    if (worldTransformation.hasDataChanges) sync()
    _viewProjection
  }
  def inverseViewProjection: ReadMat4 = {
    if (worldTransformation.hasDataChanges) sync()
    _inverseViewProjection
  }
  
  private def sync() {
    import ClearChangesAccess._
    
    _view := inverse(worldTransformation.matrix)
    _viewProjection := projection*Mat4(_view)
    _inverseViewProjection := inverse(_viewProjection)
    
    worldTransformation.clearDataChanges()
  }
}
