/*
 * Simplex3dEngine - Core Module
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
package transformation

import simplex3d.math.types._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.engine.util._


abstract class ReadTransformation[W <: Transformation[W]] extends DataChangeListener with Readable[W] 
{ self: W#Read =>
  
  def propagateChanges(parent: W#Read, result: W) :Unit
  def matrix :ReadMat4x3
  
  def isSet: Boolean
}

trait Transformation[W <: Transformation[W]] extends ReadTransformation[W] with Writable[W]
{ self: W =>
  type Read >: W <: ReadTransformation[W]
  
  def :=(t: Readable[W]) :Unit
  def unset() :Unit
}
