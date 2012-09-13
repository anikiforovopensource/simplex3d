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
import simplex3d.engine.graphics._


abstract class ReadTransformation extends Protected {
  type Read <: ReadTransformation
  type Mutable <: Transformation
  
  def direction() :Vec3
  def propagateChanges(parent: Read, result: Mutable)
  
  private[engine] final def matrix() :Mat4x3 = toMatrix()
  protected def toMatrix() :Mat4x3
}


trait Transformation extends ReadTransformation with Accessible {
  
}


object Transformation {
  
  def propagateChanges[T <: Transformation](
    parent: TransformationBinding[T], child: TransformationBinding[T], result: TransformationBinding[T]
  ) {
    import AccessChanges._
    
    val parentChanged = if (parent != null) parent.hasDataChanges else false
    
    if (parentChanged || child.hasDataChanges) {
      if (parent != null && parent.isDefined) {
        if (child.isDefined) {
          val c = child.get
          c.propagateChanges(parent.get.asInstanceOf[c.Read], result.update.asInstanceOf[c.Mutable])
        }
        else {
          result := parent
        }
      }
      else {
        result := child
      }
    }
  }
}