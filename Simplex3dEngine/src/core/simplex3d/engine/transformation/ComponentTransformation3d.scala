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
import simplex3d.engine.scene._


//XXX this name is too long, find a better one.
sealed abstract class ReadComponentTransformation3d extends ReadTransformation {
  type Read = ReadComponentTransformation3d
  type Mutable = ComponentTransformation3d
  
  final def readType = classOf[ReadComponentTransformation3d]
  
  def scale: ReadDoubleRef
  def rotation: ReadQuat4
  def translation: ReadVec3
  
  
  final def mutableCopy() = {
    val copy = new ComponentTransformation3d
    copy := this
    copy
  }
  
  final def propagateChanges(parent: ReadComponentTransformation3d, result: ComponentTransformation3d) {
    result.scale := this.scale * parent.scale
    result.rotation := this.rotation rotate parent.rotation
    result.translation := parent.rotation.rotateVector(this.translation*parent.scale) + parent.translation
  }
}

final class ComponentTransformation3d
extends ReadComponentTransformation3d with Transformation
{
  val scale = new DoubleRef(1.0)
  val rotation = Quat4.Identity.mutableCopy()
  val translation = Vec3(0)
  
  def direction() :Vec3 = rotation.rotateVector(Vec3.UnitZ)
  
  def lookAt(point: inVec3, worldUp: inVec3, isCamera: Boolean = false) {
    val dir = if (isCamera) translation - point else point - translation 
    val rotationMat = functions.lookAt(dir, worldUp)
    rotation := quaternion(rotationMat)
  }
  
  def :=(t: ReadComponentTransformation3d) {
    scale := t.scale
    rotation := t.rotation
    translation := t.translation
  }
  
  protected[engine] def toMatrix() :Mat4x3 = transformation(Vec3(scale), rotationMat(rotation), translation)
  
  
  override def toString() :String = {
    "Transformation3d(scale = " + scale.toConst + ", rotation = " + rotation + ", translation = " + translation + ")"
  }
}
