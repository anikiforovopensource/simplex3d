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


sealed abstract class ReadComponentTransformation3d(protected val camera: Boolean) extends ReadTransformation {
  import AccessChanges._
  
  type Read = ReadComponentTransformation3d
  type Mutable = ComponentTransformation3d
  final def readType = classOf[ReadComponentTransformation3d]
  
  def scale: ReadDoubleRef
  def rotation: ReadQuat4
  def translation: ReadVec3
  
  
  final def mutableCopy() = {
    val copy = new ComponentTransformation3d(camera)
    if (isSet) copy := this
    copy
  }
  
  final def propagateChanges(parent: ReadComponentTransformation3d, result: ComponentTransformation3d) {
    val parentChanged = if (parent != null) parent.hasDataChanges else false
    
    if (parentChanged || this.hasDataChanges) {
      if (parent != null && parent.isSet) {
        if (this.isSet) {
          val res = result.update
          res.scale := this.scale * parent.scale
          res.rotation := this.rotation rotate parent.rotation
          res.translation := parent.rotation.rotateVector(this.translation*parent.scale) + parent.translation
        }
        else {
          result := parent
        }
      }
      else {
        if (this.isSet)
          result := this
        else
          result.unset()
      }
    }
  }
}

final class ComponentTransformation3d(camera: Boolean)
extends ReadComponentTransformation3d(camera) with Transformation
{
  final class MutableSubtext {
    def scale = _scale
    def rotation = _rotation
    def translation = _translation
    
    def lookAt(point: inVec3, worldUp: inVec3) {
      val dir = if (camera) translation - point else point - translation 
      val rotationMat = functions.lookAt(dir, worldUp)
      rotation := quaternion(rotationMat)
    }
  }
  private val mutableSubtext = new MutableSubtext
  
  private[this] val _scale = new DoubleRef(1.0)
  private[this] val _rotation = Quat4.Identity.mutableCopy()
  private[this] val _translation = Vec3(0)
  
  def scale: ReadDoubleRef = _scale
  def rotation: ReadQuat4 = _rotation
  def translation: ReadVec3 = _translation
  
  private[this] var set = false
  def isSet = set
  def unset() {
    val m = update
    
    m.scale := 1
    m.rotation := Quat4.Identity
    m.translation := Vec3.Zero
    
    set = false
  }
  
  def :=(t: ReadComponentTransformation3d) {
    val m = update
    
    m.scale := t.scale
    m.rotation := t.rotation
    m.translation := t.translation
  }
  
  def update = {
    dataChanges = true
    updateMatrix = true
    set = true
    mutableSubtext
  }
  
  
  private[this] var _matrix: Mat4x3 = _
  private[this] var updateMatrix = true
  
  def matrix :ReadMat4x3 = {
    if (_matrix == null) _matrix = Mat4x3(1)
    
    if (updateMatrix) {
      _matrix := transformation(Vec3(scale), rotationMat(rotation), translation)
      updateMatrix = false
    }
    
    _matrix
  }
  
  
  override def toString() :String = {
    "Transformation3d(scale = " + scale.toConst + ", rotation = " + rotation + ", translation = " + translation + ")"
  }
}
