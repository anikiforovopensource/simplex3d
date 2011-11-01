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
package graphics

import scala.collection._
import simplex3d.math.types._


sealed abstract class ReadBindingArray[W <: Writable[W] with NestedBinding]
extends Readable[BindingArray[W]] with NestedBinding {
  def length: Int
  def apply(i: Int) :W#Read
}


final class BindingArray[W <: Writable[W] with NestedBinding] (private val elementFactory: Readable[W], val size: Int)
extends ReadBindingArray[W] with Writable[BindingArray[W]]
{
  type Read = ReadBindingArray[W]
  
  private[this] val array = {
    val array = new Array[AnyRef](size)
    var i = 0; while (i < array.length) {
      array(i) = elementFactory.mutableCopy()
      i += 1
    }
    array
  }
  
  
  final def mutableCopy(): BindingArray[W] = {
    val copy = new BindingArray[W](elementFactory, size)
    copy := this
    copy
  }
  
  def :=(r: Readable[BindingArray[W]]) {
    val a = r.asInstanceOf[ReadBindingArray[W]]
    var i = 0; while (i < array.length) {
      array(i).asInstanceOf[W] := a(i).asInstanceOf[W]
      i += 1
    }
  }
  
  def length: Int = array.length
  def apply(i: Int) :W = array(i).asInstanceOf[W]
  
  
  override def toString() :String = {
    "BindingArray(" + array.mkString(", ") + ")"
  }
}
