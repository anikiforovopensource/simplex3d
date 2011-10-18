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


sealed abstract class ReadBindingArray[R <: Readable[R] with NestedBinding]
extends Readable[ReadBindingArray[R]] with NestedBinding
{
  type Mutable = BindingArray[R]
  
  def length: Int
  def apply(i: Int) :R
}


final class BindingArray[R <: Readable[R] with NestedBinding] private (private val array :Array[R#Mutable])
extends ReadBindingArray[R] with Writable[ReadBindingArray[R]]
{
  def this(factory: R, size: Int) {
    this {
      val array = new Array[AnyRef](size).asInstanceOf[Array[R#Mutable]]
      var i = 0; while (i < array.length) {
        array(i) = factory.mutableCopy()
        i += 1
      }
      array
    }
  }
  
  
  final def mutableCopy(): BindingArray[R] = {
    val carray = new Array[AnyRef](array.length).asInstanceOf[Array[R#Mutable]]
    var i = 0; while (i < carray.length) {
      carray(i) = array(i).mutableCopy()
      i += 1
    }
    new BindingArray[R](carray)
  }
  
  def :=(a: ReadBindingArray[R]) {
    var i = 0; while (i < array.length) {
      array(i) := a(i)
      i += 1
    }
  }
  
  def length: Int = array.length
  def apply(i: Int) :R#Mutable = array(i)
}
