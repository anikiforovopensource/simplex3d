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

import simplex3d.math.types._


@SerialVersionUID(8104346712419693669L)
sealed abstract class ReadEnumRef[T <: Enumeration] (protected var value: T#Value)
extends ReadPropertyValue[EnumRef[T]] with Serializable
{
  final def toConst() :T#Value = value
  final def mutableCopy() = new EnumRef[T](value)

  
  // XXX enable after the next Scala release.
//  final override def equals(other: Any) :Boolean = {
//    other match {
//      case r: ReadEnumRef[_] => value == r.toConst
//      case e: AnyRef => value eq e
//      case _ => false
//    }
//  }
  
  final override def hashCode() :Int = value.hashCode
  final override def toString() :String = "EnumRef" + "(" + value + ")"
}

@SerialVersionUID(8104346712419693669L)
final class EnumRef[T <: Enumeration] (value: T#Value) extends ReadEnumRef[T](value)
with PropertyValue[EnumRef[T]] with Cloneable[EnumRef[T]] with Serializable
{
  type Read = ReadEnumRef[T]
  type Const = T#Value
  
  override def clone() = new EnumRef[T](value)

  def :=(e: T#Value) { value_=(e) }
  def :=(r: Readable[EnumRef[T]]) { value_=(r.asInstanceOf[ReadEnumRef[T]].toConst) }
}
