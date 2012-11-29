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

import java.util.HashMap
import simplex3d.math.types._
import simplex3d.engine.util._


@SerialVersionUID(8104346712419693669L)
sealed abstract class ReadEnumRef[T <: Enumeration] (protected var value0: T#Value)
extends Protected with PropertyContextDependent with Serializable
{
  type Clone <: ReadEnumRef[T]
  final def toConst() :T#Value = value0
  
  type Read = ReadEnumRef[T]
  type Mutable = EnumRef[T]
  final def readType = classOf[ReadEnumRef[T]]
  final def mutableCopy() = new EnumRef[T](value0)

  
  // XXX enable after the next Scala release.
//  final override def equals(other: Any) :Boolean = {
//    other match {
//      case r: ReadEnumRef[_] => value0 == r.toConst
//      case e: AnyRef => value0 eq e
//      case _ => false
//    }
//  }
  final override def equals(other: Any) :Boolean = {
    if (this.value0 eq other.asInstanceOf[AnyRef]) true
    else if (other.isInstanceOf[ReadEnumRef[_]]) value0 == other.asInstanceOf[ReadEnumRef[_]].toConst
    else false
  }
  
  final def collectKeys(path: String, enums: HashMap[String, Object]) {
    enums.put(path, toConst)//XXX warn on existing
  }
  
  final override def hashCode() :Int = value0.hashCode
  final override def toString() :String = "EnumRef" + "(" + value0 + ")"
}

@SerialVersionUID(8104346712419693669L)
final class EnumRef[T <: Enumeration] (value: T#Value) extends ReadEnumRef[T](value)
with Accessible with Serializable
{
  private var context: PropertyContext = _
  private[engine] override def register(context: PropertyContext) { this.context = context }
  private[engine] override def unregister() { context = null }
  protected def registerPropertyContext(context: PropertyContext) {}
  protected def unregisterPropertyContext() {}
  
  
  type Clone = EnumRef[T]
  type Const = T#Value
  
  override def clone() = new EnumRef[T](value)

  def :=(e: T#Value) {
    if (value0 != e) {
      if (context != null) context.signalStructuralChanges()
      value0 = e
    }
  }
  
  def :=(r: ReadEnumRef[T]) { this := r.toConst }
}
