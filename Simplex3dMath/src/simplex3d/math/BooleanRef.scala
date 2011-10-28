/*
 * Simplex3dMath - Core Module
 * Copyright (C) 2009-2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dMath.
 *
 * Simplex3dMath is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dMath is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.math

import simplex3d.math.types._


/**
 *
 * @author Aleksey Nikiforov (lex)
 */
@SerialVersionUID(8104346712419693669L)
sealed abstract class ReadBooleanRef(protected var x: Boolean)
extends PrimitiveRef[Boolean] with ReadPropertyRef[ReadBooleanRef] with Serializable
{
  type Const = Boolean
  type Mutable = BooleanRef
  final def toConst() :Boolean = x
  final def mutableCopy() = new BooleanRef(x)

  final def apply(i: Int) :Boolean = {
    if (i == 0) x
    else throw new IndexOutOfBoundsException("Expected from 0 to 0, got " + i + ".")
  }

  private[math] final def bx: Boolean = x
  private[math] final def ix: Int = simplex3d.math.toInt(x)
  private[math] final def fx: Float = simplex3d.math.toFloat(x)
  private[math] final def dx: Double = simplex3d.math.toDouble(x)
  
  
  final def &&(s: Boolean) :Boolean = (x && s)
  final def ||(s: Boolean) :Boolean = (x || s)
  final def ^(s: Boolean) :Boolean = (x ^ s)


  final override def equals(other: Any) :Boolean = {
    other match {
      case r: ReadBooleanRef => x == r.toConst
      case a: Boolean => x == a
      case _ => false
    }
  }
  
  final def ==(s: Boolean) :Boolean = (x == s)
  final def !=(s: Boolean) :Boolean = (x != s)

  final override def hashCode() :Int = {
    x.hashCode
  }

  final override def toString() :String = {
    "BooleanRef" + "(" + x + ")"
  }
}

@SerialVersionUID(8104346712419693669L)
final class BooleanRef(cx: Boolean) extends ReadBooleanRef(cx)
with PropertyRef[ReadBooleanRef] with Cloneable[BooleanRef] with Serializable
{
  override def clone() = new BooleanRef(x)

  def :=(s: Boolean) { x = s }
  def :=(r: ReadBooleanRef) { x = r.toConst }

  
  def &=(s: Boolean) { x &= s }
  def |=(s: Boolean) { x |= s }
  def ^=(s: Boolean) { x ^= s }
}

object BooleanRef {
  def unapply(r: ReadBooleanRef) = Some(r.toConst)
}
