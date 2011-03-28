/*
 * Simplex3d, CoreMath module
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

import simplex3d.math.CommonMath._


/**
 *
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class ReadBooleanRef(protected var x: Boolean) extends PrimitiveRef[Boolean] {
  type Read = ReadBooleanRef
  type Const = Boolean
  def toConst() :Boolean = x

  def components = 1
  def apply(i: Int) :Boolean = {
    if (i == 0) x
    else throw new IndexOutOfBoundsException("Expected from 0 to 0, got " + i + ".")
  }

  private[math] def bx: Boolean = x
  private[math] def ix: Int = Int(x)
  private[math] def fx: Float = Float(x)
  private[math] def dx: Double = Double(x)


  final override def equals(other: Any) :Boolean = {
    other match {
      case r: ReadBooleanRef => x == r.toConst
      case a => x == a
    }
  }

  final override def hashCode() :Int = {
    x.hashCode
  }

  final override def toString() :String = {
    "BooleanRef" + "(" + x + ")"
  }
}

final class BooleanRef(cx: Boolean) extends ReadBooleanRef(cx) with PropertyRef {
  type Clone = BooleanRef
  override def clone() = new BooleanRef(x)

  def :=(s: Boolean) { x = s }
  def :=(r: ReadBooleanRef) { x = r.toConst }

  
  def &=(s: Boolean) { x &= s }
  def |=(s: Boolean) { x |= s }
  def ^=(s: Boolean) { x ^= s }
}

object BooleanRef {
  def unapply(r: ReadBooleanRef) = Some(r.toConst)
  implicit def toMutable(r: ReadBooleanRef) = new BooleanRef(r.toConst)
}
