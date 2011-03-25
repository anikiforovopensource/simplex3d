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
final class BooleanRef(private[this] var x: Boolean) extends PrimitiveRef[Boolean] {
  type Clone = BooleanRef
  type Read = Boolean
  type Const = Boolean

  def components = 1
  def apply(i: Int) :Boolean = {
    if (i == 0) x
    else throw new IndexOutOfBoundsException("Expected from 0 to 0, got " + i + ".")
  }

  def :=(s: Boolean) { x = s }
  def :=(v: BooleanRef) { x = v.toConst }
  def toConst() :Boolean = x
  override def clone() = new BooleanRef(x)

  private[math] def bx: Boolean = x
  private[math] def ix: Int = Int(x)
  private[math] def fx: Float = Float(x)
  private[math] def dx: Double = Double(x)


  final override def equals(other: Any) :Boolean = {
    other match {
      case r: BooleanRef => x == r.toConst
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
