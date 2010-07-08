/*
 * Simplex3d, BaseMath module
 * Copyright (C) 2010, Simplex3d Team
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


trait PropertyValue[T] {
  def copyAsMutable() :MutableValue[T]
}

object PropertyValue {
  implicit def booleanToPropertyValue(v: Boolean) :PropertyValue[Boolean] =
    new MutablePrimitive(v)

  implicit def intToPropertyValue(v: Int) :PropertyValue[Int] =
    new MutablePrimitive(v)

  implicit def floatToPropertyValue(v: Float) :PropertyValue[Float] =
    new MutablePrimitive(v)

  implicit def doubleToPropertyValue(v: Double) :PropertyValue[Double] =
    new MutablePrimitive(v)
}

sealed trait MutableValue[
  @specialized(Boolean, Int, Float, Double) T
] extends Mutable {
  private[math] def asReadInstance() :T
  private[math] def :=(value: T) :Unit
}

/** <code>ObjectValue</code> is a trait for all mutable math objects.
 * It allows uniform treatment for all the objects with := operator.
 *
 * @author Aleksey Nikiforov (lex)
 */
trait MutableObject[T] extends MutableValue[T] {
  private[math] def asReadInstance() :T = this.asInstanceOf[T]
}

final class MutablePrimitive[
  @specialized(Boolean, Int, Float, Double) T <: AnyVal
](private var value: T) extends PropertyValue[T] with MutableValue[T]
{
  def copyAsMutable() :MutableValue[T] = new MutablePrimitive[T](value)

  override def asReadInstance() :T = value
  def :=(value: T) { this.value = value }

  override def toString() = {
    value.asInstanceOf[AnyRef].getClass.getSimpleName +
    "(" + value.toString + ")"
  }
}
