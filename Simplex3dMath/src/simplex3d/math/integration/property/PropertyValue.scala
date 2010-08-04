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

package simplex3d.math.integration.property


/** <code>PropertyValue</code> is used to integrate with properties.
 * It allows uniform treatment for all the objects with := operator.
 *
 * @author Aleksey Nikiforov (lex)
 */
sealed trait PropertyValue[
  @specialized(Boolean, Int, Float, Double) T
] extends Mutable {
  def cloneValue() :T
  def asReadInstance() :T

  override def clone(): PropertyValue[T] =
    super.clone().asInstanceOf[PropertyValue[T]]

  def :=(value: T) :Unit
}

/** <code>PropertyObject</code> is used to integrate objects with properties.
 *
 * @author Aleksey Nikiforov (lex)
 */
trait PropertyObject[T <: AnyRef] extends PropertyValue[T]


/** <code>PropertyPrimitive</code> is used to integrate primitive types
 * with properties.
 *
 * @author Aleksey Nikiforov (lex)
 */
final class PropertyPrimitive[
  @specialized(Boolean, Int, Float, Double) T <: AnyVal
](private var value: T) extends PropertyValue[T]
{
  override def cloneValue() :T = value
  override def asReadInstance() :T = value
  override def clone() = new PropertyPrimitive[T](value)
  override def :=(value: T) { this.value = value }

  override def toString() = {
    value.asInstanceOf[AnyRef].getClass.getSimpleName +
    "(" + value.toString + ")"
  }
}

object PropertyValue {
  implicit def booleanToPropertyValue(v: Boolean) :PropertyValue[Boolean] =
    new PropertyPrimitive(v)

  implicit def intToPropertyValue(v: Int) :PropertyValue[Int] =
    new PropertyPrimitive(v)

  implicit def floatToPropertyValue(v: Float) :PropertyValue[Float] =
    new PropertyPrimitive(v)

  implicit def doubleToPropertyValue(v: Double) :PropertyValue[Double] =
    new PropertyPrimitive(v)
}
