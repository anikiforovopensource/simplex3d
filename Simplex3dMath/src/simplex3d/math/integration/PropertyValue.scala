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

package simplex3d.math.integration


/** <code>PropertyValue</code> is used to integrate with properties.
 *
 * @author Aleksey Nikiforov (lex)
 */
private[simplex3d] sealed trait PropertyValue[
  @specialized(Boolean, Int, Float, Double) T
] {
  def copyAsMutable() :MutableInterface[T] with Mutable
  def copyAsImmutable() :T
}

private[simplex3d] trait PropertyObject[T] extends PropertyValue[T]


/** <code>MutableInterface</code> is used to integrate with properties.
 * It allows uniform treatment for all the objects with := operator.
 *
 * @author Aleksey Nikiforov (lex)
 */
private[simplex3d] trait MutableInterface[
  @specialized(Boolean, Int, Float, Double) T
] extends PropertyValue[T] {
  private[math] def asReadInstance() :T
  private[math] def :=(value: T) :Unit
}

/** <code>MutableObject</code> is used to integrate objects with properties.
 *
 * @author Aleksey Nikiforov (lex)
 */
private[simplex3d] trait MutableObject[T <: AnyRef]
extends MutableInterface[T] with Mutable
{
  //def asReadInstance() :T = copyAsImmutable() //Safer but slower.
  final override def asReadInstance() :T = this.asInstanceOf[T]
}
