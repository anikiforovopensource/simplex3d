/*
 * Simplex3d, Property module
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

package simplex3d.math.property


/**
 * @author Aleksey Nikiforov (lex)
 */
trait Property[@specialized(Boolean, Int, Float, Double) T] {
  protected def value: MutableValue[T]

  def apply() = this.value.asReadInstance()
  def :=(value: T) { this.value := value }

  final def updateWith(function: (T) => T) {
    this := function(value.asReadInstance())
  }

  override def toString() :String = {
    def cleanClassName() = {
      val name = this.getClass.getSimpleName
      val id = name.indexOf('$')
      
      if (id == 0) "AnonymousProperty"
      else if (id > 0) name.substring(0, id)
      else name
    }
    cleanClassName + "(" + value.toString + ")"
  }
}

trait ChangeMonitor[@specialized(Boolean, Int, Float, Double) T]
extends Property[T]
{
  override def :=(value: T) {
    if (this.value.specializedEquals(value)) {
      this.value := value
      onChange()
    }
  }

  def onChange() {}
}

trait ValueMonitor[@specialized(Boolean, Int, Float, Double) T]
extends Property[T]
{
  override def :=(value: T) {
    if (this.value.specializedEquals(value)) {
      val old = this.value.copyAsImmutable()
      this.value := value
      onChange(old)
    }
  }

  def onChange(old: T) {}
}
