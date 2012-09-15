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
import simplex3d.data._
import simplex3d.engine.util._


sealed abstract class AttributeBinding[F <: Format with MathType, R <: Raw](
  implicit private val context: PropertyContext
)
{ self: AccessibleAttributeBinding[F, R] =>
  
  def isAccessible = (isDefined && this.get.isAccessible)
  def isWritable = (isDefined && this.get.isWritable)
  
  def read: ReadDataView[F, R] = this.get.read
  def write: DataView[F, R] = this.get.write
  def write(first: Int, count: Int): DataView[F, R] = this.get.write(first, count)
  def src: DirectSrc = this.get.src
  
  
  protected final var dataChanges = true
  
  private final var attributes: Attributes[F, R] = _
  protected final var reassigned = true // Initialize as reassigned.
  
  final def get: Attributes[F, R] = if (attributes == null) throw new NoSuchElementException else attributes
  final def isDefined = (attributes != null)
  final def undefine() {
    if (attributes != null) {
      if (attributes.isWritable) attributes.unregister(this)
      if (context != null) context.signalStructuralChanges()
    }
    attributes = null.asInstanceOf[Attributes[F, R]]
  }
  
  final def :=(attributes: Attributes[F, R]) {
    if (attributes == null) throw new NullPointerException
    
    if (!isDefined && (context != null)) context.signalStructuralChanges()
    
    if (this.attributes ne attributes) {
      if (isWritable)  this.attributes.unregister(this)
      if (attributes.isWritable)  attributes.register(this)
      reassigned = true
    }
    this.attributes = attributes
  }
  
  final def :=(r: AttributeBinding[F, R]) {
    if (r.isDefined) this := r.get else undefine()
  }
  
  final override def toString() :String =
    "AttributeBinding(" + (if (isDefined) get.toString else "undefined" ) + ")(refChanges = " + hasRefChanges + ")"
}

sealed class AccessibleAttributeBinding[F <: Format with MathType, R <: Raw](
  implicit context: PropertyContext
)
extends AttributeBinding[F, R] {
  import AccessChanges._
  
  def signalDataChanges() { dataChanges = true }
  def hasDataChanges: Boolean = dataChanges
  def clearDataChanges() { dataChanges = false }
  
  def hasRefChanges = reassigned
  def clearRefChanges() { reassigned = false }
  def hasChanges = (hasRefChanges || hasDataChanges)
}

object AttributeBinding {
  def apply[F <: Format with MathType, R <: Raw](implicit context: PropertyContext)
  :AttributeBinding[F, R] = new AccessibleAttributeBinding[F, R]
}
