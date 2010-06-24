/*
 * Simplex3d, BaseBuffer module
 * Copyright (C) 2010, Simplex3d Team
 *
 * This file is part of Simplex3dBuffer.
 *
 * Simplex3dBuffer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dBuffer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.buffer


/**
 * @author Aleksey Nikiforov (lex)
 */
abstract class FactoryRef[E <: MetaElement, R <: RawData] {
  def factory: DataSeq[E, R]
}

class PrimitiveFactoryRef[E <: Primitive, R <: RawData](
  primitiveClass: String
) extends FactoryRef[E, R] {
  lazy val factory: DataSeq[E, R] = {
    Class.forName(primitiveClass).newInstance().asInstanceOf[DataSeq[E, R]]
  }
}

class CompositeFactoryRef[E <: Composite, R <: RawData](
  compositeClass: String,
  primitiveFactoryRef: PrimitiveFactoryRef[E#Component, R]
) extends FactoryRef[E, R] {

  lazy val factory: DataSeq[E, R] = {
    val constructor = {
      val cons = Class.forName(compositeClass).getConstructors()
      if (cons(0).getParameterTypes().length == 1) cons(0)
      else cons(1)
    }

    constructor.newInstance(
      primitiveFactoryRef.factory
    ).asInstanceOf[DataSeq[E, R]]
  }
}
