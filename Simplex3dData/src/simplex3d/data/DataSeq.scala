/*
 * Simplex3d, CoreData module
 * Copyright (C) 2010-2011, Simplex3d Team
 *
 * This file is part of Simplex3dData.
 *
 * Simplex3dData is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dData is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.data


/**
 * @author Aleksey Nikiforov (lex)
 */
trait ReadDataSeq[E <: Meta, +R <: Raw]
extends ReadAbstractData[E, E#Const, R]

trait DataSeq[E <: Meta, +R <: Raw]
extends AbstractData[E, E#Const, E#Read, R] with ReadDataSeq[E, R]

object DataSeq {
  def apply[E <: Meta, R <: Defined](
    implicit composition: CompositionFactory[E, _ >: R], primitive: PrimitiveFactory[E#Component, R]
  ) :DataSeq[E, R] = {
    composition.mkDataArray(primitive.mkDataArray(0))
  }
}
