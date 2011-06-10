/*
 * Simplex3d, CoreData module
 * Copyright (C) 2011, Aleksey Nikiforov
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

import scala.collection._


/**
 * @author Aleksey Nikiforov (lex)
 */
trait ReadBatch[@specialized(Int, Float, Double) +E] extends DataSource
with IndexedSeq[E] with IndexedSeqOptimized[E, IndexedSeq[E]] {
  type Read <: ReadBatch[E]
  
  def apply(i: Int) :E
}

trait Batch[@specialized(Int, Float, Double) E] extends ReadBatch[E] {
  def update(i: Int, v: E)
  
  def put(index: Int, src: Seq[E], first: Int, count: Int) :Unit
  def put(index: Int, src: Seq[E]) :Unit
  def put(src: Seq[E]) :Unit
}
