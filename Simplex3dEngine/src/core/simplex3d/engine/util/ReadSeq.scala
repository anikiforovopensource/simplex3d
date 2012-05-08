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

package simplex3d.engine.util

import scala.collection._
import scala.collection.mutable.ArrayBuffer


final class ReadSeq[+T](seq: ArrayBuffer[T]) extends IndexedSeq[T] with IndexedSeqOptimized[T, IndexedSeq[T]] {
  final override def size = seq.length
  final def length = seq.length
  final def apply(i: Int) :T = seq(i)
  
  override def equals(o: Any) :Boolean = {
    if (this eq o.asInstanceOf[AnyRef]) {
      true
    }
    else o match {
      case d: ReadSeq[_] => this.seq == d.seq
      case _ => false
    }
  }
  
  override def hashCode() :Int = {
    seq.hashCode()
  }
}

object ReadSeq {
  final val Empty = new ReadSeq[Nothing](ArrayBuffer.empty)
}
