/*
 * Simplex3dData - Core Module
 * Copyright (C) 2012, Aleksey Nikiforov
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

import java.util.Arrays


/**
 * @author Aleksey Nikiforov (lex)
 */
final class SortContext(initCapacity: Int = 32) {
  private[this] var order = new Array[Long](initCapacity)
  
  private[this] def resize(required: Int) {
    val newSize = math.max(required, order.length*2)
    val newOrder = new Array[Long](newSize)
    System.arraycopy(order, 0, newOrder, 0, order.length)
    order = newOrder
  }
  
  def mapIndex(i: Int, f: Float) {
    if (i > order.length) resize(i)
      
    val bits = java.lang.Float.floatToRawIntBits(f)
    val lead =
      if (bits < 0) { -bits | 0x80000000 }
      else bits

    order(i) = (lead.toLong << 32) | i
  }
  
  def sort(count: Int) { Arrays.sort(order, 0, count) }
  
  private[data] def capacity = order.length
  private[data] def index(i: Int) = order(i).toInt
}
