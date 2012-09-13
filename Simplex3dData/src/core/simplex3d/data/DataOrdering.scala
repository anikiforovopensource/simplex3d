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


/** DataOrdering allows to easily reorder data stored in DataSeq objects.
 * 
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class DataOrdering(initCapacity: Int) {
  protected final var order = new Array[Long](initCapacity)
  
  protected final def resize(required: Int) {
    val newSize = math.max(required, order.length*2)
    val newOrder = new Array[Long](newSize)
    System.arraycopy(order, 0, newOrder, 0, order.length)
    order = newOrder
  }
  
  final def capacity = order.length
  final def indexOf(order: Int) = this.order(order).toInt
}


/** DataMapping class allows to reorder data according to specified mapping.
 * The default mapping maps everything to index 0. Always specify the complete mapping.
 * 
 * @author Aleksey Nikiforov (lex)
 */
final class DataMapping(initCapacity: Int = 32) extends DataOrdering(initCapacity) {
  def map(order: Int, index: Int) {
    if (order >= this.order.length) resize(order)
    
    this.order(order) = index
  }
}


/** DataSort class allows to map indices to floats and then reorder data according to
 * the sorded order of mapped values.
 * Always specify the complete mapping.
 * 
 * @author Aleksey Nikiforov (lex)
 */
final class DataSort(initCapacity: Int = 32)  extends DataOrdering(initCapacity) {
  def map(index: Int, value: Float) {
    if (index >= order.length) resize(index)
      
    val bits = java.lang.Float.floatToRawIntBits(value)
    val lead =
      if (bits < 0) { -bits | 0x80000000 }
      else bits

    order(index) = (lead.toLong << 32) | index
  }
  
  def sort(count: Int) { Arrays.sort(order, 0, count) }
}
