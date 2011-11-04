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

package simplex3d.engine.common

import java.util._
import scala.collection._
import scala.collection.mutable.ArrayBuffer


class SortBuffer[T] protected(initArray: Array[AnyRef]) extends IndexedSeq[T] {
  def this(initCapacity: Int) { this(new Array[AnyRef](initCapacity)) }
  def this() { this(16) }
  
  private var _size0 = 0
  protected def size0: Int = _size0
  protected def size0_=(i: Int) { _size0 = i }
  
  private var _array = initArray
  protected[engine] def array: Array[AnyRef] = _array
  protected[engine] def array_=(a: Array[AnyRef]) { _array = a }
  
  def length = size0
  
  protected def resize(copy: Int, capacity: Int) {
    val oldarray = array
    array = new Array[AnyRef](capacity)
    System.arraycopy(oldarray, 0, array, 0, copy)
  }
  
  def apply(i: Int) :T = {
    if (i >= size) throw new IndexOutOfBoundsException(i.toString)
    array(i).asInstanceOf[T]
  }
  def update(i: Int, elem: T) {
    if (i >= size) throw new IndexOutOfBoundsException(i.toString)
    array(i) = elem.asInstanceOf[AnyRef]
  }
  
  def +=(elem: T) {
    val next = size0
    size0 += 1
    if (size0 > array.length) resize(next, next*2)
    array(next) = elem.asInstanceOf[AnyRef]
  }
  
  def ++=(buffer: SortBuffer[T]) {
    val newSize = size + buffer.size
    if (newSize > array.length) resize(size, math.max(array.length*2, newSize))
    System.arraycopy(buffer.array, 0, array, size, buffer.size)
    size0 = newSize
  }
  
  def clear() {
    size0 = 0
  }
  
  def inplaceSort(comparator: Comparator[T]) {
    Arrays.sort(array, 0, size, comparator.asInstanceOf[Comparator[Object]])
  }
}


final class ConcurrentSortBuffer[T](initArray: Array[AnyRef]) extends SortBuffer[T](initArray) {
  def this(initCapacity: Int) { this(new Array[AnyRef](initCapacity)) }
  def this() { this(16) }
  
  private val atomicSize = new java.util.concurrent.atomic.AtomicInteger(0)
  
  override protected def size0: Int = atomicSize.get
  override protected def size0_=(i: Int) { atomicSize.set(i) }
  
  @volatile private var sync = false
  @volatile private var _array = initArray
  override protected[engine] def array: Array[AnyRef] = _array
  override protected[engine] def array_=(a: Array[AnyRef]) { _array = a }
  
  override def +=(elem: T) {
    val next = atomicSize.getAndIncrement()
    
    if (next >= array.length) this.synchronized {
      val size = array.length
      if (next >= size) resize(size, size*2)
    }
    
    // Guards against losing elements added just before array is resized.
    var localArray: Array[AnyRef] = null; while (localArray ne array) {
      localArray = array
      localArray(next) = elem.asInstanceOf[AnyRef]
    }
    sync = true
  }
  
  override def ++=(buffer: SortBuffer[T]) {
    val buffSize = buffer.size
    val newSize = atomicSize.addAndGet(buffSize)
    val oldSize = newSize - buffSize
    
    if (newSize > array.length) this.synchronized {
      if (newSize > array.length) {
        resize(oldSize, newSize*2)
      }
    }
    
    var localArray: Array[AnyRef] = null; while (localArray ne array) {
      localArray = array
      System.arraycopy(buffer.array, 0, localArray, oldSize, buffSize)
    }
    sync = true
  }
  
  override def clear() {
    size0 = 0
  }
}
