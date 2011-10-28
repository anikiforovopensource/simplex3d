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

import scala.annotation.unchecked._
import scala.collection._
import scala.collection.mutable.ArrayBuffer
import simplex3d.math.types._
import simplex3d.data._


final class Attributes[F <: Format with MathType, +R <: Raw] private[engine] (
  @transient private val accessible: ReadDataView[F, _ <: R],
  private val linked: DirectSrc
) {

  private[engine] var shared: AttributesSharedState = null
  
  { // Consistency check.
    var count = 0
    if (accessible != null) count += 1
    if (linked != null) count += 1
    
    require(count == 1, "Data source must not be null.")
  }
  
  def sharedState = shared
  def isAccessible = (accessible != null)
  def isWritable = (isAccessible && !accessible.isReadOnly)
  
  def read: ReadDataView[F, R] = {
    if (isAccessible) accessible.asReadOnly()
    else null
  }
  
  def write: DataView[F, R] = write(0, src.size)
  
  def write(first: Int, count: Int): DataView[F, R] = {
    if (isWritable) {
      if (first < 0) throw new IllegalArgumentException("First = " + first + " must be greater than or equal to 0.")
      if (count < 0) throw new IllegalArgumentException("Count = " + count + " must be greater than or equal to 0.")
      if (first + count > accessible.size) throw new IllegalArgumentException(
        "Requested region from " + first + " to " + (first + count) +
        " exceeds the data size = " + accessible.size + "."
      )
      
      shared.regions.put(first, count)
      accessible.asInstanceOf[DataView[F, R]]
    }
    else null
  }
  
  def src: DirectSrc = if (isAccessible) accessible else linked
}


final class AttributesSharedState private[engine] (
  val size: Int,
  val related: ReadSeq[Attributes[_ <: Format with MathType, Raw]],
  val caching: Caching.Value,
  private[engine] var persistent: InterleavedData
) extends EngineInfo {
  
  final class Subtext private[engine] () {
    def updatedRegions = regions
    
    def clearDataChanges() {
      regions.clear()
    }
  }
  private[engine] val subtext = new Subtext
  
  private[engine] val regions = new IntervalMap(64)
  regions.put(0, size) // Initialize as changed.
  
  def hasDataChanges = regions.size > 0
}


object Attributes {
    
  def apply[F <: Format with MathType, R <: Raw]
    (data: ReadDataBuffer[F, _ <: R], caching: Caching.Value = Caching.Dynamic)
  :Attributes[F, R] = {
    val attributes = new Attributes[F, R](data, null)
    attributes.shared = new AttributesSharedState(
      data.size, new ReadSeq(ArrayBuffer(attributes)), caching, new InterleavedData(data)
    )
    attributes
  }
  
  def unchecked[F <: Format with MathType, R <: Raw]
    (src: DirectSrc with ContiguousSrc, caching: Caching.Value = Caching.Static)
    (implicit formatManifest: ClassManifest[F], rawManifest: ClassManifest[R])
  :Attributes[F, R] = {
    
    require(src.formatManifest == formatManifest, "Data source format does not match manifest.")
    require(RawManifest.fromRawType(src.rawType) <:< rawManifest, "Data source raw type does not match manifest.")
      
    if (src.isInstanceOf[ReadDataBuffer[_, _]]) {
      apply(src.asInstanceOf[ReadDataBuffer[F, R]], caching)
    }
    else {
      val attributes = new Attributes[F, R](null, src)
      attributes.shared = new AttributesSharedState(
        src.size, new ReadSeq(ArrayBuffer(attributes)), caching, null
      )
      attributes
    }
  }
}


// TODO restore DelayedInit when this scala bug is fixed: https://issues.scala-lang.org/browse/SI-4683
class interleaved(val caching: Caching.Value = Caching.Static) { // extends DelayedInit { 
  private val related = ArrayBuffer[Attributes[_ <: Format with MathType, Raw]]()
  
  val Attributes = new InterleavedFactory
  
  final class InterleavedFactory {
    def apply[F <: Format with MathType, R <: Raw](view: ReadDataView[F, _ <: R])
    :Attributes[F, R] = {
      val attributes = new Attributes[F, R](view, null)
      related += attributes
      attributes
    }
    
    def unchecked[F <: Format with MathType, R <: Raw](src: DirectSrc)
      (implicit formatManifest: ClassManifest[F], rawManifest: ClassManifest[R])
    :Attributes[F, R] = {
      
      require(src.formatManifest == formatManifest, "Data source format does not match manifest.")
      require(RawManifest.fromRawType(src.rawType) <:< rawManifest, "Data source raw type does not match manifest.")
    
      if (src.isInstanceOf[ReadDataView[_, _]]) {
        apply[F, R](src.asInstanceOf[ReadDataView[F, R]])
      }
      else {
        val attributes = new Attributes[F, R](null, src)
        related += attributes
        attributes
      }
    }
  }
  
  def delayedInit(init: => Unit) {
    init
    
    if (related.size > 0) {
      val sources = related.map(_.src)
      val isAccessible = related.head.isInstanceOf[RawView]
      
      val persistent =
        if (isAccessible) new InterleavedData(sources.asInstanceOf[Seq[RawView]]: _*)
        else { InterleavedData.verifyFormat(sources); null }
      
      val sharedState = new AttributesSharedState(related.head.src.size, new ReadSeq(related), caching, persistent)
      
      var i = 0; while (i < related.length) {
        related(i).shared = sharedState
        
        i += 1
      }
    }
  }
}


/** Specialized class to keep track of update range for VBO.
 * 
 * @param initCapacity
 * @param mergeTolerance (mergeTolerance + 1) is the minimum gap between two intervals. If two consecutive intervals
 *   are closer than this gap, they will be merged.
 *   Example: intervals (1 until 5) and (10 until 20) have a gap of 5 between them, so if tolerance is set
 *   to 5 or more, these two intervals will be merged into (1 until 20) when calling '''merge()'''.
 */
final class IntervalMap(private val initCapacity: Int, val mergeTolerance: Int) {
  
  def this(mergeTolerance: Int) {
    this(4, mergeTolerance)
  }
  
  
  private[this] var array = new Array[Long](initCapacity)
  private[this] var size0 = 0
  private[this] var needsMerge = false
  
  def size = size0
  
  def first(index: Int) = upper(array(index))
  def count(index: Int) = lower(array(index))
  
  private def upper(n: Long) = (n >> 32).toInt
  private def lower(n: Long) = n.toInt
  
  private def put(index: Int, first: Int, count: Int) {
    array(index) = ((first.toLong) << 32) | count
  }
  
  /** Empty ranges are ignored.
   * 
   * @param start the beginning of the range (inclusive)
   * @param end the end of the range (exclusive)
   */
  def put(first: Int, count: Int) {
    if (first < 0 || count < 0) throw new IndexOutOfBoundsException(
      "First and count must be non-negative: first = " + first + ", count = " + count + "."
    )
    if (count == 0) return
    
    
    if (array.length == size0) {
      val newarray = new Array[Long](2*size0)
      System.arraycopy(array, 0, newarray, 0, size0)
      array = newarray
    }
    
    put(size0, first, count)
    size0 += 1
    
    needsMerge = true
  }
  
  def merge() {
    if (!needsMerge || size0 < 2) return;
    
    java.util.Arrays.sort(array, 0, size0)
    
    var lastStart = first(0)
    var lastEnd = lastStart + count(0)
    
    var lastRange = 0
    
    val size = size0; var i = 1; while (i < size) {
      val start = first(i)
      val end = start + count(i)
      
      if (start <= lastEnd + mergeTolerance) {
        if (end > lastEnd) {
          lastEnd = end
        }
      } else {
        put(lastRange, lastStart, lastEnd - lastStart)
        lastRange += 1
        lastStart = start
        lastEnd = end
      }
      
      i += 1
    }
    
    put(lastRange, lastStart, lastEnd - lastStart)
    size0 = lastRange + 1
    
    needsMerge = false
  }
  
  def clear() {
    size0 = 0
  }
  
  override def toString() :String = {
    "UpdateIntervals(" +
      array.take(size0).map(n => (upper(n), lower(n))).mkString(", ") +
    ")"
  }
}
