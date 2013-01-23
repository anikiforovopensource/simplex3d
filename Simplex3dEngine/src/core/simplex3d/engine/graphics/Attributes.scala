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

import java.util.WeakHashMap
import scala.reflect._
import scala.annotation.unchecked._
import scala.collection._
import scala.collection.mutable.ArrayBuffer
import simplex3d.math.types._
import simplex3d.data.extension._
import simplex3d.data._
import simplex3d.engine.util._


final class Attributes[F <: Format, +R <: Raw] private[engine] (
  @transient private val accessible: ReadDataView[F, _ <: R],
  private val linked: DirectSrc
) {

  private[engine] var shared: AttributesSharedState = null
  
  private[this] val bindings = if (isWritable) new WeakHashMap[AttributeBinding[_, _], Object] else null
  private[this] def notifyBindings() {
    import AccessChanges._
    val iter = bindings.keySet.iterator()
    while(iter.hasNext) {
      val binding = iter.next()
      binding.signalDataChanges()
    }
  }
  private[engine] def register(binding: AttributeBinding[_, _]) {
    bindings.put(binding, null)
  }
  private[engine] def unregister(binding: AttributeBinding[_, _]) {
    bindings.remove(binding)
  }
  
  { // Consistency check.
    var count = 0
    if (accessible != null) count += 1
    if (linked != null) count += 1
    
    if (count != 1) throw new IllegalArgumentException(
      "Data source must not be null.")
  }
  
  def sharedState = shared
  def isAccessible = (accessible != null)
  def isWritable = (isAccessible && !accessible.isReadOnly)
  
  def read: ReadDataView[F, R] = {
    if (isAccessible) accessible.asReadOnly()
    else throw new IllegalAccessException("Attributes are not accessible.")
  }
  
  def write: DataView[F, R] = write(0, src.size)
  
  def write(first: Int, count: Int): DataView[F, R] = {
    if (isWritable) {
      if (first < 0) throw new IllegalArgumentException("First = " + first + " must be greater than or equal to 0.")
      if (count < 0) throw new IllegalArgumentException("Count = " + count + " must be greater than or equal to 0.")
      if (first + count > accessible.size) throw new IllegalArgumentException(
        "Requested region from " + first + " to " + (first + count) +
        " exceeds the data size " + accessible.size + "."
      )
      
      shared.regions.put(first, count)
      notifyBindings()
      accessible.asInstanceOf[DataView[F, R]]
    }
    else throw new IllegalAccessException("Attributes are not writable.")
  }
  
  def src: DirectSrc = if (isAccessible) accessible else linked
}


final class AttributesSharedState private[engine] (
  val size: Int,
  val related: ReadSeq[Attributes[_ <: Format, Raw]],
  val caching: Caching.Value,
  private[engine] var persistent: InterleavedData
) extends EngineInfoRef {
  
  final class Subtext private[engine] () {
    def updatedRegions = regions
    
    def clearDataChanges() {
      regions.clear()
    }
    
    def hasDataChanges = regions.size > 0
  }
  private[engine] val subtext = new Subtext
  
  private[engine] val regions = new IntervalMap(64)
  regions.put(0, size) // Initialize as changed.
}


object Attributes {
  def apply[F <: Format, R <: Raw with Tangible]
    (size: Int, caching: Caching.Value = Caching.Dynamic)
    (implicit composition: CompositionFactory[F, _ >: R], primitives: PrimitiveFactory[F#Component, R])
  :Attributes[F, R] = {
    val data = composition.mkDataBuffer(primitives.mkDataBuffer(size*composition.components))
    fromData(data)
  }
  
  def fromData[F <: Format, R <: Raw]
    (data: ReadDataBuffer[F, _ <: R], caching: Caching.Value = Caching.Dynamic)
  :Attributes[F, R] = {
    val attributes = new Attributes[F, R](data, null)
    attributes.shared = new AttributesSharedState(
      data.size, new ReadSeq(ArrayBuffer(attributes)), caching, new InterleavedData(data)
    )
    attributes
  }
  
  def fromUncheckedSrc[F <: Format, R <: Raw]
    (src: DirectSrc with ContiguousSrc, caching: Caching.Value = Caching.Static)
    (implicit formatTag: ClassTag[F], rawTag: ClassTag[R])
  :Attributes[F, R] = {
    
    if (src.formatTag != formatTag) throw new IllegalArgumentException(
      "Data source format does not match the tag.")
      
    if (!(rawTag.runtimeClass.isAssignableFrom(RawEnum.ClassTags.fromRawEnum(src.rawEnum).runtimeClass)))
      throw new IllegalArgumentException("Data source raw type does not match the tag.")
      
    if (src.isInstanceOf[ReadDataBuffer[_, _]]) {
      fromData(src.asInstanceOf[ReadDataBuffer[F, R]], caching)
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
  private val related = ArrayBuffer[Attributes[_ <: Format, Raw]]()
  
  val Attributes = new InterleavedFactory
  
  final class InterleavedFactory {
    def fromData[F <: Format, R <: Raw](view: ReadDataView[F, _ <: R])
    :Attributes[F, R] = {
      val attributes = new Attributes[F, R](view, null)
      related += attributes
      attributes
    }
    
    def fromUncheckedSrc[F <: Format, R <: Raw](src: DirectSrc)
      (implicit formatTag: ClassTag[F], rawTag: ClassTag[R])
    :Attributes[F, R] = {
      
      if (src.formatTag != formatTag) throw new IllegalArgumentException(
        "Data source format does not match the tag.")
        
      if (!(rawTag.runtimeClass.isAssignableFrom(RawEnum.ClassTags.fromRawEnum(src.rawEnum).runtimeClass)))
        throw new IllegalArgumentException("Data source raw type does not match the tag.")
    
      if (src.isInstanceOf[ReadDataView[_, _]]) {
        fromData[F, R](src.asInstanceOf[ReadDataView[F, R]])
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
  private[this] var putCount = 0
  
  /* Auto merge after a predefined number of added elements to prevent memory leaks when the interval map is
   * updated by the application but never merged because the parent mesh is discarded by the frustum culling.
   */
  private[this] val autoMergeAt = 64
  
  
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
    
    putCount += 1
    if (putCount > autoMergeAt) merge()
  }
  
  def merge() {
    if (!needsMerge || size0 < 2) {
      needsMerge = false
      return
    }
    
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
    putCount = 0
  }
  
  def clear() {
    size0 = 0
    needsMerge = false
    putCount = 0
  }
  
  override def toString() :String = {
    "IntervalMap(" +
      array.take(size0).map(n => (upper(n), lower(n))).mkString(", ") +
    ")"
  }
}
