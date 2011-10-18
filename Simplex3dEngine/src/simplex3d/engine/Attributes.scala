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


// XXX track and free discarded gl buffers. Track and handle data changes.
final class Attributes[F <: Format with MathType, +R <: Raw] private[engine] (
  @transient private val accessible: ReadDataView[F, _ <: R],
  private val linked: DirectSrc,
  val formatManifest: ClassManifest[F],
  val rawManifest: ClassManifest[_ <: R]
) {

  private[engine] var shared: AttributesSharedState = null
  
  { // Consistency check.
    var count = 0
    if (accessible != null) count += 1
    if (linked != null) count += 1
    
    require(count == 1, "Data source must not be null.")
    
    //require(src.formatManifest == formatManifest, "Data source format does not match manifest.") // XXX rething this
    //require(RawManifest.fromRawType(src.rawType) <:< rawManifest, "Data source raw type does not match manifest.") // XXX rething this
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
      
      shared.regions += new Region(first, count)
      accessible.asInstanceOf[DataView[F, R]]
    }
    else null
  }
  
  def src: DirectSrc = if (isAccessible) accessible else linked
}


final class AttributesSharedState private[engine] (
  updateRegion: Region,
  val related: ReadSeq[Attributes[_ <: Format with MathType, Raw]],
  val caching: Caching.Value,
  private[engine] var persistent: InterleavedData
) extends EngineInfo {
  
  final class Subtext private[engine] () {
    def clearDataChanges() {
      regions.clear()
    }
  }
  
  private[engine] val subtext = new Subtext
  
  
  private[engine] val regions = ArrayBuffer[Region](updateRegion)
  val updatedRegions = new ReadSeq(regions)
  def hasDataChanges = regions.size > 0
}


object Attributes {
    
  def apply[F <: Format with MathType, R <: Raw]
    (data: ReadDataBuffer[F, _ <: R], caching: Caching.Value = Caching.Dynamic)
    (implicit formatManifest: ClassManifest[F], rawManifest: ClassManifest[R])
  :Attributes[F, R] = {
    val region = new Region(0, data.size)
    val attributes = new Attributes[F, R](data, null, formatManifest, rawManifest)
    attributes.shared = new AttributesSharedState(
      region, new ReadSeq(ArrayBuffer(attributes)), caching, new InterleavedData(data)
    )
    attributes
  }
  
  def unchecked[F <: Format with MathType, R <: Raw]
    (src: DirectSrc with ContiguousSrc, caching: Caching.Value = Caching.Static)
    (implicit formatManifest: ClassManifest[F], rawManifest: ClassManifest[R])
  :Attributes[F, R] = {
    if (src.isInstanceOf[ReadDataBuffer[_, _]]) {
      apply(src.asInstanceOf[ReadDataBuffer[F, R]], caching)(formatManifest, rawManifest)
    }
    else {
      val region = new Region(0, src.size)
      val attributes = new Attributes[F, R](null, src, formatManifest, rawManifest)
      attributes.shared = new AttributesSharedState(
        region, new ReadSeq(ArrayBuffer(attributes)), caching, null
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
      (implicit formatManifest: ClassManifest[F], rawManifest: ClassManifest[R])
    :Attributes[F, R] = {
      val attributes = new Attributes[F, R](view, null, formatManifest, rawManifest)
      related += attributes
      attributes
    }
    
    def unchecked[F <: Format with MathType, R <: Raw](src: DirectSrc)
      (implicit formatManifest: ClassManifest[F], rawManifest: ClassManifest[R])
    :Attributes[F, R] = {
      if (src.isInstanceOf[ReadDataView[_, _]]) {
        apply[F, R](src.asInstanceOf[ReadDataView[F, R]])(formatManifest, rawManifest)
      }
      else {
        val attributes = new Attributes[F, R](null, src, formatManifest, rawManifest)
        related += attributes
        attributes
      }
    }
  }
  
  def delayedInit(init: => Unit) {
    init
    
    if (related.size > 0) {
      val region = new Region(0, related.head.src.size)
      val sources = related.map(_.src)
      val isAccessible = related.head.isInstanceOf[RawView]
      
      val persistent =
        if (isAccessible) new InterleavedData(sources.asInstanceOf[Seq[RawView]]: _*)
        else { InterleavedData.verifyFormat(sources); null }
      
      val sharedState = new AttributesSharedState(region, new ReadSeq(related), caching, persistent)
      
      var i = 0; while (i < related.length) {
        related(i).shared = sharedState
        
        i += 1
      }
    }
  }
}
