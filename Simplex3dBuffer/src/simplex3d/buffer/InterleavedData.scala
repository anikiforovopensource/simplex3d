/*
 * Simplex3d, CoreBuffer module
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

import scala.collection._
import java.io._
import java.nio._


/**
 * @author Aleksey Nikiforov (lex)
 */
@serializable @SerialVersionUID(8104346712419693669L)
class InterleavedData private (dviews: Seq[RawView]) extends immutable.IndexedSeq[RawView] {

  InterleavedData.verify(dviews)
  @transient private[this] var views = dviews.toArray
  
  def apply(i: Int) :RawView = views(i)
  def length = views.length

  @throws(classOf[IOException])
  private[this] def writeObject(out: ObjectOutputStream) {
    // Save byteCapacity.
    out.writeInt(views(0).byteCapacity)

    // Save view count.
    out.writeInt(views.length)

    // Save a header with offset-stride pairs.
    var i = 0; while (i < length) {
      val view = views(i)
      out.writeInt(view.offset)
      out.writeInt(view.stride)

      i += 1
    }

    // Save views as data arrays.
    i = 0; while (i < length) {
      val array = views(i).copyAsDataArray()
      val store = if (views(i).readOnly) array.asReadOnly() else array
      out.writeObject(store)

      i += 1
    }
  }

  @throws(classOf[IOException]) @throws(classOf[ClassNotFoundException])
  private[this] def readObject(in: ObjectInputStream) {
    // Load byteCapacity.
    val byteCapacity = in.readInt()
    val byteBuffer = ByteBuffer.allocateDirect(byteCapacity)

    // Load view count.
    val size = in.readInt()

    // Load the header as offset-stride pairs.
    val header = new Array[(Int, Int)](size)

    var i = 0; while (i < size) {
      header(i) = (in.readInt(), in.readInt())

      i += 1
    }

    // Restore views from saved data arrays.
    val views = new Array[RawView](size)

    i = 0; while (i < size) {
      val darray = in.readObject().asInstanceOf[Data[_]]
      val (offset, stride) = header(i)

      val view = darray.copyAsDataView(byteBuffer, offset, stride)
      views(i) = if (darray.readOnly) view.asReadOnly() else view

      i += 1
    }

    InterleavedData.verify(views)
    this.views = views
  }
}

object InterleavedData {
  def apply(seqs: RawView*) = new InterleavedData(seqs)
  def apply(seqs: IndexedSeq[RawView]) = new InterleavedData(seqs)

  final def verify(views: Seq[RawView]) {
    val first = views.head
    val interval = new Array[Boolean](first.stride*first.bytesPerRawComponent)

    def checkOverlap(byteOffset: Int, bytesTaken: Int) {
      var i = byteOffset; while (i < byteOffset + bytesTaken) {
        if (interval(i)) throw new IllegalArgumentException("Views must not have overlapping data.")
        interval(i) = true

        i += 1
      }
    }

    for (seq <- views) {
      if (first.byteStride != seq.byteStride)
        throw new IllegalArgumentException("Views must have the same byte stride.")

      if(first.size != seq.size)
        throw new IllegalArgumentException("Views must have the same size.")

      if(!first.sharesStoreObject(seq))
        throw new IllegalArgumentException("Views must share the same ByteByffer object.")

      checkOverlap(seq.byteOffset, seq.components*seq.bytesPerRawComponent)
    }
  }
}
