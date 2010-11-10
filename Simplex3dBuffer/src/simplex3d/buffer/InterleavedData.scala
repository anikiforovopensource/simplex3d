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
class InterleavedData private (dviews: Seq[AnyView]) extends immutable.IndexedSeq[AnyView] {

  verify(dviews)
  @transient private[this] var views = dviews.toArray
  
  def apply(i: Int) :AnyView = views(i)
  def length = views.length

  private[this] def verify(seqs: Seq[AnyView]) {
    val first = seqs.head
    val checks = new Array[Boolean](first.stride*first.bytesPerRawComponent)

    def checkOverlap(offset: Int, components: Int) {
      var i = offset; while (i < offset + components) {
        if (checks(i)) throw new IllegalArgumentException("Views must not have overlapping data.")
        checks(i) = true

        i += 1
      }
    }

    for (seq <- seqs) {
      if (first.byteStride != seq.byteStride)
        throw new IllegalArgumentException("Views must have the same byte stride.")

      if(first.size != seq.size)
        throw new IllegalArgumentException("Views must have the same size.")

      if(!first.sharesStoreObject(seq))
        throw new IllegalArgumentException("Views must share the same ByteByffer object.")

      checkOverlap(seq.byteOffset, seq.components)
    }
  }

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
      out.writeObject(views(i).copyAsDataArray())

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
    val views = new Array[AnyView](size)

    i = 0; while (i < size) {
      val darray = in.readObject().asInstanceOf[Data[_]]
      val (offset, stride) = header(i)

      views(i) = darray.copyAsDataView(byteBuffer, offset, stride)

      i += 1
    }

    verify(views)
    this.views = views
  }
}

object InterleavedData {
  def apply(seqs: AnyView*) = new InterleavedData(seqs)
  def apply(seqs: IndexedSeq[AnyView]) = new InterleavedData(seqs)
  def apply(seqs: inData[_]*)(size: Int) = new InterleavedData(interleaveAny(seqs: _*)(size))
}
