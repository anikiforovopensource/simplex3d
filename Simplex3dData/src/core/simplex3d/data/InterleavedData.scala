/*
 * Simplex3dData - Core Module
 * Copyright (C) 2010-2011, Aleksey Nikiforov
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
import java.io._
import java.nio._


/**
 * @author Aleksey Nikiforov (lex)
 */
@SerialVersionUID(8104346712419693669L)
class InterleavedData(seqs: RawView*) extends immutable.IndexedSeq[RawView] with Serializable {
  
  def this(seqs: IndexedSeq[RawView]) = this(seqs: _*)


  InterleavedData.verifyFormat(seqs)
  @transient private[this] var views = seqs.toArray
  
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
      val store = if (views(i).isReadOnly) array.asReadOnly() else array
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
      type T = F forSome { type F <: Format }
      val darray = in.readObject().asInstanceOf[DataSeq[T, Raw]]
      val (offset, stride) = header(i)

      val view = darray.mkDataView(byteBuffer, offset, stride)
      view.put(darray)
      views(i) = if (darray.isReadOnly) view.asReadOnly() else view

      i += 1
    }

    InterleavedData.verifyFormat(views)
    this.views = views
  }
}


object InterleavedData {

  final def verifyFormat(sources: Seq[DataSrc]) {
    val first = sources.head
    val interval = new Array[Boolean](first.byteStride)

    def checkOverlap(byteOffset: Int, bytesTaken: Int) {
      var i = byteOffset; while (i < byteOffset + bytesTaken) {
        if (interval(i)) throw new DataFormatException("Interleaved data must not overlap.")
        interval(i) = true

        i += 1
      }
    }

    for (src <- sources) {
      if (first.byteStride != src.byteStride)
        throw new DataFormatException("Interleaved sources must have the same byte stride.")

      if(first.size != src.size)
        throw new DataFormatException("Interleaved sources must have the same size.")

      if(!first.sharesStorageWith(src))
        throw new DataFormatException("Interleaved sources must share the same storage.")

      checkOverlap(src.byteOffset, src.components*src.bytesPerComponent)
    }
  }
}
