/*
 * Simplex3d, BaseBuffer module
 * Copyright (C) 2010 Simplex3d Team
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

package simplex3d

import java.nio._
import simplex3d.buffer._
import simplex3d.math._


/**
 * @author Aleksey Nikiforov (lex)
 */
package buffer {
  private[buffer] class UnsignedImplicits {
    implicit val arrayInt1UByte = (
      (array: Array[Byte]) => new ArrayInt1UByte(array), 1, classOf[UByte]
    )
    implicit val arrayInt1UShort = (
      (array: Array[Char]) => new ArrayInt1UShort(array), 1, classOf[UShort]
    )
    implicit val arrayInt1UInt = (
      (array: Array[Int]) => new ArrayInt1UInt(array), 1, classOf[UInt]
    )

    implicit val bufferInt1UByte = (
      (buffer: ByteBuffer) => new BufferInt1UByte(buffer), 1, classOf[UByte]
    )
    implicit val bufferInt1UShort = (
      (buffer: ByteBuffer) => new BufferInt1UShort(buffer), 1, classOf[UShort]
    )
    implicit val bufferInt1UInt = (
      (buffer: ByteBuffer) => new BufferInt1UInt(buffer), 1, classOf[UInt]
    )
  }
}

package object buffer extends UnsignedImplicits {
  
  def interleave[
    T1 <: MetaType, D1 <: RawType,
    T2 <: MetaType, D2 <: RawType
  ](
    seq1: DataSeq[T1, D1],
    seq2: DataSeq[T2, D2]
  ) = {
    val views = interleaveMany(seq1, seq2)
    (
      views(0).asInstanceOf[DataView[T1, D1]],
      views(1).asInstanceOf[DataView[T2, D2]]
    )
  }

  def interleave[
    T1 <: MetaType, D1 <: RawType,
    T2 <: MetaType, D2 <: RawType,
    T3 <: MetaType, D3 <: RawType
  ](
    seq1: DataSeq[T1, D1],
    seq2: DataSeq[T2, D2],
    seq3: DataSeq[T3, D3]
  ) = {
    val views = interleaveMany(seq1, seq2, seq3)
    (
      views(0).asInstanceOf[DataView[T1, D1]],
      views(1).asInstanceOf[DataView[T2, D2]],
      views(2).asInstanceOf[DataView[T3, D3]]
    )
  }

  def interleave[
    T1 <: MetaType, D1 <: RawType,
    T2 <: MetaType, D2 <: RawType,
    T3 <: MetaType, D3 <: RawType,
    T4 <: MetaType, D4 <: RawType
  ](
    seq1: DataSeq[T1, D1],
    seq2: DataSeq[T2, D2],
    seq3: DataSeq[T3, D3],
    seq4: DataSeq[T4, D4]
  ) = {
    val views = interleaveMany(
      seq1, seq2, seq3, seq4
    )
    (
      views(0).asInstanceOf[DataView[T1, D1]],
      views(1).asInstanceOf[DataView[T2, D2]],
      views(2).asInstanceOf[DataView[T3, D3]],
      views(0).asInstanceOf[DataView[T4, D4]]
    )
  }

  def interleave[
    T1 <: MetaType, D1 <: RawType,
    T2 <: MetaType, D2 <: RawType,
    T3 <: MetaType, D3 <: RawType,
    T4 <: MetaType, D4 <: RawType,
    T5 <: MetaType, D5 <: RawType
  ](
    seq1: DataSeq[T1, D1],
    seq2: DataSeq[T2, D2],
    seq3: DataSeq[T3, D3],
    seq4: DataSeq[T4, D4],
    seq5: DataSeq[T5, D5]
  ) = {
    val views = interleaveMany(
      seq1, seq2, seq3, seq4, seq5
    )
    (
      views(0).asInstanceOf[DataView[T1, D1]],
      views(1).asInstanceOf[DataView[T2, D2]],
      views(2).asInstanceOf[DataView[T3, D3]],
      views(0).asInstanceOf[DataView[T4, D4]],
      views(0).asInstanceOf[DataView[T5, D5]]
    )
  }

  def interleave[
    T1 <: MetaType, D1 <: RawType,
    T2 <: MetaType, D2 <: RawType,
    T3 <: MetaType, D3 <: RawType,
    T4 <: MetaType, D4 <: RawType,
    T5 <: MetaType, D5 <: RawType,
    T6 <: MetaType, D6 <: RawType
  ](
    seq1: DataSeq[T1, D1],
    seq2: DataSeq[T2, D2],
    seq3: DataSeq[T3, D3],
    seq4: DataSeq[T4, D4],
    seq5: DataSeq[T5, D5],
    seq6: DataSeq[T6, D6]
  ) = {
    val views = interleaveMany(
      seq1, seq2, seq3, seq4, seq5, seq6
    )
    (
      views(0).asInstanceOf[DataView[T1, D1]],
      views(1).asInstanceOf[DataView[T2, D2]],
      views(2).asInstanceOf[DataView[T3, D3]],
      views(0).asInstanceOf[DataView[T4, D4]],
      views(0).asInstanceOf[DataView[T5, D5]],
      views(0).asInstanceOf[DataView[T6, D6]]
    )
  }

  def interleave[
    T1 <: MetaType, D1 <: RawType,
    T2 <: MetaType, D2 <: RawType,
    T3 <: MetaType, D3 <: RawType,
    T4 <: MetaType, D4 <: RawType,
    T5 <: MetaType, D5 <: RawType,
    T6 <: MetaType, D6 <: RawType,
    T7 <: MetaType, D7 <: RawType
  ](
    seq1: DataSeq[T1, D1],
    seq2: DataSeq[T2, D2],
    seq3: DataSeq[T3, D3],
    seq4: DataSeq[T4, D4],
    seq5: DataSeq[T5, D5],
    seq6: DataSeq[T6, D6],
    seq7: DataSeq[T7, D7]
  ) = {
    val views = interleaveMany(
      seq1, seq2, seq3, seq4, seq5, seq6, seq7
    )
    (
      views(0).asInstanceOf[DataView[T1, D1]],
      views(1).asInstanceOf[DataView[T2, D2]],
      views(2).asInstanceOf[DataView[T3, D3]],
      views(0).asInstanceOf[DataView[T4, D4]],
      views(0).asInstanceOf[DataView[T5, D5]],
      views(0).asInstanceOf[DataView[T6, D6]],
      views(0).asInstanceOf[DataView[T7, D7]]
    )
  }

  def interleave[
    T1 <: MetaType, D1 <: RawType,
    T2 <: MetaType, D2 <: RawType,
    T3 <: MetaType, D3 <: RawType,
    T4 <: MetaType, D4 <: RawType,
    T5 <: MetaType, D5 <: RawType,
    T6 <: MetaType, D6 <: RawType,
    T7 <: MetaType, D7 <: RawType,
    T8 <: MetaType, D8 <: RawType
  ](
    seq1: DataSeq[T1, D1],
    seq2: DataSeq[T2, D2],
    seq3: DataSeq[T3, D3],
    seq4: DataSeq[T4, D4],
    seq5: DataSeq[T5, D5],
    seq6: DataSeq[T6, D6],
    seq7: DataSeq[T7, D7],
    seq8: DataSeq[T8, D8]
  ) = {
    val views = interleaveMany(
      seq1, seq2, seq3, seq4, seq5, seq6, seq7, seq8
    )
    (
      views(0).asInstanceOf[DataView[T1, D1]],
      views(1).asInstanceOf[DataView[T2, D2]],
      views(2).asInstanceOf[DataView[T3, D3]],
      views(0).asInstanceOf[DataView[T4, D4]],
      views(0).asInstanceOf[DataView[T5, D5]],
      views(0).asInstanceOf[DataView[T6, D6]],
      views(0).asInstanceOf[DataView[T7, D7]],
      views(0).asInstanceOf[DataView[T8, D8]]
    )
  }

  def interleave[
    T1 <: MetaType, D1 <: RawType,
    T2 <: MetaType, D2 <: RawType,
    T3 <: MetaType, D3 <: RawType,
    T4 <: MetaType, D4 <: RawType,
    T5 <: MetaType, D5 <: RawType,
    T6 <: MetaType, D6 <: RawType,
    T7 <: MetaType, D7 <: RawType,
    T8 <: MetaType, D8 <: RawType,
    T9 <: MetaType, D9 <: RawType
  ](
    seq1: DataSeq[T1, D1],
    seq2: DataSeq[T2, D2],
    seq3: DataSeq[T3, D3],
    seq4: DataSeq[T4, D4],
    seq5: DataSeq[T5, D5],
    seq6: DataSeq[T6, D6],
    seq7: DataSeq[T7, D7],
    seq8: DataSeq[T8, D8],
    seq9: DataSeq[T9, D9]
  ) = {
    val views = interleaveMany(
      seq1, seq2, seq3, seq4, seq5, seq6, seq7, seq8, seq9
    )
    (
      views(0).asInstanceOf[DataView[T1, D1]],
      views(1).asInstanceOf[DataView[T2, D2]],
      views(2).asInstanceOf[DataView[T3, D3]],
      views(0).asInstanceOf[DataView[T4, D4]],
      views(0).asInstanceOf[DataView[T5, D5]],
      views(0).asInstanceOf[DataView[T6, D6]],
      views(0).asInstanceOf[DataView[T7, D7]],
      views(0).asInstanceOf[DataView[T8, D8]],
      views(0).asInstanceOf[DataView[T9, D9]]
    )
  }

  def interleaveMany(dataSeqs: DataSeq[_ <: MetaType, _ <: RawType]*)
  :Array[DataView[_, _]] =
  {
    // check arguments
    if (dataSeqs.length == 0) return new Array[DataView[_, _]](0)

    val size = dataSeqs(0).size
    var i = 1; while(i < dataSeqs.length) {
      if (dataSeqs(i).size != size) throw new IllegalArgumentException(
        "All sequences must have the same size."
      )
      i += 1
    }

    // find maxComponentWidth and totalWidth
    var totalWidth = 0
    var maxComponentWidth = 1

    i = 0; while (i < dataSeqs.length) {
      val seq = dataSeqs(i)

      totalWidth += seq.componentBytes*seq.components
      if (seq.componentBytes > maxComponentWidth) {
        maxComponentWidth = seq.componentBytes
      }

      i += 1
    }


    // order by data width
    val order = new Array[Int](dataSeqs.length)
    var count = 0
    
    var width = maxComponentWidth; while (width > 0) {

      i = 0; while (i < dataSeqs.length) {
        if (dataSeqs(i).componentBytes == width) {
          order(count) = i
          count += 1
        }

        i += 1
      }

      width /= 2
    }

    // find pad value
    var pad = totalWidth%maxComponentWidth
    if (pad > 0) pad = maxComponentWidth - pad
    val byteStride = totalWidth + pad

    // generate
    val byteBuffer = BufferUtil.allocateByteBuffer(byteStride*size)
    val result = new Array[DataView[_, _]](dataSeqs.length)
    var byteOffset = 0
    i = 0; while (i < dataSeqs.length) {
      val seq = dataSeqs(order(i))
      result(order(i)) = seq.asView(
        byteBuffer,
        byteOffset/seq.componentBytes,
        byteStride/seq.componentBytes - seq.components
      )
      byteOffset += seq.componentBytes*seq.components
      
      i += 1
    }

    result
  }
}
