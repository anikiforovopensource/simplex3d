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
import simplex3d.math._
import simplex3d.buffer._


/**
 * @author Aleksey Nikiforov (lex)
 */
package buffer {
  private[buffer] class UnsignedImplicits {
    private final type F[T <: ElemType, D <: RawType] = SimpleFactoryRef[T, D]

    implicit final val FactoryInt1SByte = new F(new ArrayInt1SByte)
    implicit final val FactoryInt1UByte = new F(new ArrayInt1UByte)

    implicit final val FactoryInt1SShort = new F(new ArrayInt1SShort)
    implicit final val FactoryInt1UShort = new F(new ArrayInt1UShort)

    implicit final val FactoryInt1SInt = new F(new ArrayInt1SInt)
    implicit final val FactoryInt1UInt = new F(new ArrayInt1UInt)
  }
}

package object buffer extends UnsignedImplicits {

  @inline implicit final def roArrayDataToIndex[D  <: ReadableIndex] (
    d: ReadOnlyDataArray[Int1, D]
  ) = d.asInstanceOf[ReadOnlyIndexArray[D]]

  @inline implicit final def roBufferDataToIndex[D  <: ReadableIndex](
    d: ReadOnlyDataBuffer[Int1, D]
  ) = d.asInstanceOf[ReadOnlyIndexBuffer[D]]

  
  @inline implicit final def arrayDataToIndex[D  <: ReadableIndex](
    d: DataArray[Int1, D]
  ) = d.asInstanceOf[IndexArray[D]]

  @inline implicit final def bufferDataToIndex[D  <: ReadableIndex](
    d: DataBuffer[Int1, D]
  ) = d.asInstanceOf[IndexBuffer[D]]

  
  def allocateByteBuffer(size: Int) = {
    val direct = ByteBuffer.allocateDirect(size)
    direct.order(ByteOrder.nativeOrder())
  }
  
  def interleave[
    T1 <: ElemType, D1 <: RawType,
    T2 <: ElemType, D2 <: RawType
  ](
    size: Int,
    seq1: DataSeq[T1, D1],
    seq2: DataSeq[T2, D2]
  ) = {
    val views = interleaveAny(size, seq1, seq2)
    (
      views(0).asInstanceOf[DataView[T1, D1]],
      views(1).asInstanceOf[DataView[T2, D2]]
    )
  }

  def interleave[
    T1 <: ElemType, D1 <: RawType,
    T2 <: ElemType, D2 <: RawType,
    T3 <: ElemType, D3 <: RawType
  ](
    size: Int,
    seq1: DataSeq[T1, D1],
    seq2: DataSeq[T2, D2],
    seq3: DataSeq[T3, D3]
  ) = {
    val views = interleaveAny(size, seq1, seq2, seq3)
    (
      views(0).asInstanceOf[DataView[T1, D1]],
      views(1).asInstanceOf[DataView[T2, D2]],
      views(2).asInstanceOf[DataView[T3, D3]]
    )
  }

  def interleave[
    T1 <: ElemType, D1 <: RawType,
    T2 <: ElemType, D2 <: RawType,
    T3 <: ElemType, D3 <: RawType,
    T4 <: ElemType, D4 <: RawType
  ](
    size: Int,
    seq1: DataSeq[T1, D1],
    seq2: DataSeq[T2, D2],
    seq3: DataSeq[T3, D3],
    seq4: DataSeq[T4, D4]
  ) = {
    val views = interleaveAny(
      size,
      seq1, seq2, seq3, seq4
    )
    (
      views(0).asInstanceOf[DataView[T1, D1]],
      views(1).asInstanceOf[DataView[T2, D2]],
      views(2).asInstanceOf[DataView[T3, D3]],
      views(3).asInstanceOf[DataView[T4, D4]]
    )
  }

  def interleave[
    T1 <: ElemType, D1 <: RawType,
    T2 <: ElemType, D2 <: RawType,
    T3 <: ElemType, D3 <: RawType,
    T4 <: ElemType, D4 <: RawType,
    T5 <: ElemType, D5 <: RawType
  ](
    size: Int,
    seq1: DataSeq[T1, D1],
    seq2: DataSeq[T2, D2],
    seq3: DataSeq[T3, D3],
    seq4: DataSeq[T4, D4],
    seq5: DataSeq[T5, D5]
  ) = {
    val views = interleaveAny(
      size,
      seq1, seq2, seq3, seq4, seq5
    )
    (
      views(0).asInstanceOf[DataView[T1, D1]],
      views(1).asInstanceOf[DataView[T2, D2]],
      views(2).asInstanceOf[DataView[T3, D3]],
      views(3).asInstanceOf[DataView[T4, D4]],
      views(4).asInstanceOf[DataView[T5, D5]]
    )
  }

  def interleave[
    T1 <: ElemType, D1 <: RawType,
    T2 <: ElemType, D2 <: RawType,
    T3 <: ElemType, D3 <: RawType,
    T4 <: ElemType, D4 <: RawType,
    T5 <: ElemType, D5 <: RawType,
    T6 <: ElemType, D6 <: RawType
  ](
    size: Int,
    seq1: DataSeq[T1, D1],
    seq2: DataSeq[T2, D2],
    seq3: DataSeq[T3, D3],
    seq4: DataSeq[T4, D4],
    seq5: DataSeq[T5, D5],
    seq6: DataSeq[T6, D6]
  ) = {
    val views = interleaveAny(
      size,
      seq1, seq2, seq3, seq4, seq5, seq6
    )
    (
      views(0).asInstanceOf[DataView[T1, D1]],
      views(1).asInstanceOf[DataView[T2, D2]],
      views(2).asInstanceOf[DataView[T3, D3]],
      views(3).asInstanceOf[DataView[T4, D4]],
      views(4).asInstanceOf[DataView[T5, D5]],
      views(5).asInstanceOf[DataView[T6, D6]]
    )
  }

  def interleave[
    T1 <: ElemType, D1 <: RawType,
    T2 <: ElemType, D2 <: RawType,
    T3 <: ElemType, D3 <: RawType,
    T4 <: ElemType, D4 <: RawType,
    T5 <: ElemType, D5 <: RawType,
    T6 <: ElemType, D6 <: RawType,
    T7 <: ElemType, D7 <: RawType
  ](
    size: Int,
    seq1: DataSeq[T1, D1],
    seq2: DataSeq[T2, D2],
    seq3: DataSeq[T3, D3],
    seq4: DataSeq[T4, D4],
    seq5: DataSeq[T5, D5],
    seq6: DataSeq[T6, D6],
    seq7: DataSeq[T7, D7]
  ) = {
    val views = interleaveAny(
      size,
      seq1, seq2, seq3, seq4, seq5, seq6, seq7
    )
    (
      views(0).asInstanceOf[DataView[T1, D1]],
      views(1).asInstanceOf[DataView[T2, D2]],
      views(2).asInstanceOf[DataView[T3, D3]],
      views(3).asInstanceOf[DataView[T4, D4]],
      views(4).asInstanceOf[DataView[T5, D5]],
      views(5).asInstanceOf[DataView[T6, D6]],
      views(6).asInstanceOf[DataView[T7, D7]]
    )
  }

  def interleave[
    T1 <: ElemType, D1 <: RawType,
    T2 <: ElemType, D2 <: RawType,
    T3 <: ElemType, D3 <: RawType,
    T4 <: ElemType, D4 <: RawType,
    T5 <: ElemType, D5 <: RawType,
    T6 <: ElemType, D6 <: RawType,
    T7 <: ElemType, D7 <: RawType,
    T8 <: ElemType, D8 <: RawType
  ](
    size: Int,
    seq1: DataSeq[T1, D1],
    seq2: DataSeq[T2, D2],
    seq3: DataSeq[T3, D3],
    seq4: DataSeq[T4, D4],
    seq5: DataSeq[T5, D5],
    seq6: DataSeq[T6, D6],
    seq7: DataSeq[T7, D7],
    seq8: DataSeq[T8, D8]
  ) = {
    val views = interleaveAny(
      size,
      seq1, seq2, seq3, seq4, seq5, seq6, seq7, seq8
    )
    (
      views(0).asInstanceOf[DataView[T1, D1]],
      views(1).asInstanceOf[DataView[T2, D2]],
      views(2).asInstanceOf[DataView[T3, D3]],
      views(3).asInstanceOf[DataView[T4, D4]],
      views(4).asInstanceOf[DataView[T5, D5]],
      views(5).asInstanceOf[DataView[T6, D6]],
      views(6).asInstanceOf[DataView[T7, D7]],
      views(7).asInstanceOf[DataView[T8, D8]]
    )
  }

  def interleave[
    T1 <: ElemType, D1 <: RawType,
    T2 <: ElemType, D2 <: RawType,
    T3 <: ElemType, D3 <: RawType,
    T4 <: ElemType, D4 <: RawType,
    T5 <: ElemType, D5 <: RawType,
    T6 <: ElemType, D6 <: RawType,
    T7 <: ElemType, D7 <: RawType,
    T8 <: ElemType, D8 <: RawType,
    T9 <: ElemType, D9 <: RawType
  ](
    size: Int,
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
    val views = interleaveAny(
      size,
      seq1, seq2, seq3, seq4, seq5, seq6, seq7, seq8, seq9
    )
    (
      views(0).asInstanceOf[DataView[T1, D1]],
      views(1).asInstanceOf[DataView[T2, D2]],
      views(2).asInstanceOf[DataView[T3, D3]],
      views(3).asInstanceOf[DataView[T4, D4]],
      views(4).asInstanceOf[DataView[T5, D5]],
      views(5).asInstanceOf[DataView[T6, D6]],
      views(6).asInstanceOf[DataView[T7, D7]],
      views(7).asInstanceOf[DataView[T8, D8]],
      views(8).asInstanceOf[DataView[T9, D9]]
    )
  }

  def interleave[
    T1 <: ElemType, D1 <: RawType,
    T2 <: ElemType, D2 <: RawType,
    T3 <: ElemType, D3 <: RawType,
    T4 <: ElemType, D4 <: RawType,
    T5 <: ElemType, D5 <: RawType,
    T6 <: ElemType, D6 <: RawType,
    T7 <: ElemType, D7 <: RawType,
    T8 <: ElemType, D8 <: RawType,
    T9 <: ElemType, D9 <: RawType,
    T10 <: ElemType, D10 <: RawType
  ](
    size: Int,
    seq1: DataSeq[T1, D1],
    seq2: DataSeq[T2, D2],
    seq3: DataSeq[T3, D3],
    seq4: DataSeq[T4, D4],
    seq5: DataSeq[T5, D5],
    seq6: DataSeq[T6, D6],
    seq7: DataSeq[T7, D7],
    seq8: DataSeq[T8, D8],
    seq9: DataSeq[T9, D9],
    seq10: DataSeq[T10, D10]
  ) = {
    val views = interleaveAny(
      size,
      seq1, seq2, seq3, seq4, seq5, seq6, seq7, seq8, seq9, seq10
    )
    (
      views(0).asInstanceOf[DataView[T1, D1]],
      views(1).asInstanceOf[DataView[T2, D2]],
      views(2).asInstanceOf[DataView[T3, D3]],
      views(3).asInstanceOf[DataView[T4, D4]],
      views(4).asInstanceOf[DataView[T5, D5]],
      views(5).asInstanceOf[DataView[T6, D6]],
      views(6).asInstanceOf[DataView[T7, D7]],
      views(7).asInstanceOf[DataView[T8, D8]],
      views(8).asInstanceOf[DataView[T9, D9]],
      views(9).asInstanceOf[DataView[T10, D10]]
    )
  }

  def interleave[
    T1 <: ElemType, D1 <: RawType,
    T2 <: ElemType, D2 <: RawType,
    T3 <: ElemType, D3 <: RawType,
    T4 <: ElemType, D4 <: RawType,
    T5 <: ElemType, D5 <: RawType,
    T6 <: ElemType, D6 <: RawType,
    T7 <: ElemType, D7 <: RawType,
    T8 <: ElemType, D8 <: RawType,
    T9 <: ElemType, D9 <: RawType,
    T10 <: ElemType, D10 <: RawType,
    T11 <: ElemType, D11 <: RawType
  ](
    size: Int,
    seq1: DataSeq[T1, D1],
    seq2: DataSeq[T2, D2],
    seq3: DataSeq[T3, D3],
    seq4: DataSeq[T4, D4],
    seq5: DataSeq[T5, D5],
    seq6: DataSeq[T6, D6],
    seq7: DataSeq[T7, D7],
    seq8: DataSeq[T8, D8],
    seq9: DataSeq[T9, D9],
    seq10: DataSeq[T10, D10],
    seq11: DataSeq[T11, D11]
  ) = {
    val views = interleaveAny(
      size,
      seq1, seq2, seq3, seq4, seq5, seq6, seq7, seq8, seq9, seq10, seq11
    )
    (
      views(0).asInstanceOf[DataView[T1, D1]],
      views(1).asInstanceOf[DataView[T2, D2]],
      views(2).asInstanceOf[DataView[T3, D3]],
      views(3).asInstanceOf[DataView[T4, D4]],
      views(4).asInstanceOf[DataView[T5, D5]],
      views(5).asInstanceOf[DataView[T6, D6]],
      views(6).asInstanceOf[DataView[T7, D7]],
      views(7).asInstanceOf[DataView[T8, D8]],
      views(8).asInstanceOf[DataView[T9, D9]],
      views(9).asInstanceOf[DataView[T10, D10]],
      views(10).asInstanceOf[DataView[T11, D11]]
    )
  }

  def interleave[
    T1 <: ElemType, D1 <: RawType,
    T2 <: ElemType, D2 <: RawType,
    T3 <: ElemType, D3 <: RawType,
    T4 <: ElemType, D4 <: RawType,
    T5 <: ElemType, D5 <: RawType,
    T6 <: ElemType, D6 <: RawType,
    T7 <: ElemType, D7 <: RawType,
    T8 <: ElemType, D8 <: RawType,
    T9 <: ElemType, D9 <: RawType,
    T10 <: ElemType, D10 <: RawType,
    T11 <: ElemType, D11 <: RawType,
    T12 <: ElemType, D12 <: RawType
  ](
    size: Int,
    seq1: DataSeq[T1, D1],
    seq2: DataSeq[T2, D2],
    seq3: DataSeq[T3, D3],
    seq4: DataSeq[T4, D4],
    seq5: DataSeq[T5, D5],
    seq6: DataSeq[T6, D6],
    seq7: DataSeq[T7, D7],
    seq8: DataSeq[T8, D8],
    seq9: DataSeq[T9, D9],
    seq10: DataSeq[T10, D10],
    seq11: DataSeq[T11, D11],
    seq12: DataSeq[T12, D12]
  ) = {
    val views = interleaveAny(
      size,
      seq1, seq2, seq3, seq4, seq5, seq6, seq7, seq8, seq9, seq10, seq11, seq12
    )
    (
      views(0).asInstanceOf[DataView[T1, D1]],
      views(1).asInstanceOf[DataView[T2, D2]],
      views(2).asInstanceOf[DataView[T3, D3]],
      views(3).asInstanceOf[DataView[T4, D4]],
      views(4).asInstanceOf[DataView[T5, D5]],
      views(5).asInstanceOf[DataView[T6, D6]],
      views(6).asInstanceOf[DataView[T7, D7]],
      views(7).asInstanceOf[DataView[T8, D8]],
      views(8).asInstanceOf[DataView[T9, D9]],
      views(9).asInstanceOf[DataView[T10, D10]],
      views(10).asInstanceOf[DataView[T11, D11]],
      views(11).asInstanceOf[DataView[T12, D12]]
    )
  }


  def interleaveAny(
    size: Int,
    dataSeqs: DataSeq[_ <: ElemType, _ <: RawType]*
  )
  :Array[DataView[_, _]] =
  {
    // check arguments
    if (dataSeqs.length == 0) return new Array[DataView[_, _]](0)

    // verify size
    var i = 0; while(i < dataSeqs.length) {
      val s = dataSeqs(i).size
      if (s != 0 && s != size) throw new IllegalArgumentException(
        "All sequences must have the same size."
      )
      i += 1
    }

    // find maxComponentWidth and totalWidth
    var totalWidth = 0
    var maxComponentWidth = 1

    i = 0; while (i < dataSeqs.length) {
      val seq = dataSeqs(i)

      totalWidth += seq.bytesPerRawComponent*seq.components
      if (seq.bytesPerRawComponent > maxComponentWidth) {
        maxComponentWidth = seq.bytesPerRawComponent
      }

      i += 1
    }


    // order by data width
    val order = new Array[Int](dataSeqs.length)
    var count = 0
    
    var width = maxComponentWidth; while (width > 0) {

      i = 0; while (i < dataSeqs.length) {
        if (dataSeqs(i).bytesPerRawComponent == width) {
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
    val byteBuffer = allocateByteBuffer(byteStride*size)
    val result = new Array[DataView[_, _]](dataSeqs.length)
    var byteOffset = 0
    i = 0; while (i < dataSeqs.length) {
      val seq = dataSeqs(order(i))
      result(order(i)) = seq.copyAsDataView(
        byteBuffer,
        byteOffset/seq.bytesPerRawComponent,
        byteStride/seq.bytesPerRawComponent
      )
      byteOffset += seq.bytesPerRawComponent*seq.components
      
      i += 1
    }

    result
  }
}
