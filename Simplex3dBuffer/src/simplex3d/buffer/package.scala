/*
 * Simplex3d, BaseBuffer module
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

package simplex3d

import java.nio._


/**
 * @author Aleksey Nikiforov (lex)
 */
package buffer {
  private[buffer] class UnsignedImplicits {

    private final type PrimitiveFactory[R <: ReadableInt] =
      PrimitiveFactoryRef[Int1, R]

    
    implicit final val FactoryInt1SByte = new PrimitiveFactory[SByte](
      "simplex3d.buffer.ArrayInt1SByte"
    )
    implicit final val FactoryInt1UByte = new PrimitiveFactory[UByte](
      "simplex3d.buffer.ArrayInt1UByte"
    )

    implicit final val FactoryInt1SShort = new PrimitiveFactory[SShort](
      "simplex3d.buffer.ArrayInt1SShort"
    )
    implicit final val FactoryInt1UShort = new PrimitiveFactory[UShort](
      "simplex3d.buffer.ArrayInt1UShort"
    )

    implicit final val FactoryInt1SInt = new PrimitiveFactory[SInt](
      "simplex3d.buffer.ArrayInt1SInt"
    )
    implicit final val FactoryInt1UInt = new PrimitiveFactory[UInt](
      "simplex3d.buffer.ArrayInt1UInt"
    )
  }
}

package object buffer extends UnsignedImplicits {

  type ElemType = simplex3d.math.types.ElemType
  type Primitive = simplex3d.math.types.Primitive
  type Composite = simplex3d.math.types.Composite
  type Int1 = simplex3d.math.types.Int1
  type Float1 = simplex3d.math.types.Float1
  type Double1 = simplex3d.math.types.Double1

  type inDataSeq[T <: ElemType, +D <: RawType] = ReadDataSeq[T, D]
  type inContiguousSeq[T <: ElemType, +D <: RawType] =ReadContiguousSeq[T, D]
  type inDataArray[T <: ElemType, +D <: RawType] = ReadDataArray[T, D]
  type inDataBuffer[T <: ElemType, +D <: RawType] = ReadDataBuffer[T, D]
  type inDataView[T <: ElemType, +D <: RawType] = ReadDataView[T, D]

  type inIndexSeq[+D <: ReadableIndex] = ReadIndexSeq[D]
  type inIndexArray[+D <: ReadableIndex] = ReadIndexArray[D]
  type inIndexBuffer[+D <: ReadableIndex] = ReadIndexBuffer[D]

  type outDataSeq[T <: ElemType, +D <: RawType] = DataSeq[T, D]
  type outContiguousSeq[T <: ElemType, +D <: RawType] =ContiguousSeq[T, D]
  type outDataArray[T <: ElemType, +D <: RawType] = DataArray[T, D]
  type outDataBuffer[T <: ElemType, +D <: RawType] = DataBuffer[T, D]
  type outDataView[T <: ElemType, +D <: RawType] = DataView[T, D]

  type outIndexSeq[+D <: ReadableIndex] = IndexSeq[D]
  type outIndexArray[+D <: ReadableIndex] = IndexArray[D]
  type outIndexBuffer[+D <: ReadableIndex] = IndexBuffer[D]

  @inline implicit final def roArrayDataToIndex[D  <: ReadableIndex] (
    d: ReadDataArray[Int1, D]
  ) = d.asInstanceOf[ReadIndexArray[D]]

  @inline implicit final def roBufferDataToIndex[D  <: ReadableIndex](
    d: ReadDataBuffer[Int1, D]
  ) = d.asInstanceOf[ReadIndexBuffer[D]]

  
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
    seq1: DataSeq[T1, D1],
    seq2: DataSeq[T2, D2]
  )(size: Int) = {
    val views = interleaveAny(seq1, seq2)(size)
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
    seq1: DataSeq[T1, D1],
    seq2: DataSeq[T2, D2],
    seq3: DataSeq[T3, D3]
  )(size: Int) = {
    val views = interleaveAny(seq1, seq2, seq3)(size)
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
    seq1: DataSeq[T1, D1],
    seq2: DataSeq[T2, D2],
    seq3: DataSeq[T3, D3],
    seq4: DataSeq[T4, D4]
  )(size: Int) = {
    val views = interleaveAny(
      seq1, seq2, seq3, seq4
    )(size)
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
    seq1: DataSeq[T1, D1],
    seq2: DataSeq[T2, D2],
    seq3: DataSeq[T3, D3],
    seq4: DataSeq[T4, D4],
    seq5: DataSeq[T5, D5]
  )(size: Int) = {
    val views = interleaveAny(
      seq1, seq2, seq3, seq4, seq5
    )(size)
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
    seq1: DataSeq[T1, D1],
    seq2: DataSeq[T2, D2],
    seq3: DataSeq[T3, D3],
    seq4: DataSeq[T4, D4],
    seq5: DataSeq[T5, D5],
    seq6: DataSeq[T6, D6]
  )(size: Int) = {
    val views = interleaveAny(
      seq1, seq2, seq3, seq4, seq5, seq6
    )(size)
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
    seq1: DataSeq[T1, D1],
    seq2: DataSeq[T2, D2],
    seq3: DataSeq[T3, D3],
    seq4: DataSeq[T4, D4],
    seq5: DataSeq[T5, D5],
    seq6: DataSeq[T6, D6],
    seq7: DataSeq[T7, D7]
  )(size: Int) = {
    val views = interleaveAny(
      seq1, seq2, seq3, seq4, seq5, seq6, seq7
    )(size)
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
    seq1: DataSeq[T1, D1],
    seq2: DataSeq[T2, D2],
    seq3: DataSeq[T3, D3],
    seq4: DataSeq[T4, D4],
    seq5: DataSeq[T5, D5],
    seq6: DataSeq[T6, D6],
    seq7: DataSeq[T7, D7],
    seq8: DataSeq[T8, D8]
  )(size: Int) = {
    val views = interleaveAny(
      seq1, seq2, seq3, seq4, seq5, seq6, seq7, seq8
    )(size)
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
    seq1: DataSeq[T1, D1],
    seq2: DataSeq[T2, D2],
    seq3: DataSeq[T3, D3],
    seq4: DataSeq[T4, D4],
    seq5: DataSeq[T5, D5],
    seq6: DataSeq[T6, D6],
    seq7: DataSeq[T7, D7],
    seq8: DataSeq[T8, D8],
    seq9: DataSeq[T9, D9]
  )(size: Int) = {
    val views = interleaveAny(
      seq1, seq2, seq3, seq4, seq5, seq6, seq7, seq8, seq9
    )(size)
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
  )(size: Int) = {
    val views = interleaveAny(
      seq1, seq2, seq3, seq4, seq5, seq6, seq7, seq8, seq9, seq10
    )(size)
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
  )(size: Int) = {
    val views = interleaveAny(
      seq1, seq2, seq3, seq4, seq5, seq6, seq7, seq8, seq9, seq10, seq11
    )(size)
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
  )(size: Int) = {
    val views = interleaveAny(
      seq1, seq2, seq3, seq4, seq5, seq6, seq7, seq8, seq9, seq10, seq11, seq12
    )(size)
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
    dataSeqs: DataSeq[_ <: ElemType, _ <: RawType]*
  )(size: Int) :Array[DataView[_, _]] = {
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
