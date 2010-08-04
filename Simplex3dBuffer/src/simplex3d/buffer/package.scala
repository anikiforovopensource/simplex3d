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
import simplex3d.math._


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

  type MetaElement = integration.buffer.MetaElement
  type Primitive = integration.buffer.Primitive
  type Composite = integration.buffer.Composite
  type Int1 = integration.buffer.Int1
  type Float1 = integration.buffer.Float1
  type Double1 = integration.buffer.Double1

  type inDataSeq[E <: MetaElement, +R <: RawData] = ReadDataSeq[E, R]
  type inContiguousSeq[E <: MetaElement, +R <: RawData] =ReadContiguousSeq[E, R]
  type inDataArray[E <: MetaElement, +R <: RawData] = ReadDataArray[E, R]
  type inDataBuffer[E <: MetaElement, +R <: RawData] = ReadDataBuffer[E, R]
  type inDataView[E <: MetaElement, +R <: RawData] = ReadDataView[E, R]

  type inIndexSeq[+R <: RawData] = ReadIndexSeq[R]
  type inIndexArray[+R <: RawData] = ReadIndexArray[R]
  type inIndexBuffer[+R <: RawData] = ReadIndexBuffer[R]

  type outDataSeq[E <: MetaElement, +R <: RawData] = DataSeq[E, R]
  type outContiguousSeq[E <: MetaElement, +R <: RawData] =ContiguousSeq[E, R]
  type outDataArray[E <: MetaElement, +R <: RawData] = DataArray[E, R]
  type outDataBuffer[E <: MetaElement, +R <: RawData] = DataBuffer[E, R]
  type outDataView[E <: MetaElement, +R <: RawData] = DataView[E, R]

  type outIndexSeq[+R <: RawData] = IndexSeq[R]
  type outIndexArray[+R <: RawData] = IndexArray[R]
  type outIndexBuffer[+R <: RawData] = IndexBuffer[R]

  @inline implicit final def readContegiousDataToIndex[R  <: ReadableIndex] (
    d: ReadContiguousSeq[Int1, R]
  ) = d.asInstanceOf[ReadIndexSeq[R]]

  @inline implicit final def readArrayDataToIndex[R  <: ReadableIndex] (
    d: ReadDataArray[Int1, R]
  ) = d.asInstanceOf[ReadIndexArray[R]]

  @inline implicit final def readBufferDataToIndex[R  <: ReadableIndex](
    d: ReadDataBuffer[Int1, R]
  ) = d.asInstanceOf[ReadIndexBuffer[R]]

  @inline implicit final def contegiousDataToIndex[R  <: ReadableIndex] (
    d: ContiguousSeq[Int1, R]
  ) = d.asInstanceOf[IndexSeq[R]]
  
  @inline implicit final def arrayDataToIndex[R  <: ReadableIndex](
    d: DataArray[Int1, R]
  ) = d.asInstanceOf[IndexArray[R]]

  @inline implicit final def bufferDataToIndex[R  <: ReadableIndex](
    d: DataBuffer[Int1, R]
  ) = d.asInstanceOf[IndexBuffer[R]]

  
  def allocateByteBuffer(size: Int) = {
    val direct = ByteBuffer.allocateDirect(size)
    direct.order(ByteOrder.nativeOrder())
  }
  
  def interleave[
    E1 <: MetaElement, R1 <: RawData,
    E2 <: MetaElement, R2 <: RawData
  ](
    seq1: inDataSeq[E1, R1],
    seq2: inDataSeq[E2, R2]
  )(size: Int) = {
    val views = interleaveAny(seq1, seq2)(size)
    (
      views(0).asInstanceOf[DataView[E1, R1]],
      views(1).asInstanceOf[DataView[E2, R2]]
    )
  }

  def interleave[
    E1 <: MetaElement, R1 <: RawData,
    E2 <: MetaElement, R2 <: RawData,
    E3 <: MetaElement, R3 <: RawData
  ](
    seq1: inDataSeq[E1, R1],
    seq2: inDataSeq[E2, R2],
    seq3: inDataSeq[E3, R3]
  )(size: Int) = {
    val views = interleaveAny(seq1, seq2, seq3)(size)
    (
      views(0).asInstanceOf[DataView[E1, R1]],
      views(1).asInstanceOf[DataView[E2, R2]],
      views(2).asInstanceOf[DataView[E3, R3]]
    )
  }

  def interleave[
    E1 <: MetaElement, R1 <: RawData,
    E2 <: MetaElement, R2 <: RawData,
    E3 <: MetaElement, R3 <: RawData,
    E4 <: MetaElement, R4 <: RawData
  ](
    seq1: inDataSeq[E1, R1],
    seq2: inDataSeq[E2, R2],
    seq3: inDataSeq[E3, R3],
    seq4: inDataSeq[E4, R4]
  )(size: Int) = {
    val views = interleaveAny(
      seq1, seq2, seq3, seq4
    )(size)
    (
      views(0).asInstanceOf[DataView[E1, R1]],
      views(1).asInstanceOf[DataView[E2, R2]],
      views(2).asInstanceOf[DataView[E3, R3]],
      views(3).asInstanceOf[DataView[E4, R4]]
    )
  }

  def interleave[
    E1 <: MetaElement, R1 <: RawData,
    E2 <: MetaElement, R2 <: RawData,
    E3 <: MetaElement, R3 <: RawData,
    E4 <: MetaElement, R4 <: RawData,
    E5 <: MetaElement, R5 <: RawData
  ](
    seq1: inDataSeq[E1, R1],
    seq2: inDataSeq[E2, R2],
    seq3: inDataSeq[E3, R3],
    seq4: inDataSeq[E4, R4],
    seq5: inDataSeq[E5, R5]
  )(size: Int) = {
    val views = interleaveAny(
      seq1, seq2, seq3, seq4, seq5
    )(size)
    (
      views(0).asInstanceOf[DataView[E1, R1]],
      views(1).asInstanceOf[DataView[E2, R2]],
      views(2).asInstanceOf[DataView[E3, R3]],
      views(3).asInstanceOf[DataView[E4, R4]],
      views(4).asInstanceOf[DataView[E5, R5]]
    )
  }

  def interleave[
    E1 <: MetaElement, R1 <: RawData,
    E2 <: MetaElement, R2 <: RawData,
    E3 <: MetaElement, R3 <: RawData,
    E4 <: MetaElement, R4 <: RawData,
    E5 <: MetaElement, R5 <: RawData,
    E6 <: MetaElement, R6 <: RawData
  ](
    seq1: inDataSeq[E1, R1],
    seq2: inDataSeq[E2, R2],
    seq3: inDataSeq[E3, R3],
    seq4: inDataSeq[E4, R4],
    seq5: inDataSeq[E5, R5],
    seq6: inDataSeq[E6, R6]
  )(size: Int) = {
    val views = interleaveAny(
      seq1, seq2, seq3, seq4, seq5, seq6
    )(size)
    (
      views(0).asInstanceOf[DataView[E1, R1]],
      views(1).asInstanceOf[DataView[E2, R2]],
      views(2).asInstanceOf[DataView[E3, R3]],
      views(3).asInstanceOf[DataView[E4, R4]],
      views(4).asInstanceOf[DataView[E5, R5]],
      views(5).asInstanceOf[DataView[E6, R6]]
    )
  }

  def interleave[
    E1 <: MetaElement, R1 <: RawData,
    E2 <: MetaElement, R2 <: RawData,
    E3 <: MetaElement, R3 <: RawData,
    E4 <: MetaElement, R4 <: RawData,
    E5 <: MetaElement, R5 <: RawData,
    E6 <: MetaElement, R6 <: RawData,
    E7 <: MetaElement, R7 <: RawData
  ](
    seq1: inDataSeq[E1, R1],
    seq2: inDataSeq[E2, R2],
    seq3: inDataSeq[E3, R3],
    seq4: inDataSeq[E4, R4],
    seq5: inDataSeq[E5, R5],
    seq6: inDataSeq[E6, R6],
    seq7: inDataSeq[E7, R7]
  )(size: Int) = {
    val views = interleaveAny(
      seq1, seq2, seq3, seq4, seq5, seq6, seq7
    )(size)
    (
      views(0).asInstanceOf[DataView[E1, R1]],
      views(1).asInstanceOf[DataView[E2, R2]],
      views(2).asInstanceOf[DataView[E3, R3]],
      views(3).asInstanceOf[DataView[E4, R4]],
      views(4).asInstanceOf[DataView[E5, R5]],
      views(5).asInstanceOf[DataView[E6, R6]],
      views(6).asInstanceOf[DataView[E7, R7]]
    )
  }

  def interleave[
    E1 <: MetaElement, R1 <: RawData,
    E2 <: MetaElement, R2 <: RawData,
    E3 <: MetaElement, R3 <: RawData,
    E4 <: MetaElement, R4 <: RawData,
    E5 <: MetaElement, R5 <: RawData,
    E6 <: MetaElement, R6 <: RawData,
    E7 <: MetaElement, R7 <: RawData,
    E8 <: MetaElement, R8 <: RawData
  ](
    seq1: inDataSeq[E1, R1],
    seq2: inDataSeq[E2, R2],
    seq3: inDataSeq[E3, R3],
    seq4: inDataSeq[E4, R4],
    seq5: inDataSeq[E5, R5],
    seq6: inDataSeq[E6, R6],
    seq7: inDataSeq[E7, R7],
    seq8: inDataSeq[E8, R8]
  )(size: Int) = {
    val views = interleaveAny(
      seq1, seq2, seq3, seq4, seq5, seq6, seq7, seq8
    )(size)
    (
      views(0).asInstanceOf[DataView[E1, R1]],
      views(1).asInstanceOf[DataView[E2, R2]],
      views(2).asInstanceOf[DataView[E3, R3]],
      views(3).asInstanceOf[DataView[E4, R4]],
      views(4).asInstanceOf[DataView[E5, R5]],
      views(5).asInstanceOf[DataView[E6, R6]],
      views(6).asInstanceOf[DataView[E7, R7]],
      views(7).asInstanceOf[DataView[E8, R8]]
    )
  }

  def interleave[
    E1 <: MetaElement, R1 <: RawData,
    E2 <: MetaElement, R2 <: RawData,
    E3 <: MetaElement, R3 <: RawData,
    E4 <: MetaElement, R4 <: RawData,
    E5 <: MetaElement, R5 <: RawData,
    E6 <: MetaElement, R6 <: RawData,
    E7 <: MetaElement, R7 <: RawData,
    E8 <: MetaElement, R8 <: RawData,
    E9 <: MetaElement, R9 <: RawData
  ](
    seq1: inDataSeq[E1, R1],
    seq2: inDataSeq[E2, R2],
    seq3: inDataSeq[E3, R3],
    seq4: inDataSeq[E4, R4],
    seq5: inDataSeq[E5, R5],
    seq6: inDataSeq[E6, R6],
    seq7: inDataSeq[E7, R7],
    seq8: inDataSeq[E8, R8],
    seq9: inDataSeq[E9, R9]
  )(size: Int) = {
    val views = interleaveAny(
      seq1, seq2, seq3, seq4, seq5, seq6, seq7, seq8, seq9
    )(size)
    (
      views(0).asInstanceOf[DataView[E1, R1]],
      views(1).asInstanceOf[DataView[E2, R2]],
      views(2).asInstanceOf[DataView[E3, R3]],
      views(3).asInstanceOf[DataView[E4, R4]],
      views(4).asInstanceOf[DataView[E5, R5]],
      views(5).asInstanceOf[DataView[E6, R6]],
      views(6).asInstanceOf[DataView[E7, R7]],
      views(7).asInstanceOf[DataView[E8, R8]],
      views(8).asInstanceOf[DataView[E9, R9]]
    )
  }

  def interleave[
    E1 <: MetaElement, R1 <: RawData,
    E2 <: MetaElement, R2 <: RawData,
    E3 <: MetaElement, R3 <: RawData,
    E4 <: MetaElement, R4 <: RawData,
    E5 <: MetaElement, R5 <: RawData,
    E6 <: MetaElement, R6 <: RawData,
    E7 <: MetaElement, R7 <: RawData,
    E8 <: MetaElement, R8 <: RawData,
    E9 <: MetaElement, R9 <: RawData,
    E10 <: MetaElement, R10 <: RawData
  ](
    seq1: inDataSeq[E1, R1],
    seq2: inDataSeq[E2, R2],
    seq3: inDataSeq[E3, R3],
    seq4: inDataSeq[E4, R4],
    seq5: inDataSeq[E5, R5],
    seq6: inDataSeq[E6, R6],
    seq7: inDataSeq[E7, R7],
    seq8: inDataSeq[E8, R8],
    seq9: inDataSeq[E9, R9],
    seq10: inDataSeq[E10, R10]
  )(size: Int) = {
    val views = interleaveAny(
      seq1, seq2, seq3, seq4, seq5, seq6, seq7, seq8, seq9, seq10
    )(size)
    (
      views(0).asInstanceOf[DataView[E1, R1]],
      views(1).asInstanceOf[DataView[E2, R2]],
      views(2).asInstanceOf[DataView[E3, R3]],
      views(3).asInstanceOf[DataView[E4, R4]],
      views(4).asInstanceOf[DataView[E5, R5]],
      views(5).asInstanceOf[DataView[E6, R6]],
      views(6).asInstanceOf[DataView[E7, R7]],
      views(7).asInstanceOf[DataView[E8, R8]],
      views(8).asInstanceOf[DataView[E9, R9]],
      views(9).asInstanceOf[DataView[E10, R10]]
    )
  }

  def interleave[
    E1 <: MetaElement, R1 <: RawData,
    E2 <: MetaElement, R2 <: RawData,
    E3 <: MetaElement, R3 <: RawData,
    E4 <: MetaElement, R4 <: RawData,
    E5 <: MetaElement, R5 <: RawData,
    E6 <: MetaElement, R6 <: RawData,
    E7 <: MetaElement, R7 <: RawData,
    E8 <: MetaElement, R8 <: RawData,
    E9 <: MetaElement, R9 <: RawData,
    E10 <: MetaElement, R10 <: RawData,
    E11 <: MetaElement, R11 <: RawData
  ](
    seq1: inDataSeq[E1, R1],
    seq2: inDataSeq[E2, R2],
    seq3: inDataSeq[E3, R3],
    seq4: inDataSeq[E4, R4],
    seq5: inDataSeq[E5, R5],
    seq6: inDataSeq[E6, R6],
    seq7: inDataSeq[E7, R7],
    seq8: inDataSeq[E8, R8],
    seq9: inDataSeq[E9, R9],
    seq10: inDataSeq[E10, R10],
    seq11: inDataSeq[E11, R11]
  )(size: Int) = {
    val views = interleaveAny(
      seq1, seq2, seq3, seq4, seq5, seq6, seq7, seq8, seq9, seq10, seq11
    )(size)
    (
      views(0).asInstanceOf[DataView[E1, R1]],
      views(1).asInstanceOf[DataView[E2, R2]],
      views(2).asInstanceOf[DataView[E3, R3]],
      views(3).asInstanceOf[DataView[E4, R4]],
      views(4).asInstanceOf[DataView[E5, R5]],
      views(5).asInstanceOf[DataView[E6, R6]],
      views(6).asInstanceOf[DataView[E7, R7]],
      views(7).asInstanceOf[DataView[E8, R8]],
      views(8).asInstanceOf[DataView[E9, R9]],
      views(9).asInstanceOf[DataView[E10, R10]],
      views(10).asInstanceOf[DataView[E11, R11]]
    )
  }

  def interleave[
    E1 <: MetaElement, R1 <: RawData,
    E2 <: MetaElement, R2 <: RawData,
    E3 <: MetaElement, R3 <: RawData,
    E4 <: MetaElement, R4 <: RawData,
    E5 <: MetaElement, R5 <: RawData,
    E6 <: MetaElement, R6 <: RawData,
    E7 <: MetaElement, R7 <: RawData,
    E8 <: MetaElement, R8 <: RawData,
    E9 <: MetaElement, R9 <: RawData,
    E10 <: MetaElement, R10 <: RawData,
    E11 <: MetaElement, R11 <: RawData,
    E12 <: MetaElement, R12 <: RawData
  ](
    seq1: inDataSeq[E1, R1],
    seq2: inDataSeq[E2, R2],
    seq3: inDataSeq[E3, R3],
    seq4: inDataSeq[E4, R4],
    seq5: inDataSeq[E5, R5],
    seq6: inDataSeq[E6, R6],
    seq7: inDataSeq[E7, R7],
    seq8: inDataSeq[E8, R8],
    seq9: inDataSeq[E9, R9],
    seq10: inDataSeq[E10, R10],
    seq11: inDataSeq[E11, R11],
    seq12: inDataSeq[E12, R12]
  )(size: Int) = {
    val views = interleaveAny(
      seq1, seq2, seq3, seq4, seq5, seq6, seq7, seq8, seq9, seq10, seq11, seq12
    )(size)
    (
      views(0).asInstanceOf[DataView[E1, R1]],
      views(1).asInstanceOf[DataView[E2, R2]],
      views(2).asInstanceOf[DataView[E3, R3]],
      views(3).asInstanceOf[DataView[E4, R4]],
      views(4).asInstanceOf[DataView[E5, R5]],
      views(5).asInstanceOf[DataView[E6, R6]],
      views(6).asInstanceOf[DataView[E7, R7]],
      views(7).asInstanceOf[DataView[E8, R8]],
      views(8).asInstanceOf[DataView[E9, R9]],
      views(9).asInstanceOf[DataView[E10, R10]],
      views(10).asInstanceOf[DataView[E11, R11]],
      views(11).asInstanceOf[DataView[E12, R12]]
    )
  }


  def interleaveAny(
    dataSeqs: inDataSeq[_ <: MetaElement, _ <: RawData]*
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
