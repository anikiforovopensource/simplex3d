/*
 * Simplex3d, CoreData module
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

package simplex3d

import java.nio._
import simplex3d.math._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object data {

  private[this] final def primitiveFactory[R <: DefinedInt](f: PrimitiveFactory[SInt, R]) = f
  private[this] final def factory[F <: Meta](f: CompositionFactory[F, DefinedInt]) = f
  private[this] final val default = new ArraySIntSInt

  // SInt
  implicit final val FactorySIntSByte = primitiveFactory[SByte](new ArraySIntSByte)
  implicit final val FactorySIntUByte = primitiveFactory[UByte](new ArraySIntUByte)
  implicit final val FactorySIntSShort = primitiveFactory[SShort](new ArraySIntSShort)
  implicit final val FactorySIntUShort = primitiveFactory[UShort](new ArraySIntUShort)
  implicit final val FactorySIntSInt = primitiveFactory[SInt](default)
  implicit final val FactorySIntUInt = primitiveFactory[UInt](new ArraySIntUInt)

  // Composition
  implicit final val FactorySInt = factory[SInt](default)
  implicit final val FactoryVec2i = factory[Vec2i](new ArrayVec2i(default))
  implicit final val FactoryVec3i = factory[Vec3i](new ArrayVec3i(default))
  implicit final val FactoryVec4i = factory[Vec4i](new ArrayVec4i(default))


  type Format = integration.Format
  type CompressedFormat = integration.CompressedFormat
  type Meta = integration.Meta
  type PrimitiveFormat = integration.PrimitiveFormat
  type CompositeFormat = integration.CompositeFormat
  val PrimitiveFormat = integration.PrimitiveFormat
  type Raw = integration.Raw
  type Defined = integration.Defined
  type DefinedInt = integration.DefinedInt
  type DefinedIndex = integration.DefinedIndex
  type DefinedFloat = integration.DefinedFloat
  type DefinedDouble = integration.DefinedDouble
  type Integral = integration.Integral
  type Signed = integration.Signed
  type Unsigned = integration.Unsigned
  type RawByte = integration.RawByte
  type SByte = integration.SByte
  type UByte = integration.UByte
  type RawShort = integration.RawShort
  type SShort = integration.SShort
  type UShort = integration.UShort
  type RawInt = integration.RawInt
  type SInt = integration.SInt
  type UInt = integration.UInt
  type FloatingPoint = integration.FloatingPoint
  type SysFP = integration.SysFP
  type HFloat = integration.HFloat
  type RFloat = integration.RFloat
  type RDouble = integration.RDouble


  type inBatch[E] = ReadBatch[E]
  type outBatch[E] = Batch[E]
  
  type ReadIndex = ReadIndexSeq[Unsigned]
  type Index = IndexSeq[Unsigned]
  type inIndex = inIndexSeq[Unsigned]
  type outIndex = outIndexSeq[Unsigned]
  
  type RawView = ReadDataView[_ <: Meta, Raw]

  type inDataSeq[F <: Meta, +R <: Raw] = ReadDataSeq[F, R]
  type inContiguous[F <: Meta, +R <: Raw] = ReadContiguous[F, R]
  type inDataArray[F <: Meta, +R <: Raw] = ReadDataArray[F, R]
  type inDataBuffer[F <: Meta, +R <: Raw] = ReadDataBuffer[F, R]
  type inDataView[F <: Meta, +R <: Raw] = ReadDataView[F, R]

  type inIndexSeq[+R <: Unsigned] = ReadIndexSeq[R]
  type inIndexArray[+R <: Unsigned] = ReadIndexArray[R]
  type inIndexBuffer[+R <: Unsigned] = ReadIndexBuffer[R]

  type outDataSeq[F <: Meta, +R <: Raw] = DataSeq[F, R]
  type outContiguous[F <: Meta, +R <: Raw] = Contiguous[F, R]
  type outDataArray[F <: Meta, +R <: Raw] = DataArray[F, R]
  type outDataBuffer[F <: Meta, +R <: Raw] = DataBuffer[F, R]
  type outDataView[F <: Meta, +R <: Raw] = DataView[F, R]

  type outIndexSeq[+R <: Unsigned] = IndexSeq[R]
  type outIndexArray[+R <: Unsigned] = IndexArray[R]
  type outIndexBuffer[+R <: Unsigned] = IndexBuffer[R]

  @inline implicit final def readContegiousDataToIndex[R <: Unsigned] (
    d: ReadContiguous[SInt, R]
  ) = d.asInstanceOf[ReadIndexSeq[R]]

  @inline implicit final def readArrayDataToIndex[R <: Unsigned] (
    d: ReadDataArray[SInt, R]
  ) = d.asInstanceOf[ReadIndexArray[R]]

  @inline implicit final def readBufferDataToIndex[R <: Unsigned](
    d: ReadDataBuffer[SInt, R]
  ) = d.asInstanceOf[ReadIndexBuffer[R]]

  @inline implicit final def contegiousDataToIndex[R <: Unsigned] (
    d: Contiguous[SInt, R]
  ) = d.asInstanceOf[IndexSeq[R]]
  
  @inline implicit final def arrayDataToIndex[R <: Unsigned](
    d: DataArray[SInt, R]
  ) = d.asInstanceOf[IndexArray[R]]

  @inline implicit final def bufferDataToIndex[R <: Unsigned](
    d: DataBuffer[SInt, R]
  ) = d.asInstanceOf[IndexBuffer[R]]


  def interleave[
    E1 <: Meta, R1 <: Raw,
    E2 <: Meta, R2 <: Raw
  ](
    seq1: inDataSeq[E1, R1],
    seq2: inDataSeq[E2, R2]
  )(size: Int) = {
    val views = interleaveAll(seq1, seq2)(size)
    (
      views(0).asInstanceOf[DataView[E1, R1]],
      views(1).asInstanceOf[DataView[E2, R2]]
    )
  }

  def interleave[
    E1 <: Meta, R1 <: Raw,
    E2 <: Meta, R2 <: Raw,
    E3 <: Meta, R3 <: Raw
  ](
    seq1: inDataSeq[E1, R1],
    seq2: inDataSeq[E2, R2],
    seq3: inDataSeq[E3, R3]
  )(size: Int) = {
    val views = interleaveAll(seq1, seq2, seq3)(size)
    (
      views(0).asInstanceOf[DataView[E1, R1]],
      views(1).asInstanceOf[DataView[E2, R2]],
      views(2).asInstanceOf[DataView[E3, R3]]
    )
  }

  def interleave[
    E1 <: Meta, R1 <: Raw,
    E2 <: Meta, R2 <: Raw,
    E3 <: Meta, R3 <: Raw,
    E4 <: Meta, R4 <: Raw
  ](
    seq1: inDataSeq[E1, R1],
    seq2: inDataSeq[E2, R2],
    seq3: inDataSeq[E3, R3],
    seq4: inDataSeq[E4, R4]
  )(size: Int) = {
    val views = interleaveAll(
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
    E1 <: Meta, R1 <: Raw,
    E2 <: Meta, R2 <: Raw,
    E3 <: Meta, R3 <: Raw,
    E4 <: Meta, R4 <: Raw,
    E5 <: Meta, R5 <: Raw
  ](
    seq1: inDataSeq[E1, R1],
    seq2: inDataSeq[E2, R2],
    seq3: inDataSeq[E3, R3],
    seq4: inDataSeq[E4, R4],
    seq5: inDataSeq[E5, R5]
  )(size: Int) = {
    val views = interleaveAll(
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
    E1 <: Meta, R1 <: Raw,
    E2 <: Meta, R2 <: Raw,
    E3 <: Meta, R3 <: Raw,
    E4 <: Meta, R4 <: Raw,
    E5 <: Meta, R5 <: Raw,
    E6 <: Meta, R6 <: Raw
  ](
    seq1: inDataSeq[E1, R1],
    seq2: inDataSeq[E2, R2],
    seq3: inDataSeq[E3, R3],
    seq4: inDataSeq[E4, R4],
    seq5: inDataSeq[E5, R5],
    seq6: inDataSeq[E6, R6]
  )(size: Int) = {
    val views = interleaveAll(
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
    E1 <: Meta, R1 <: Raw,
    E2 <: Meta, R2 <: Raw,
    E3 <: Meta, R3 <: Raw,
    E4 <: Meta, R4 <: Raw,
    E5 <: Meta, R5 <: Raw,
    E6 <: Meta, R6 <: Raw,
    E7 <: Meta, R7 <: Raw
  ](
    seq1: inDataSeq[E1, R1],
    seq2: inDataSeq[E2, R2],
    seq3: inDataSeq[E3, R3],
    seq4: inDataSeq[E4, R4],
    seq5: inDataSeq[E5, R5],
    seq6: inDataSeq[E6, R6],
    seq7: inDataSeq[E7, R7]
  )(size: Int) = {
    val views = interleaveAll(
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
    E1 <: Meta, R1 <: Raw,
    E2 <: Meta, R2 <: Raw,
    E3 <: Meta, R3 <: Raw,
    E4 <: Meta, R4 <: Raw,
    E5 <: Meta, R5 <: Raw,
    E6 <: Meta, R6 <: Raw,
    E7 <: Meta, R7 <: Raw,
    E8 <: Meta, R8 <: Raw
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
    val views = interleaveAll(
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
    E1 <: Meta, R1 <: Raw,
    E2 <: Meta, R2 <: Raw,
    E3 <: Meta, R3 <: Raw,
    E4 <: Meta, R4 <: Raw,
    E5 <: Meta, R5 <: Raw,
    E6 <: Meta, R6 <: Raw,
    E7 <: Meta, R7 <: Raw,
    E8 <: Meta, R8 <: Raw,
    E9 <: Meta, R9 <: Raw
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
    val views = interleaveAll(
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
    E1 <: Meta, R1 <: Raw,
    E2 <: Meta, R2 <: Raw,
    E3 <: Meta, R3 <: Raw,
    E4 <: Meta, R4 <: Raw,
    E5 <: Meta, R5 <: Raw,
    E6 <: Meta, R6 <: Raw,
    E7 <: Meta, R7 <: Raw,
    E8 <: Meta, R8 <: Raw,
    E9 <: Meta, R9 <: Raw,
    E10 <: Meta, R10 <: Raw
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
    val views = interleaveAll(
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
    E1 <: Meta, R1 <: Raw,
    E2 <: Meta, R2 <: Raw,
    E3 <: Meta, R3 <: Raw,
    E4 <: Meta, R4 <: Raw,
    E5 <: Meta, R5 <: Raw,
    E6 <: Meta, R6 <: Raw,
    E7 <: Meta, R7 <: Raw,
    E8 <: Meta, R8 <: Raw,
    E9 <: Meta, R9 <: Raw,
    E10 <: Meta, R10 <: Raw,
    E11 <: Meta, R11 <: Raw
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
    val views = interleaveAll(
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
    E1 <: Meta, R1 <: Raw,
    E2 <: Meta, R2 <: Raw,
    E3 <: Meta, R3 <: Raw,
    E4 <: Meta, R4 <: Raw,
    E5 <: Meta, R5 <: Raw,
    E6 <: Meta, R6 <: Raw,
    E7 <: Meta, R7 <: Raw,
    E8 <: Meta, R8 <: Raw,
    E9 <: Meta, R9 <: Raw,
    E10 <: Meta, R10 <: Raw,
    E11 <: Meta, R11 <: Raw,
    E12 <: Meta, R12 <: Raw
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
    val views = interleaveAll(
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


  def interleaveAll(seqs: inDataSeq[_, Raw]*)(size: Int) :IndexedSeq[RawView] = {
    val dataSeqs = seqs.toArray

    // check arguments
    if (dataSeqs.length == 0) return new Array[RawView](0)

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

      totalWidth += seq.bytesPerComponent*seq.components
      if (seq.bytesPerComponent > maxComponentWidth) {
        maxComponentWidth = seq.bytesPerComponent
      }

      i += 1
    }


    // order by data width
    val order = new Array[Int](dataSeqs.length)
    var count = 0
    
    var width = maxComponentWidth; while (width > 0) {

      i = 0; while (i < dataSeqs.length) {
        if (dataSeqs(i).bytesPerComponent == width) {
          order(count) = i
          count += 1
        }

        i += 1
      }

      width /= 2
    }

    // find pad value
    var pad = totalWidth % maxComponentWidth
    if (pad > 0) pad = maxComponentWidth - pad
    val byteStride = totalWidth + pad

    // generate
    val byteBuffer = ByteBuffer.allocateDirect(byteStride*size)
    val result = new Array[RawView](dataSeqs.length)
    var byteOffset = 0

    i = 0; while (i < dataSeqs.length) {

      type T = F forSome { type F <: Meta }
      val seq = dataSeqs(order(i)).asInstanceOf[DataSeq[T, Raw]]
      val view = seq.mkDataView(
        byteBuffer,
        byteOffset/seq.bytesPerComponent,
        byteStride/seq.bytesPerComponent
      )
      view.put(seq)
      result(order(i)) = view
      byteOffset += seq.bytesPerComponent*seq.components
      
      i += 1
    }

    result
  }
}
