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

package simplex3d

import java.nio._
import simplex3d.math._
import simplex3d.data.extension._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object data {

  private[this] final def primitiveFactory[R <: TangibleInt](f: PrimitiveFactory[SInt, R]) = f
  private[this] final def factory[F <: Format](f: CompositionFactory[F, TangibleInt]) = f
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


  type FormatBound[F <: Format] = DataSrc { type Format = F }
  type AccessorBound[A <: Accessor] = DataSrc { type Format <: simplex3d.data.Format { type Accessor = A } }

  
  type Accessor = integration.Accessor
  type Format = integration.Format
  type PrimitiveFormat = integration.PrimitiveFormat
  type CompositeFormat = integration.CompositeFormat
  type CompressedFormat = integration.CompressedFormat
  val PrimitiveFormat = integration.PrimitiveFormat
  type Raw = integration.Raw
  type Tangible = integration.Tangible
  type TangibleInt = integration.TangibleInt
  type TangibleIndex = integration.TangibleIndex
  type TangibleFloat = integration.TangibleFloat
  type TangibleDouble = integration.TangibleDouble
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

  type RawView = ReadDataView[_ <: Format, _ <: Raw]
  
  type ReadIndex = ReadContiguous[SInt, Unsigned]
  type Index = Contiguous[SInt, Unsigned]
  type inIndex = inContiguous[SInt, Unsigned]
  
  type ReadIndexArray = ReadDataArray[SInt, Unsigned]
  type IndexArray = DataArray[SInt, Unsigned]
  type inIndexArray = inDataArray[SInt, Unsigned]
  
  type ReadIndexBuffer = ReadDataBuffer[SInt, Unsigned]
  type IndexBuffer = DataBuffer[SInt, Unsigned]
  type inIndexBuffer = inDataBuffer[SInt, Unsigned]
  
  def IndexArray(max: Int, size: Int) :IndexArray = {
    if (max < 256) DataArray[SInt, UByte](size)
    else if (max < 65536) DataArray[SInt, UShort](size)
    else DataArray[SInt, UInt](size)
  }
  
  def IndexBuffer(max: Int, size: Int) :IndexBuffer = {
    if (max < 256) DataBuffer[SInt, UByte](size)
    else if (max < 65536) DataBuffer[SInt, UShort](size)
    else DataBuffer[SInt, UInt](size)
  }
  
  
  type inData[A <: Accessor] = ReadData[A]

  type inDataSeq[F <: Format, +R <: Raw] = ReadDataSeq[F, R]
  type inContiguous[F <: Format, +R <: Raw] = ReadContiguous[F, R]
  type inDataArray[F <: Format, +R <: Raw] = ReadDataArray[F, R]
  type inDataBuffer[F <: Format, +R <: Raw] = ReadDataBuffer[F, R]
  type inDataView[F <: Format, +R <: Raw] = ReadDataView[F, R]

  
  def interleave[
    F1 <: Format, R1 <: Raw,
    F2 <: Format, R2 <: Raw
  ](
    seq1: inDataSeq[F1, R1],
    seq2: inDataSeq[F2, R2]
  )(size: Int) = {
    val views = interleaveAll(seq1, seq2)(size)
    (
      views(0).asInstanceOf[DataView[F1, R1]],
      views(1).asInstanceOf[DataView[F2, R2]]
    )
  }

  def interleave[
    F1 <: Format, R1 <: Raw,
    F2 <: Format, R2 <: Raw,
    F3 <: Format, R3 <: Raw
  ](
    seq1: inDataSeq[F1, R1],
    seq2: inDataSeq[F2, R2],
    seq3: inDataSeq[F3, R3]
  )(size: Int) = {
    val views = interleaveAll(seq1, seq2, seq3)(size)
    (
      views(0).asInstanceOf[DataView[F1, R1]],
      views(1).asInstanceOf[DataView[F2, R2]],
      views(2).asInstanceOf[DataView[F3, R3]]
    )
  }

  def interleave[
    F1 <: Format, R1 <: Raw,
    F2 <: Format, R2 <: Raw,
    F3 <: Format, R3 <: Raw,
    F4 <: Format, R4 <: Raw
  ](
    seq1: inDataSeq[F1, R1],
    seq2: inDataSeq[F2, R2],
    seq3: inDataSeq[F3, R3],
    seq4: inDataSeq[F4, R4]
  )(size: Int) = {
    val views = interleaveAll(
      seq1, seq2, seq3, seq4
    )(size)
    (
      views(0).asInstanceOf[DataView[F1, R1]],
      views(1).asInstanceOf[DataView[F2, R2]],
      views(2).asInstanceOf[DataView[F3, R3]],
      views(3).asInstanceOf[DataView[F4, R4]]
    )
  }

  def interleave[
    F1 <: Format, R1 <: Raw,
    F2 <: Format, R2 <: Raw,
    F3 <: Format, R3 <: Raw,
    F4 <: Format, R4 <: Raw,
    F5 <: Format, R5 <: Raw
  ](
    seq1: inDataSeq[F1, R1],
    seq2: inDataSeq[F2, R2],
    seq3: inDataSeq[F3, R3],
    seq4: inDataSeq[F4, R4],
    seq5: inDataSeq[F5, R5]
  )(size: Int) = {
    val views = interleaveAll(
      seq1, seq2, seq3, seq4, seq5
    )(size)
    (
      views(0).asInstanceOf[DataView[F1, R1]],
      views(1).asInstanceOf[DataView[F2, R2]],
      views(2).asInstanceOf[DataView[F3, R3]],
      views(3).asInstanceOf[DataView[F4, R4]],
      views(4).asInstanceOf[DataView[F5, R5]]
    )
  }

  def interleave[
    F1 <: Format, R1 <: Raw,
    F2 <: Format, R2 <: Raw,
    F3 <: Format, R3 <: Raw,
    F4 <: Format, R4 <: Raw,
    F5 <: Format, R5 <: Raw,
    F6 <: Format, R6 <: Raw
  ](
    seq1: inDataSeq[F1, R1],
    seq2: inDataSeq[F2, R2],
    seq3: inDataSeq[F3, R3],
    seq4: inDataSeq[F4, R4],
    seq5: inDataSeq[F5, R5],
    seq6: inDataSeq[F6, R6]
  )(size: Int) = {
    val views = interleaveAll(
      seq1, seq2, seq3, seq4, seq5, seq6
    )(size)
    (
      views(0).asInstanceOf[DataView[F1, R1]],
      views(1).asInstanceOf[DataView[F2, R2]],
      views(2).asInstanceOf[DataView[F3, R3]],
      views(3).asInstanceOf[DataView[F4, R4]],
      views(4).asInstanceOf[DataView[F5, R5]],
      views(5).asInstanceOf[DataView[F6, R6]]
    )
  }

  def interleave[
    F1 <: Format, R1 <: Raw,
    F2 <: Format, R2 <: Raw,
    F3 <: Format, R3 <: Raw,
    F4 <: Format, R4 <: Raw,
    F5 <: Format, R5 <: Raw,
    F6 <: Format, R6 <: Raw,
    F7 <: Format, R7 <: Raw
  ](
    seq1: inDataSeq[F1, R1],
    seq2: inDataSeq[F2, R2],
    seq3: inDataSeq[F3, R3],
    seq4: inDataSeq[F4, R4],
    seq5: inDataSeq[F5, R5],
    seq6: inDataSeq[F6, R6],
    seq7: inDataSeq[F7, R7]
  )(size: Int) = {
    val views = interleaveAll(
      seq1, seq2, seq3, seq4, seq5, seq6, seq7
    )(size)
    (
      views(0).asInstanceOf[DataView[F1, R1]],
      views(1).asInstanceOf[DataView[F2, R2]],
      views(2).asInstanceOf[DataView[F3, R3]],
      views(3).asInstanceOf[DataView[F4, R4]],
      views(4).asInstanceOf[DataView[F5, R5]],
      views(5).asInstanceOf[DataView[F6, R6]],
      views(6).asInstanceOf[DataView[F7, R7]]
    )
  }

  def interleave[
    F1 <: Format, R1 <: Raw,
    F2 <: Format, R2 <: Raw,
    F3 <: Format, R3 <: Raw,
    F4 <: Format, R4 <: Raw,
    F5 <: Format, R5 <: Raw,
    F6 <: Format, R6 <: Raw,
    F7 <: Format, R7 <: Raw,
    F8 <: Format, R8 <: Raw
  ](
    seq1: inDataSeq[F1, R1],
    seq2: inDataSeq[F2, R2],
    seq3: inDataSeq[F3, R3],
    seq4: inDataSeq[F4, R4],
    seq5: inDataSeq[F5, R5],
    seq6: inDataSeq[F6, R6],
    seq7: inDataSeq[F7, R7],
    seq8: inDataSeq[F8, R8]
  )(size: Int) = {
    val views = interleaveAll(
      seq1, seq2, seq3, seq4, seq5, seq6, seq7, seq8
    )(size)
    (
      views(0).asInstanceOf[DataView[F1, R1]],
      views(1).asInstanceOf[DataView[F2, R2]],
      views(2).asInstanceOf[DataView[F3, R3]],
      views(3).asInstanceOf[DataView[F4, R4]],
      views(4).asInstanceOf[DataView[F5, R5]],
      views(5).asInstanceOf[DataView[F6, R6]],
      views(6).asInstanceOf[DataView[F7, R7]],
      views(7).asInstanceOf[DataView[F8, R8]]
    )
  }

  def interleave[
    F1 <: Format, R1 <: Raw,
    F2 <: Format, R2 <: Raw,
    F3 <: Format, R3 <: Raw,
    F4 <: Format, R4 <: Raw,
    F5 <: Format, R5 <: Raw,
    F6 <: Format, R6 <: Raw,
    F7 <: Format, R7 <: Raw,
    F8 <: Format, R8 <: Raw,
    F9 <: Format, R9 <: Raw
  ](
    seq1: inDataSeq[F1, R1],
    seq2: inDataSeq[F2, R2],
    seq3: inDataSeq[F3, R3],
    seq4: inDataSeq[F4, R4],
    seq5: inDataSeq[F5, R5],
    seq6: inDataSeq[F6, R6],
    seq7: inDataSeq[F7, R7],
    seq8: inDataSeq[F8, R8],
    seq9: inDataSeq[F9, R9]
  )(size: Int) = {
    val views = interleaveAll(
      seq1, seq2, seq3, seq4, seq5, seq6, seq7, seq8, seq9
    )(size)
    (
      views(0).asInstanceOf[DataView[F1, R1]],
      views(1).asInstanceOf[DataView[F2, R2]],
      views(2).asInstanceOf[DataView[F3, R3]],
      views(3).asInstanceOf[DataView[F4, R4]],
      views(4).asInstanceOf[DataView[F5, R5]],
      views(5).asInstanceOf[DataView[F6, R6]],
      views(6).asInstanceOf[DataView[F7, R7]],
      views(7).asInstanceOf[DataView[F8, R8]],
      views(8).asInstanceOf[DataView[F9, R9]]
    )
  }

  def interleave[
    F1 <: Format, R1 <: Raw,
    F2 <: Format, R2 <: Raw,
    F3 <: Format, R3 <: Raw,
    F4 <: Format, R4 <: Raw,
    F5 <: Format, R5 <: Raw,
    F6 <: Format, R6 <: Raw,
    F7 <: Format, R7 <: Raw,
    F8 <: Format, R8 <: Raw,
    F9 <: Format, R9 <: Raw,
    F10 <: Format, R10 <: Raw
  ](
    seq1: inDataSeq[F1, R1],
    seq2: inDataSeq[F2, R2],
    seq3: inDataSeq[F3, R3],
    seq4: inDataSeq[F4, R4],
    seq5: inDataSeq[F5, R5],
    seq6: inDataSeq[F6, R6],
    seq7: inDataSeq[F7, R7],
    seq8: inDataSeq[F8, R8],
    seq9: inDataSeq[F9, R9],
    seq10: inDataSeq[F10, R10]
  )(size: Int) = {
    val views = interleaveAll(
      seq1, seq2, seq3, seq4, seq5, seq6, seq7, seq8, seq9, seq10
    )(size)
    (
      views(0).asInstanceOf[DataView[F1, R1]],
      views(1).asInstanceOf[DataView[F2, R2]],
      views(2).asInstanceOf[DataView[F3, R3]],
      views(3).asInstanceOf[DataView[F4, R4]],
      views(4).asInstanceOf[DataView[F5, R5]],
      views(5).asInstanceOf[DataView[F6, R6]],
      views(6).asInstanceOf[DataView[F7, R7]],
      views(7).asInstanceOf[DataView[F8, R8]],
      views(8).asInstanceOf[DataView[F9, R9]],
      views(9).asInstanceOf[DataView[F10, R10]]
    )
  }

  def interleave[
    F1 <: Format, R1 <: Raw,
    F2 <: Format, R2 <: Raw,
    F3 <: Format, R3 <: Raw,
    F4 <: Format, R4 <: Raw,
    F5 <: Format, R5 <: Raw,
    F6 <: Format, R6 <: Raw,
    F7 <: Format, R7 <: Raw,
    F8 <: Format, R8 <: Raw,
    F9 <: Format, R9 <: Raw,
    F10 <: Format, R10 <: Raw,
    F11 <: Format, R11 <: Raw
  ](
    seq1: inDataSeq[F1, R1],
    seq2: inDataSeq[F2, R2],
    seq3: inDataSeq[F3, R3],
    seq4: inDataSeq[F4, R4],
    seq5: inDataSeq[F5, R5],
    seq6: inDataSeq[F6, R6],
    seq7: inDataSeq[F7, R7],
    seq8: inDataSeq[F8, R8],
    seq9: inDataSeq[F9, R9],
    seq10: inDataSeq[F10, R10],
    seq11: inDataSeq[F11, R11]
  )(size: Int) = {
    val views = interleaveAll(
      seq1, seq2, seq3, seq4, seq5, seq6, seq7, seq8, seq9, seq10, seq11
    )(size)
    (
      views(0).asInstanceOf[DataView[F1, R1]],
      views(1).asInstanceOf[DataView[F2, R2]],
      views(2).asInstanceOf[DataView[F3, R3]],
      views(3).asInstanceOf[DataView[F4, R4]],
      views(4).asInstanceOf[DataView[F5, R5]],
      views(5).asInstanceOf[DataView[F6, R6]],
      views(6).asInstanceOf[DataView[F7, R7]],
      views(7).asInstanceOf[DataView[F8, R8]],
      views(8).asInstanceOf[DataView[F9, R9]],
      views(9).asInstanceOf[DataView[F10, R10]],
      views(10).asInstanceOf[DataView[F11, R11]]
    )
  }

  def interleave[
    F1 <: Format, R1 <: Raw,
    F2 <: Format, R2 <: Raw,
    F3 <: Format, R3 <: Raw,
    F4 <: Format, R4 <: Raw,
    F5 <: Format, R5 <: Raw,
    F6 <: Format, R6 <: Raw,
    F7 <: Format, R7 <: Raw,
    F8 <: Format, R8 <: Raw,
    F9 <: Format, R9 <: Raw,
    F10 <: Format, R10 <: Raw,
    F11 <: Format, R11 <: Raw,
    F12 <: Format, R12 <: Raw
  ](
    seq1: inDataSeq[F1, R1],
    seq2: inDataSeq[F2, R2],
    seq3: inDataSeq[F3, R3],
    seq4: inDataSeq[F4, R4],
    seq5: inDataSeq[F5, R5],
    seq6: inDataSeq[F6, R6],
    seq7: inDataSeq[F7, R7],
    seq8: inDataSeq[F8, R8],
    seq9: inDataSeq[F9, R9],
    seq10: inDataSeq[F10, R10],
    seq11: inDataSeq[F11, R11],
    seq12: inDataSeq[F12, R12]
  )(size: Int) = {
    val views = interleaveAll(
      seq1, seq2, seq3, seq4, seq5, seq6, seq7, seq8, seq9, seq10, seq11, seq12
    )(size)
    (
      views(0).asInstanceOf[DataView[F1, R1]],
      views(1).asInstanceOf[DataView[F2, R2]],
      views(2).asInstanceOf[DataView[F3, R3]],
      views(3).asInstanceOf[DataView[F4, R4]],
      views(4).asInstanceOf[DataView[F5, R5]],
      views(5).asInstanceOf[DataView[F6, R6]],
      views(6).asInstanceOf[DataView[F7, R7]],
      views(7).asInstanceOf[DataView[F8, R8]],
      views(8).asInstanceOf[DataView[F9, R9]],
      views(9).asInstanceOf[DataView[F10, R10]],
      views(10).asInstanceOf[DataView[F11, R11]],
      views(11).asInstanceOf[DataView[F12, R12]]
    )
  }


  def interleaveAll(seqs: inData[_]*)(size: Int) :IndexedSeq[RawView] = {
    val dataSeqs = seqs.toArray

    // check arguments
    if (dataSeqs.length == 0) return new Array[RawView](0)

    // verify size
    var i = 0; while(i < dataSeqs.length) {
      val s = dataSeqs(i).size
      if (s != 0 && s != size) throw new IllegalArgumentException(
        "All data sources must have the same size."
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

      import scala.language.existentials
      type T = F forSome { type F <: Format }
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
