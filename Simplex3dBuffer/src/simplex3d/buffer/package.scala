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

package simplex3d

import java.nio._
import simplex3d.math._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object buffer {

  private final def primitiveFactory[R <: DefinedInt](s: DataSeq[SInt, R]) :DataFactory[SInt, R] = s
  private final def factory[E <: Composite, R <: DefinedInt](s: DataSeq[E, R]) :DataFactory[E, R] = s
  private final def cast[R <: DefinedInt](f: DataFactory[SInt, R]) = f.asInstanceOf[DataArray[SInt, R]]

  // SInt
  implicit final lazy val FactorySIntSByte = primitiveFactory[SByte](new ArraySIntSByte)
  implicit final lazy val FactorySIntUByte = primitiveFactory[UByte](new ArraySIntUByte)
  implicit final lazy val FactorySIntSShort = primitiveFactory[SShort](new ArraySIntSShort)
  implicit final lazy val FactorySIntUShort = primitiveFactory[UShort](new ArraySIntUShort)
  implicit final lazy val FactorySIntSInt = primitiveFactory[SInt](new ArraySIntSInt)
  implicit final lazy val FactorySIntUInt = primitiveFactory[UInt](new ArraySIntUInt)

  // Vec2i
  implicit final lazy val FactoryVec2iSByte = factory[Vec2i, SByte](new ArrayVec2i(cast(FactorySIntSByte)))
  implicit final lazy val FactoryVec2iUByte = factory[Vec2i, UByte](new ArrayVec2i(cast(FactorySIntUByte)))
  implicit final lazy val FactoryVec2iSShort = factory[Vec2i, SShort](new ArrayVec2i(cast(FactorySIntSShort)))
  implicit final lazy val FactoryVec2iUShort = factory[Vec2i, UShort](new ArrayVec2i(cast(FactorySIntUShort)))
  implicit final lazy val FactoryVec2iSInt = factory[Vec2i, SInt](new ArrayVec2i(cast(FactorySIntSInt)))
  implicit final lazy val FactoryVec2iUInt = factory[Vec2i, UInt](new ArrayVec2i(cast(FactorySIntUInt)))

  // Vec3i
  implicit final lazy val FactoryVec3iSByte = factory[Vec3i, SByte](new ArrayVec3i(cast(FactorySIntSByte)))
  implicit final lazy val FactoryVec3iUByte = factory[Vec3i, UByte](new ArrayVec3i(cast(FactorySIntUByte)))
  implicit final lazy val FactoryVec3iSShort = factory[Vec3i, SShort](new ArrayVec3i(cast(FactorySIntSShort)))
  implicit final lazy val FactoryVec3iUShort = factory[Vec3i, UShort](new ArrayVec3i(cast(FactorySIntUShort)))
  implicit final lazy val FactoryVec3iSInt = factory[Vec3i, SInt](new ArrayVec3i(cast(FactorySIntSInt)))
  implicit final lazy val FactoryVec3iUInt = factory[Vec3i, UInt](new ArrayVec3i(cast(FactorySIntUInt)))

  // Vec4i
  implicit final lazy val FactoryVec4iSByte = factory[Vec4i, SByte](new ArrayVec4i(cast(FactorySIntSByte)))
  implicit final lazy val FactoryVec4iUByte = factory[Vec4i, UByte](new ArrayVec4i(cast(FactorySIntUByte)))
  implicit final lazy val FactoryVec4iSShort = factory[Vec4i, SShort](new ArrayVec4i(cast(FactorySIntSShort)))
  implicit final lazy val FactoryVec4iUShort = factory[Vec4i, UShort](new ArrayVec4i(cast(FactorySIntUShort)))
  implicit final lazy val FactoryVec4iSInt = factory[Vec4i, SInt](new ArrayVec4i(cast(FactorySIntSInt)))
  implicit final lazy val FactoryVec4iUInt = factory[Vec4i, UInt](new ArrayVec4i(cast(FactorySIntUInt)))

  
  type Meta = integration.buffer.Meta
  type Primitive = integration.buffer.Primitive
  type Composite = integration.buffer.Composite
  val MetaManifest = integration.buffer.MetaManifest
  type Raw = integration.buffer.Raw
  type Defined = integration.buffer.Defined
  type DefinedInt = integration.buffer.DefinedInt
  type DefinedIndex = integration.buffer.DefinedIndex
  type DefinedFloat = integration.buffer.DefinedFloat
  type DefinedDouble = integration.buffer.DefinedDouble
  type Integral = integration.buffer.Integral
  type Signed = integration.buffer.Signed
  type Unsigned = integration.buffer.Unsigned
  type RawByte = integration.buffer.RawByte
  type SByte = integration.buffer.SByte
  type UByte = integration.buffer.UByte
  type SShort = integration.buffer.SShort
  type UShort = integration.buffer.UShort
  type SInt = integration.buffer.SInt
  type UInt = integration.buffer.UInt
  type FloatingPoint = integration.buffer.FloatingPoint
  type HFloat = integration.buffer.HFloat
  type RFloat = integration.buffer.RFloat
  type RDouble = integration.buffer.RDouble


  type ReadData[E <: Meta] = ReadDataSeq[E, Raw]
  type Data[E <: Meta] = DataSeq[E, Raw]
  type inData[E <: Meta] = inDataSeq[E, Raw]
  type outData[E <: Meta] = outDataSeq[E, Raw]

  type ReadIndex = ReadIndexSeq[Unsigned]
  type Index = IndexSeq[Unsigned]
  type inIndex = inIndexSeq[Unsigned]
  type outIndex = outIndexSeq[Unsigned]
  
  type RawView = ReadDataView[_ <: Meta, Raw]

  type inDataSeq[E <: Meta, +R <: Raw] = ReadDataSeq[E, R]
  type inContiguous[E <: Meta, +R <: Raw] = ReadContiguous[E, R]
  type inDataArray[E <: Meta, +R <: Raw] = ReadDataArray[E, R]
  type inDataBuffer[E <: Meta, +R <: Raw] = ReadDataBuffer[E, R]
  type inDataView[E <: Meta, +R <: Raw] = ReadDataView[E, R]

  type inIndexSeq[+R <: Unsigned] = ReadIndexSeq[R]
  type inIndexArray[+R <: Unsigned] = ReadIndexArray[R]
  type inIndexBuffer[+R <: Unsigned] = ReadIndexBuffer[R]

  type outDataSeq[E <: Meta, +R <: Raw] = DataSeq[E, R]
  type outContiguous[E <: Meta, +R <: Raw] = Contiguous[E, R]
  type outDataArray[E <: Meta, +R <: Raw] = DataArray[E, R]
  type outDataBuffer[E <: Meta, +R <: Raw] = DataBuffer[E, R]
  type outDataView[E <: Meta, +R <: Raw] = DataView[E, R]

  type outIndexSeq[+R <: Unsigned] = IndexSeq[R]
  type outIndexArray[+R <: Unsigned] = IndexArray[R]
  type outIndexBuffer[+R <: Unsigned] = IndexBuffer[R]

  @inline implicit final def readContegiousDataToIndex[R  <: Unsigned] (
    d: ReadContiguous[SInt, R]
  ) = d.asInstanceOf[ReadIndexSeq[R]]

  @inline implicit final def readArrayDataToIndex[R  <: Unsigned] (
    d: ReadDataArray[SInt, R]
  ) = d.asInstanceOf[ReadIndexArray[R]]

  @inline implicit final def readBufferDataToIndex[R  <: Unsigned](
    d: ReadDataBuffer[SInt, R]
  ) = d.asInstanceOf[ReadIndexBuffer[R]]

  @inline implicit final def contegiousDataToIndex[R  <: Unsigned] (
    d: Contiguous[SInt, R]
  ) = d.asInstanceOf[IndexSeq[R]]
  
  @inline implicit final def arrayDataToIndex[R  <: Unsigned](
    d: DataArray[SInt, R]
  ) = d.asInstanceOf[IndexArray[R]]

  @inline implicit final def bufferDataToIndex[R  <: Unsigned](
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


  def interleaveAll(seqs: inData[_]*)(size: Int) :IndexedSeq[RawView] = {
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

      type T = E forSome { type E <: Meta }
      val seq = dataSeqs(order(i)).asInstanceOf[Data[T]]
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
