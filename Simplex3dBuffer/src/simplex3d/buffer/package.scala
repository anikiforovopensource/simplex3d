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

  private final def primitiveFactory[R <: DefinedInt](s: DataSeq[Int1, R]) :DataSeqFactory[Int1, R] = s
  private final def factory[E <: Composite, R <: DefinedInt](s: DataSeq[E, R]) :DataSeqFactory[E, R] = s
  private final def cast[R <: DefinedInt](f: DataSeqFactory[Int1, R]) = f.asInstanceOf[DataArray[Int1, R]]

  // Int1
  implicit final lazy val FactoryInt1SByte = primitiveFactory[SByte](new ArrayInt1SByte)
  implicit final lazy val FactoryInt1UByte = primitiveFactory[UByte](new ArrayInt1UByte)
  implicit final lazy val FactoryInt1SShort = primitiveFactory[SShort](new ArrayInt1SShort)
  implicit final lazy val FactoryInt1UShort = primitiveFactory[UShort](new ArrayInt1UShort)
  implicit final lazy val FactoryInt1SInt = primitiveFactory[SInt](new ArrayInt1SInt)
  implicit final lazy val FactoryInt1UInt = primitiveFactory[UInt](new ArrayInt1UInt)

  // Vec2i
  implicit final lazy val FactoryVec2iSByte = factory[Vec2i, SByte](new ArrayVec2i(cast(FactoryInt1SByte)))
  implicit final lazy val FactoryVec2iUByte = factory[Vec2i, UByte](new ArrayVec2i(cast(FactoryInt1UByte)))
  implicit final lazy val FactoryVec2iSShort = factory[Vec2i, SShort](new ArrayVec2i(cast(FactoryInt1SShort)))
  implicit final lazy val FactoryVec2iUShort = factory[Vec2i, UShort](new ArrayVec2i(cast(FactoryInt1UShort)))
  implicit final lazy val FactoryVec2iSInt = factory[Vec2i, SInt](new ArrayVec2i(cast(FactoryInt1SInt)))
  implicit final lazy val FactoryVec2iUInt = factory[Vec2i, UInt](new ArrayVec2i(cast(FactoryInt1UInt)))

  // Vec3i
  implicit final lazy val FactoryVec3iSByte = factory[Vec3i, SByte](new ArrayVec3i(cast(FactoryInt1SByte)))
  implicit final lazy val FactoryVec3iUByte = factory[Vec3i, UByte](new ArrayVec3i(cast(FactoryInt1UByte)))
  implicit final lazy val FactoryVec3iSShort = factory[Vec3i, SShort](new ArrayVec3i(cast(FactoryInt1SShort)))
  implicit final lazy val FactoryVec3iUShort = factory[Vec3i, UShort](new ArrayVec3i(cast(FactoryInt1UShort)))
  implicit final lazy val FactoryVec3iSInt = factory[Vec3i, SInt](new ArrayVec3i(cast(FactoryInt1SInt)))
  implicit final lazy val FactoryVec3iUInt = factory[Vec3i, UInt](new ArrayVec3i(cast(FactoryInt1UInt)))

  // Vec4i
  implicit final lazy val FactoryVec4iSByte = factory[Vec4i, SByte](new ArrayVec4i(cast(FactoryInt1SByte)))
  implicit final lazy val FactoryVec4iUByte = factory[Vec4i, UByte](new ArrayVec4i(cast(FactoryInt1UByte)))
  implicit final lazy val FactoryVec4iSShort = factory[Vec4i, SShort](new ArrayVec4i(cast(FactoryInt1SShort)))
  implicit final lazy val FactoryVec4iUShort = factory[Vec4i, UShort](new ArrayVec4i(cast(FactoryInt1UShort)))
  implicit final lazy val FactoryVec4iSInt = factory[Vec4i, SInt](new ArrayVec4i(cast(FactoryInt1SInt)))
  implicit final lazy val FactoryVec4iUInt = factory[Vec4i, UInt](new ArrayVec4i(cast(FactoryInt1UInt)))

  
  type MetaElement = integration.buffer.MetaElement
  type Primitive = integration.buffer.Primitive
  type Composite = integration.buffer.Composite
  type Int1 = integration.buffer.Int1
  type Float1 = integration.buffer.Float1
  type Double1 = integration.buffer.Double1
  val MetaManifest = integration.buffer.MetaManifest

  type ReadData[E <: MetaElement] = ReadDataSeq[E, RawData]
  type Data[E <: MetaElement] = DataSeq[E, RawData]
  type inData[E <: MetaElement] = inDataSeq[E, RawData]
  type outData[E <: MetaElement] = outDataSeq[E, RawData]

  type ReadIndex = ReadIndexSeq[Unsigned]
  type Index = IndexSeq[Unsigned]
  type inIndex = inIndexSeq[Unsigned]
  type outIndex = outIndexSeq[Unsigned]
  
  type RawView = ReadDataView[_, RawData]

  type inDataSeq[E <: MetaElement, +R <: RawData] = ReadDataSeq[E, R]
  type inContiguousSeq[E <: MetaElement, +R <: RawData] =ReadContiguousSeq[E, R]
  type inDataArray[E <: MetaElement, +R <: RawData] = ReadDataArray[E, R]
  type inDataBuffer[E <: MetaElement, +R <: RawData] = ReadDataBuffer[E, R]
  type inDataView[E <: MetaElement, +R <: RawData] = ReadDataView[E, R]

  type inIndexSeq[+R <: Unsigned] = ReadIndexSeq[R]
  type inIndexArray[+R <: Unsigned] = ReadIndexArray[R]
  type inIndexBuffer[+R <: Unsigned] = ReadIndexBuffer[R]

  type outDataSeq[E <: MetaElement, +R <: RawData] = DataSeq[E, R]
  type outContiguousSeq[E <: MetaElement, +R <: RawData] =ContiguousSeq[E, R]
  type outDataArray[E <: MetaElement, +R <: RawData] = DataArray[E, R]
  type outDataBuffer[E <: MetaElement, +R <: RawData] = DataBuffer[E, R]
  type outDataView[E <: MetaElement, +R <: RawData] = DataView[E, R]

  type outIndexSeq[+R <: Unsigned] = IndexSeq[R]
  type outIndexArray[+R <: Unsigned] = IndexArray[R]
  type outIndexBuffer[+R <: Unsigned] = IndexBuffer[R]

  @inline implicit final def readContegiousDataToIndex[R  <: Unsigned] (
    d: ReadContiguousSeq[Int1, R]
  ) = d.asInstanceOf[ReadIndexSeq[R]]

  @inline implicit final def readArrayDataToIndex[R  <: Unsigned] (
    d: ReadDataArray[Int1, R]
  ) = d.asInstanceOf[ReadIndexArray[R]]

  @inline implicit final def readBufferDataToIndex[R  <: Unsigned](
    d: ReadDataBuffer[Int1, R]
  ) = d.asInstanceOf[ReadIndexBuffer[R]]

  @inline implicit final def contegiousDataToIndex[R  <: Unsigned] (
    d: ContiguousSeq[Int1, R]
  ) = d.asInstanceOf[IndexSeq[R]]
  
  @inline implicit final def arrayDataToIndex[R  <: Unsigned](
    d: DataArray[Int1, R]
  ) = d.asInstanceOf[IndexArray[R]]

  @inline implicit final def bufferDataToIndex[R  <: Unsigned](
    d: DataBuffer[Int1, R]
  ) = d.asInstanceOf[IndexBuffer[R]]


  def interleave[
    E1 <: MetaElement, R1 <: RawData,
    E2 <: MetaElement, R2 <: RawData
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
    E1 <: MetaElement, R1 <: RawData,
    E2 <: MetaElement, R2 <: RawData,
    E3 <: MetaElement, R3 <: RawData
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
    var pad = totalWidth % maxComponentWidth
    if (pad > 0) pad = maxComponentWidth - pad
    val byteStride = totalWidth + pad

    // generate
    val byteBuffer = ByteBuffer.allocateDirect(byteStride*size)
    val result = new Array[RawView](dataSeqs.length)
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
