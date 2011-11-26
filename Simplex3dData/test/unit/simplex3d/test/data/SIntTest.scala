/*
 * Simplex3dData - Test Package
 * Copyright (C) 2010-2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dDataTest.
 *
 * Simplex3dDataTest is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dDataTest is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.test.data

import org.scalatest._
import simplex3d.math._
import simplex3d.data._

import Descriptors._
import TestUtil._
import FactoryTestUtil._
import ApplyUpdateTestUtil._
import SubCopyTestUtil._


/**
 * @author Aleksey Nikiforov (lex)
 */
class SIntTest extends FunSuite {

  test("Factories") {
    testArrayFromSize(DataArray[SInt, SByte](_))
    testArrayFromData[SInt, SByte](DataArray[SInt, SByte](_))
    testBufferFromSize(DataBuffer[SInt, SByte](_))
    testBufferFromData(DataBuffer[SInt, SByte](_))
    testViewFromData(DataView[SInt, SByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[SInt, SByte](_))
    testReadViewFromData(ReadDataView[SInt, SByte](_, _, _))
    testArrayFromCollection[SInt, SByte]((a: IndexedSeq[Int]) => DataArray[SInt, SByte](a: _*))
    testBufferFromCollection[SInt, SByte]((a: IndexedSeq[Int]) => DataBuffer[SInt, SByte](a: _*))
    
    testArrayFromSize(DataArray[SInt, UByte](_))
    testArrayFromData[SInt, UByte](DataArray[SInt, UByte](_))
    testBufferFromSize(DataBuffer[SInt, UByte](_))
    testBufferFromData(DataBuffer[SInt, UByte](_))
    testViewFromData(DataView[SInt, UByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[SInt, UByte](_))
    testReadViewFromData(ReadDataView[SInt, UByte](_, _, _))
    testArrayFromCollection[SInt, UByte]((a: IndexedSeq[Int]) => DataArray[SInt, UByte](a: _*))
    testBufferFromCollection[SInt, UByte]((a: IndexedSeq[Int]) => DataBuffer[SInt, UByte](a: _*))
    
    testArrayFromSize(DataArray[SInt, SShort](_))
    testArrayFromData[SInt, SShort](DataArray[SInt, SShort](_))
    testBufferFromSize(DataBuffer[SInt, SShort](_))
    testBufferFromData(DataBuffer[SInt, SShort](_))
    testViewFromData(DataView[SInt, SShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[SInt, SShort](_))
    testReadViewFromData(ReadDataView[SInt, SShort](_, _, _))
    testArrayFromCollection[SInt, SShort]((a: IndexedSeq[Int]) => DataArray[SInt, SShort](a: _*))
    testBufferFromCollection[SInt, SShort]((a: IndexedSeq[Int]) => DataBuffer[SInt, SShort](a: _*))
    
    testArrayFromSize(DataArray[SInt, UShort](_))
    testArrayFromData[SInt, UShort](DataArray[SInt, UShort](_))
    testBufferFromSize(DataBuffer[SInt, UShort](_))
    testBufferFromData(DataBuffer[SInt, UShort](_))
    testViewFromData(DataView[SInt, UShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[SInt, UShort](_))
    testReadViewFromData(ReadDataView[SInt, UShort](_, _, _))
    testArrayFromCollection[SInt, UShort]((a: IndexedSeq[Int]) => DataArray[SInt, UShort](a: _*))
    testBufferFromCollection[SInt, UShort]((a: IndexedSeq[Int]) => DataBuffer[SInt, UShort](a: _*))
    
    testArrayFromSize(DataArray[SInt, SInt](_))
    testArrayFromData[SInt, SInt](DataArray[SInt, SInt](_))
    testBufferFromSize(DataBuffer[SInt, SInt](_))
    testBufferFromData(DataBuffer[SInt, SInt](_))
    testViewFromData(DataView[SInt, SInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[SInt, SInt](_))
    testReadViewFromData(ReadDataView[SInt, SInt](_, _, _))
    testArrayFromCollection[SInt, SInt]((a: IndexedSeq[Int]) => DataArray[SInt, SInt](a: _*))
    testBufferFromCollection[SInt, SInt]((a: IndexedSeq[Int]) => DataBuffer[SInt, SInt](a: _*))
    
    testArrayFromSize(DataArray[SInt, UInt](_))
    testArrayFromData[SInt, UInt](DataArray[SInt, UInt](_))
    testBufferFromSize(DataBuffer[SInt, UInt](_))
    testBufferFromData(DataBuffer[SInt, UInt](_))
    testViewFromData(DataView[SInt, UInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[SInt, UInt](_))
    testReadViewFromData(ReadDataView[SInt, UInt](_, _, _))
    testArrayFromCollection[SInt, UInt]((a: IndexedSeq[Int]) => DataArray[SInt, UInt](a: _*))
    testBufferFromCollection[SInt, UInt]((a: IndexedSeq[Int]) => DataBuffer[SInt, UInt](a: _*))
  }

  
  private val size = 10

  test("Apply/Update") {
    testSByte(DataArray[SInt, SByte](size))
    testSByte(DataBuffer[SInt, SByte](size))
    testSByte(DataView[SInt, SByte](genBuffer(size, Descriptors.SIntSByte)._1, 0, 2))
    testSByte(DataView[SInt, SByte](genBuffer(size, Descriptors.SIntSByte)._1, 1, 2))

    testUByte(DataArray[SInt, UByte](size))
    testUByte(DataBuffer[SInt, UByte](size))
    testUByte(DataView[SInt, UByte](genBuffer(size, Descriptors.SIntUByte)._1, 0, 2))
    testUByte(DataView[SInt, UByte](genBuffer(size, Descriptors.SIntUByte)._1, 1, 2))
    
    testSShort(DataArray[SInt, SShort](size))
    testSShort(DataBuffer[SInt, SShort](size))
    testSShort(DataView[SInt, SShort](genBuffer(size, Descriptors.SIntSShort)._1, 0, 2))
    testSShort(DataView[SInt, SShort](genBuffer(size, Descriptors.SIntSShort)._1, 1, 2))

    testUShort(DataArray[SInt, UShort](size))
    testUShort(DataBuffer[SInt, UShort](size))
    testUShort(DataView[SInt, UShort](genBuffer(size, Descriptors.SIntUShort)._1, 0, 2))
    testUShort(DataView[SInt, UShort](genBuffer(size, Descriptors.SIntUShort)._1, 1, 2))

    testSInt(DataArray[SInt, SInt](size))
    testSInt(DataBuffer[SInt, SInt](size))
    testSInt(DataView[SInt, SInt](genBuffer(size, Descriptors.SIntSInt)._1, 0, 2))
    testSInt(DataView[SInt, SInt](genBuffer(size, Descriptors.SIntSInt)._1, 1, 2))

    testUInt(DataArray[SInt, UInt](size))
    testUInt(DataBuffer[SInt, UInt](size))
    testUInt(DataView[SInt, UInt](genBuffer(size, Descriptors.SIntUInt)._1, 0, 2))
    testUInt(DataView[SInt, UInt](genBuffer(size, Descriptors.SIntUInt)._1, 1, 2))
  }
  
  test("Sub Copy") {
    testSubCopy(DataSeq[SInt, SByte])
    testSubCopy(DataSeq[SInt, UByte])
    testSubCopy(DataSeq[SInt, SShort])
    testSubCopy(DataSeq[SInt, UShort])
    testSubCopy(DataSeq[SInt, SInt])
    testSubCopy(DataSeq[SInt, UInt])
  }

  private def testSByte(seq: DataSeq[SInt, SByte]) {
    testIndex(seq)

    testApplyUpdate(seq, -129, 127, 127)
    testApplyUpdate(seq, -128, -128, -128)
    testApplyUpdate(seq, -1, -1, -1)
    testApplyUpdate(seq, 0, 0, 0)
    testApplyUpdate(seq, 1, 1, 1)
    testApplyUpdate(seq, 127, 127, 127)
    testApplyUpdate(seq, 128, -128, -128)
  }

  private def testUByte(seq: DataSeq[SInt, UByte]) {
    testIndex(seq)

    testApplyUpdate(seq, -1, 255, -1)
    testApplyUpdate(seq, 0, 0, 0)
    testApplyUpdate(seq, 1, 1, 1)
    testApplyUpdate(seq, 127, 127, 127)
    testApplyUpdate(seq, 128, 128, -128)
    testApplyUpdate(seq, 255, 255, -1)
    testApplyUpdate(seq, 256, 0, 0)
    testApplyUpdate(seq, 257, 1, 1)
  }
  
  private def testSShort(seq: DataSeq[SInt, SShort]) {
    testIndex(seq)

    testApplyUpdate(seq, -32769, 32767, 32767)
    testApplyUpdate(seq, -32768, -32768, -32768)
    testApplyUpdate(seq, -1, -1, -1)
    testApplyUpdate(seq, 0, 0, 0)
    testApplyUpdate(seq, 1, 1, 1)
    testApplyUpdate(seq, 32767, 32767, 32767)
    testApplyUpdate(seq, 32768, -32768, -32768)
  }

  private def testUShort(seq: DataSeq[SInt, UShort]) {
    testIndex(seq)

    testApplyUpdate(seq, -1, 65535, 65535)
    testApplyUpdate(seq, 0, 0, 0)
    testApplyUpdate(seq, 1, 1, 1)
    testApplyUpdate(seq, 32767, 32767, 32767)
    testApplyUpdate(seq, 32768, 32768, 32768)
    testApplyUpdate(seq, 65535, 65535, 65535)
    testApplyUpdate(seq, 65536, 0, 0)
    testApplyUpdate(seq, 65537, 1, 1)
  }
  
  private def testSInt(seq: DataSeq[SInt, SInt]) {
    testIndex(seq)

    testApplyUpdate(seq, scala.Int.MinValue, scala.Int.MinValue, scala.Int.MinValue)
    testApplyUpdate(seq, -1, -1, -1)
    testApplyUpdate(seq, 0, 0, 0)
    testApplyUpdate(seq, 1, 1, 1)
    testApplyUpdate(seq, scala.Int.MaxValue, scala.Int.MaxValue, scala.Int.MaxValue)
  }

  private def testUInt(seq: DataSeq[SInt, UInt]) {
    testIndex(seq)

    testApplyUpdate(seq, scala.Int.MinValue, scala.Int.MinValue, scala.Int.MinValue)
    testApplyUpdate(seq, -1, -1, -1)
    testApplyUpdate(seq, 0, 0, 0)
    testApplyUpdate(seq, 1, 1, 1)
    testApplyUpdate(seq, scala.Int.MaxValue, scala.Int.MaxValue, scala.Int.MaxValue)
  }
}
