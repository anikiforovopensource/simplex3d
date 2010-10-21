/*
 * Simplex3d, BufferTest package
 * Copyright (C) 2010, Simplex3d Team
 *
 * This file is part of Simplex3dBufferTest.
 *
 * Simplex3dBufferTest is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dBufferTest is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package test.buffer
package floatm

import org.scalatest._
import simplex3d.math.floatm._
import simplex3d.buffer._
import simplex3d.buffer.floatm._

import Descriptors._
import FactoryTest._
import TestUtil._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Float1Test extends FunSuite {

  test("Factories") {
    testArrayFromSize(DataArray[Float1, SByte](_))
    testArrayFromData[Float1, SByte](DataArray[Float1, SByte](_))
    testBufferFromSize(DataBuffer[Float1, SByte](_))
    testBufferFromData(DataBuffer[Float1, SByte](_))
    testViewFromData(DataView[Float1, SByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Float1, SByte](_))
    testReadViewFromData(ReadDataView[Float1, SByte](_, _, _))
    testArrayFromCollection[Float1, SByte]((a: IndexedSeq[Float]) => DataArray[Float1, SByte](a: _*))
    testArrayFromCollection[Float1, SByte]((a: IndexedSeq[Float]) => DataArray[Float1, SByte](a))
    testBufferFromCollection[Float1, SByte]((a: IndexedSeq[Float]) => DataBuffer[Float1, SByte](a: _*))
    testBufferFromCollection[Float1, SByte]((a: IndexedSeq[Float]) => DataBuffer[Float1, SByte](a))

    testArrayFromSize(DataArray[Float1, UByte](_))
    testArrayFromData[Float1, UByte](DataArray[Float1, UByte](_))
    testBufferFromSize(DataBuffer[Float1, UByte](_))
    testBufferFromData(DataBuffer[Float1, UByte](_))
    testViewFromData(DataView[Float1, UByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Float1, UByte](_))
    testReadViewFromData(ReadDataView[Float1, UByte](_, _, _))
    testArrayFromCollection[Float1, UByte]((a: IndexedSeq[Float]) => DataArray[Float1, UByte](a: _*))
    testArrayFromCollection[Float1, UByte]((a: IndexedSeq[Float]) => DataArray[Float1, UByte](a))
    testBufferFromCollection[Float1, UByte]((a: IndexedSeq[Float]) => DataBuffer[Float1, UByte](a: _*))
    testBufferFromCollection[Float1, UByte]((a: IndexedSeq[Float]) => DataBuffer[Float1, UByte](a))

    testArrayFromSize(DataArray[Float1, SShort](_))
    testArrayFromData[Float1, SShort](DataArray[Float1, SShort](_))
    testBufferFromSize(DataBuffer[Float1, SShort](_))
    testBufferFromData(DataBuffer[Float1, SShort](_))
    testViewFromData(DataView[Float1, SShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Float1, SShort](_))
    testReadViewFromData(ReadDataView[Float1, SShort](_, _, _))
    testArrayFromCollection[Float1, SShort]((a: IndexedSeq[Float]) => DataArray[Float1, SShort](a: _*))
    testArrayFromCollection[Float1, SShort]((a: IndexedSeq[Float]) => DataArray[Float1, SShort](a))
    testBufferFromCollection[Float1, SShort]((a: IndexedSeq[Float]) => DataBuffer[Float1, SShort](a: _*))
    testBufferFromCollection[Float1, SShort]((a: IndexedSeq[Float]) => DataBuffer[Float1, SShort](a))

    testArrayFromSize(DataArray[Float1, UShort](_))
    testArrayFromData[Float1, UShort](DataArray[Float1, UShort](_))
    testBufferFromSize(DataBuffer[Float1, UShort](_))
    testBufferFromData(DataBuffer[Float1, UShort](_))
    testViewFromData(DataView[Float1, UShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Float1, UShort](_))
    testReadViewFromData(ReadDataView[Float1, UShort](_, _, _))
    testArrayFromCollection[Float1, UShort]((a: IndexedSeq[Float]) => DataArray[Float1, UShort](a: _*))
    testArrayFromCollection[Float1, UShort]((a: IndexedSeq[Float]) => DataArray[Float1, UShort](a))
    testBufferFromCollection[Float1, UShort]((a: IndexedSeq[Float]) => DataBuffer[Float1, UShort](a: _*))
    testBufferFromCollection[Float1, UShort]((a: IndexedSeq[Float]) => DataBuffer[Float1, UShort](a))

    testArrayFromSize(DataArray[Float1, SInt](_))
    testArrayFromData[Float1, SInt](DataArray[Float1, SInt](_))
    testBufferFromSize(DataBuffer[Float1, SInt](_))
    testBufferFromData(DataBuffer[Float1, SInt](_))
    testViewFromData(DataView[Float1, SInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Float1, SInt](_))
    testReadViewFromData(ReadDataView[Float1, SInt](_, _, _))
    testArrayFromCollection[Float1, SInt]((a: IndexedSeq[Float]) => DataArray[Float1, SInt](a: _*))
    testArrayFromCollection[Float1, SInt]((a: IndexedSeq[Float]) => DataArray[Float1, SInt](a))
    testBufferFromCollection[Float1, SInt]((a: IndexedSeq[Float]) => DataBuffer[Float1, SInt](a: _*))
    testBufferFromCollection[Float1, SInt]((a: IndexedSeq[Float]) => DataBuffer[Float1, SInt](a))

    testArrayFromSize(DataArray[Float1, UInt](_))
    testArrayFromData[Float1, UInt](DataArray[Float1, UInt](_))
    testBufferFromSize(DataBuffer[Float1, UInt](_))
    testBufferFromData(DataBuffer[Float1, UInt](_))
    testViewFromData(DataView[Float1, UInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Float1, UInt](_))
    testReadViewFromData(ReadDataView[Float1, UInt](_, _, _))
    testArrayFromCollection[Float1, UInt]((a: IndexedSeq[Float]) => DataArray[Float1, UInt](a: _*))
    testArrayFromCollection[Float1, UInt]((a: IndexedSeq[Float]) => DataArray[Float1, UInt](a))
    testBufferFromCollection[Float1, UInt]((a: IndexedSeq[Float]) => DataBuffer[Float1, UInt](a: _*))
    testBufferFromCollection[Float1, UInt]((a: IndexedSeq[Float]) => DataBuffer[Float1, UInt](a))
    
    testArrayFromSize(DataArray[Float1, HalfFloat](_))
    testArrayFromData[Float1, HalfFloat](DataArray[Float1, HalfFloat](_))
    testBufferFromSize(DataBuffer[Float1, HalfFloat](_))
    testBufferFromData(DataBuffer[Float1, HalfFloat](_))
    testViewFromData(DataView[Float1, HalfFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Float1, HalfFloat](_))
    testReadViewFromData(ReadDataView[Float1, HalfFloat](_, _, _))
    testArrayFromCollection[Float1, HalfFloat]((a: IndexedSeq[Float]) => DataArray[Float1, HalfFloat](a: _*))
    testArrayFromCollection[Float1, HalfFloat]((a: IndexedSeq[Float]) => DataArray[Float1, HalfFloat](a))
    testBufferFromCollection[Float1, HalfFloat]((a: IndexedSeq[Float]) => DataBuffer[Float1, HalfFloat](a: _*))
    testBufferFromCollection[Float1, HalfFloat]((a: IndexedSeq[Float]) => DataBuffer[Float1, HalfFloat](a))
    
    testArrayFromSize(DataArray[Float1, RawFloat](_))
    testArrayFromData[Float1, RawFloat](DataArray[Float1, RawFloat](_))
    testBufferFromSize(DataBuffer[Float1, RawFloat](_))
    testBufferFromData(DataBuffer[Float1, RawFloat](_))
    testViewFromData(DataView[Float1, RawFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Float1, RawFloat](_))
    testReadViewFromData(ReadDataView[Float1, RawFloat](_, _, _))
    testArrayFromCollection[Float1, RawFloat]((a: IndexedSeq[Float]) => DataArray[Float1, RawFloat](a: _*))
    testArrayFromCollection[Float1, RawFloat]((a: IndexedSeq[Float]) => DataArray[Float1, RawFloat](a))
    testBufferFromCollection[Float1, RawFloat]((a: IndexedSeq[Float]) => DataBuffer[Float1, RawFloat](a: _*))
    testBufferFromCollection[Float1, RawFloat]((a: IndexedSeq[Float]) => DataBuffer[Float1, RawFloat](a))
  }
  
  private val size = 10
  
  test("Apply/Update") {
    testSByte(DataArray[Float1, SByte](size))
    testSByte(DataBuffer[Float1, SByte](size))
    testSByte(DataView[Float1, SByte](genBuffer(size, Descriptors.Float1SByte)._1, 0, 2))
    testSByte(DataView[Float1, SByte](genBuffer(size, Descriptors.Float1SByte)._1, 1, 2))
    
    testUByte(DataArray[Float1, UByte](size))
    testUByte(DataBuffer[Float1, UByte](size))
    testUByte(DataView[Float1, UByte](genBuffer(size, Descriptors.Float1UByte)._1, 0, 2))
    testUByte(DataView[Float1, UByte](genBuffer(size, Descriptors.Float1UByte)._1, 1, 2))
    
    testSShort(DataArray[Float1, SShort](size))
    testSShort(DataBuffer[Float1, SShort](size))
    testSShort(DataView[Float1, SShort](genBuffer(size, Descriptors.Float1SShort)._1, 0, 2))
    testSShort(DataView[Float1, SShort](genBuffer(size, Descriptors.Float1SShort)._1, 1, 2))
    
    testUShort(DataArray[Float1, UShort](size))
    testUShort(DataBuffer[Float1, UShort](size))
    testUShort(DataView[Float1, UShort](genBuffer(size, Descriptors.Float1UShort)._1, 0, 2))
    testUShort(DataView[Float1, UShort](genBuffer(size, Descriptors.Float1UShort)._1, 1, 2))
    
    testSInt(DataArray[Float1, SInt](size))
    testSInt(DataBuffer[Float1, SInt](size))
    testSInt(DataView[Float1, SInt](genBuffer(size, Descriptors.Float1SInt)._1, 0, 2))
    testSInt(DataView[Float1, SInt](genBuffer(size, Descriptors.Float1SInt)._1, 1, 2))
    
//    testUInt(DataArray[Float1, UInt](size))
//    testUInt(DataBuffer[Float1, UInt](size))
//    testUInt(DataView[Float1, UInt](genBuffer(size, Descriptors.Float1UInt)._1, 0, 2))
//    testUInt(DataView[Float1, UInt](genBuffer(size, Descriptors.Float1UInt)._1, 1, 2))
  }

  private def testSByte(seq: DataSeq[Float1, SByte]) {
    testIndex(seq)

    seq.asBuffer().put(seq.offset, -128); assert(seq(0) == -1)

    testApplyUpdate(seq, Float.NegativeInfinity, -1, -127)
    testApplyUpdate(seq, -Float.MaxValue, -1, -127)
    testApplyUpdate(seq, -2, -1, -127)
    testApplyUpdate(seq, -128/127f, -1, -127)
    testApplyUpdate(seq, -1, -1, -127)
    testApplyUpdate(seq, -0.5f, -64/127f, -64)
    testApplyUpdate(seq, -1/84f, -2/127f, -2)
    testApplyUpdate(seq, -1/85f, -1/127f, -1)
    testApplyUpdate(seq, -1/127f, -1/127f, -1)
    testApplyUpdate(seq, -1/254f, -1/127f, -1)
    testApplyUpdate(seq, -1/255f, 0, 0)
    testApplyUpdate(seq, Float.NaN, 0, 0)
    testApplyUpdate(seq, 0, 0, 0)
    testApplyUpdate(seq, 1/255f, 0, 0)
    testApplyUpdate(seq, 1/254f, 1/127f, 1)
    testApplyUpdate(seq, 1/127f, 1/127f, 1)
    testApplyUpdate(seq, 1/85f, 1/127f, 1)
    testApplyUpdate(seq, 1/84f, 2/127f, 2)
    testApplyUpdate(seq, 0.5f, 64/127f, 64)
    testApplyUpdate(seq, 1, 1, 127)
    testApplyUpdate(seq, 2, 1, 127)
    testApplyUpdate(seq, Float.MaxValue, 1, 127)
    testApplyUpdate(seq, Float.PositiveInfinity, 1, 127)
  }

  private def testUByte(seq: DataSeq[Float1, UByte]) {
    testIndex(seq)

    testApplyUpdate(seq, Float.NegativeInfinity, 0, 0)
    testApplyUpdate(seq, -Float.MaxValue, 0, 0)
    testApplyUpdate(seq, -1, 0, 0)
    testApplyUpdate(seq, -0.5f, 0, 0)
    testApplyUpdate(seq, Float.NaN, 0, 0)
    testApplyUpdate(seq, 0, 0, 0)
    testApplyUpdate(seq, 1/511f, 0, 0)
    testApplyUpdate(seq, 1/510f, 1/255f, 1)
    testApplyUpdate(seq, 1/255f, 1/255f, 1)
    testApplyUpdate(seq, 1/171f, 1/255f, 1)
    testApplyUpdate(seq, 1/170f, 2/255f, 2)
    testApplyUpdate(seq, 0.25f, 64/255f, 64)
    testApplyUpdate(seq, 0.5f, 128/255f, -128)
    testApplyUpdate(seq, 1, 1, -1)
    testApplyUpdate(seq, 2, 1, -1)
    testApplyUpdate(seq, Float.MaxValue, 1, -1)
    testApplyUpdate(seq, Float.PositiveInfinity, 1, -1)
  }

  private def testSShort(seq: DataSeq[Float1, SShort]) {
    testIndex(seq)

    seq.asBuffer().put(seq.offset, -32768); assert(seq(0) == -1)

    testApplyUpdate(seq, Float.NegativeInfinity, -1, -32767)
    testApplyUpdate(seq, -Float.MaxValue, -1, -32767)
    testApplyUpdate(seq, -2, -1, -32767)
    testApplyUpdate(seq, -32768/32767f, -1, -32767)
    testApplyUpdate(seq, -1, -1, -32767)
    testApplyUpdate(seq, -0.5f, -16384/32767f, -16384)
    testApplyUpdate(seq, -1/21844f, -2/32767f, -2)
    testApplyUpdate(seq, -1/21845f, -1/32767f, -1)
    testApplyUpdate(seq, -1/32767f, -1/32767f, -1)
    testApplyUpdate(seq, -1/65534f, -1/32767f, -1)
    testApplyUpdate(seq, -1/65535f, 0, 0)
    testApplyUpdate(seq, Float.NaN, 0, 0)
    testApplyUpdate(seq, 0, 0, 0)
    testApplyUpdate(seq, 1/65535f, 0, 0)
    testApplyUpdate(seq, 1/65534f, 1/32767f, 1)
    testApplyUpdate(seq, 1/32767f, 1/32767f, 1)
    testApplyUpdate(seq, 1/21845f, 1/32767f, 1)
    testApplyUpdate(seq, 1/21844f, 2/32767f, 2)
    testApplyUpdate(seq, 0.5f, 16384/32767f, 16384)
    testApplyUpdate(seq, 1, 1, 32767)
    testApplyUpdate(seq, 2, 1, 32767)
    testApplyUpdate(seq, Float.MaxValue, 1, 32767)
    testApplyUpdate(seq, Float.PositiveInfinity, 1, 32767)
  }

  private def testUShort(seq: DataSeq[Float1, UShort]) {
    testIndex(seq)

    testApplyUpdate(seq, Float.NegativeInfinity, 0, 0)
    testApplyUpdate(seq, -Float.MaxValue, 0, 0)
    testApplyUpdate(seq, -1, 0, 0)
    testApplyUpdate(seq, -0.5f, 0, 0)
    testApplyUpdate(seq, Float.NaN, 0, 0)
    testApplyUpdate(seq, 0, 0, 0)
    testApplyUpdate(seq, 1/131071f, 0, 0)
    testApplyUpdate(seq, 1/131070f, 1/65535f, 1)
    testApplyUpdate(seq, 1/65535f, 1/65535f, 1)
    testApplyUpdate(seq, 1/43691f, 1/65535f, 1)
    testApplyUpdate(seq, 1/43690f, 2/65535f, 2)
    testApplyUpdate(seq, 0.25f, 16384/65535f, 16384)
    testApplyUpdate(seq, 0.5f, 32768/65535f, 32768)
    testApplyUpdate(seq, 1, 1, 65535)
    testApplyUpdate(seq, 2, 1, 65535)
    testApplyUpdate(seq, Float.MaxValue, 1, 65535)
    testApplyUpdate(seq, Float.PositiveInfinity, 1, 65535)
  }
  
  private def testSInt(seq: DataSeq[Float1, SInt]) {
    testIndex(seq)

    seq.asBuffer().put(seq.offset, Int.MinValue); assert(seq(0) == -1)

    testApplyUpdate(seq, Float.NegativeInfinity, -1, -2147483647)
    testApplyUpdate(seq, -Float.MaxValue, -1, -2147483647)
    testApplyUpdate(seq, -2, -1, -2147483647)
    testApplyUpdate(seq, Int.MinValue/2147483647f, -1, -2147483647)
    testApplyUpdate(seq, -1, -1, -2147483647)
    testApplyUpdate(seq, -0.5f, -1073741824/2147483647f, -1073741824)
//    testApplyUpdate(seq, -1/84f, -3/2147483647f, -3)
//    testApplyUpdate(seq, -1/84f, -2/2147483647f, -2)
    testApplyUpdate(seq, Float.NaN, 0, 0)
    testApplyUpdate(seq, 0, 0, 0)
//    testApplyUpdate(seq, 1/84f, 2/2147483647f, 2)
//    testApplyUpdate(seq, 1/84f, 3/2147483647f, 3)
    testApplyUpdate(seq, 0.5f, 1073741824/2147483647f, 1073741824)
    testApplyUpdate(seq, 1, 1, 2147483647f)
    testApplyUpdate(seq, 2, 1, 2147483647f)
    testApplyUpdate(seq, Float.MaxValue, 1, 2147483647)
    testApplyUpdate(seq, Float.PositiveInfinity, 1, 2147483647)
  }

//  private def testUInt(seq: DataSeq[Float1, UInt]) {
//    testIndex(seq)
//
//    testApplyUpdate(seq, Float.NegativeInfinity, 0, 0)
//    testApplyUpdate(seq, -Float.MaxValue, 0, 0)
//    testApplyUpdate(seq, -1, 0, 0)
//    testApplyUpdate(seq, -0.5f, 0, 0)
//    testApplyUpdate(seq, Float.NaN, 0, 0)
//    testApplyUpdate(seq, 0, 0, 0)
//    testApplyUpdate(seq, 1/511f, 0, 0)
//    testApplyUpdate(seq, 1/510f, 1/255f, 1)
//    testApplyUpdate(seq, 1/255f, 1/255f, 1)
//    testApplyUpdate(seq, 1/171f, 1/255f, 1)
//    testApplyUpdate(seq, 1/170f, 2/255f, 2)
//    testApplyUpdate(seq, 0.25f, 64/255f, 64)
//    testApplyUpdate(seq, 0.5f, 128/255f, -128)
//    testApplyUpdate(seq, 1, 1, -1)
//    testApplyUpdate(seq, 2, 1, -1)
//    testApplyUpdate(seq, Float.MaxValue, 1, -1)
//    testApplyUpdate(seq, Float.PositiveInfinity, 1, -1)
//  }
}
