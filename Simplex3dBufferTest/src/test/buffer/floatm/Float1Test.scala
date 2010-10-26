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
import ApplyUpdateTest._


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
    
    testUInt(DataArray[Float1, UInt](size))
    testUInt(DataBuffer[Float1, UInt](size))
    testUInt(DataView[Float1, UInt](genBuffer(size, Descriptors.Float1UInt)._1, 0, 2))
    testUInt(DataView[Float1, UInt](genBuffer(size, Descriptors.Float1UInt)._1, 1, 2))
    
    testHalfFloat(DataArray[Float1, HalfFloat](size))
    testHalfFloat(DataBuffer[Float1, HalfFloat](size))
    testHalfFloat(DataView[Float1, HalfFloat](genBuffer(size, Descriptors.Float1HalfFloat)._1, 0, 2))
    testHalfFloat(DataView[Float1, HalfFloat](genBuffer(size, Descriptors.Float1HalfFloat)._1, 1, 2))
    
    testRawFloat(DataArray[Float1, RawFloat](size))
    testRawFloat(DataBuffer[Float1, RawFloat](size))
    testRawFloat(DataView[Float1, RawFloat](genBuffer(size, Descriptors.Float1RawFloat)._1, 0, 2))
    testRawFloat(DataView[Float1, RawFloat](genBuffer(size, Descriptors.Float1RawFloat)._1, 1, 2))
  }

  private def testFloatSByte(seq: DataSeq[Float1, SByte], testValue: Float) {
    val converted = conversion.Float.toSByte(testValue)
    val convertedDack = conversion.Float.fromSByte(converted)
    testApplyUpdate(seq, testValue, convertedDack, converted)
  }
  private def testFloatUByte(seq: DataSeq[Float1, UByte], testValue: Float) {
    val converted = conversion.Float.toUByte(testValue)
    val convertedDack = conversion.Float.fromUByte(converted)
    testApplyUpdate(seq, testValue, convertedDack, converted)
  }
  private def testFloatSShort(seq: DataSeq[Float1, SShort], testValue: Float) {
    val converted = conversion.Float.toSShort(testValue)
    val convertedDack = conversion.Float.fromSShort(converted)
    testApplyUpdate(seq, testValue, convertedDack, converted)
  }
  private def testFloatUShort(seq: DataSeq[Float1, UShort], testValue: Float) {
    val converted = conversion.Float.toUShort(testValue)
    val convertedDack = conversion.Float.fromUShort(converted)
    testApplyUpdate(seq, testValue, convertedDack, converted)
  }
  private def testFloatSInt(seq: DataSeq[Float1, SInt], testValue: Float) {
    val converted = conversion.Float.toSInt(testValue)
    val convertedDack = conversion.Float.fromSInt(converted)
    testApplyUpdate(seq, testValue, convertedDack, converted)
  }
  private def testFloatUInt(seq: DataSeq[Float1, UInt], testValue: Float) {
    val converted = conversion.Float.toUInt(testValue)
    val convertedDack = conversion.Float.fromUInt(converted)
    testApplyUpdate(seq, testValue, convertedDack, converted)
  }
  private def testFloatHalfFloat(seq: DataSeq[Float1, HalfFloat], testValue: Float) {
    val converted = conversion.Float.toHalfFloat(testValue)
    val convertedDack = conversion.Float.fromHalfFloat(converted)
    testApplyUpdate(seq, testValue, convertedDack, converted)
  }

  private def testSByte(seq: DataSeq[Float1, SByte]) {
    testIndex(seq)

    seq.asBuffer().put(seq.offset, -128); assert(seq(0) == -1)

    testFloatSByte(seq, Float.NegativeInfinity)
    testFloatSByte(seq, -Float.MaxValue)
    testFloatSByte(seq, -2)
    testFloatSByte(seq, -128/127f)
    testFloatSByte(seq, -1)
    testFloatSByte(seq, -0.5f)
    testFloatSByte(seq, -1/84f)
    testFloatSByte(seq, -1/85f)
    testFloatSByte(seq, -1/127f)
    testFloatSByte(seq, -1/254f)
    testFloatSByte(seq, -1/255f)
    testFloatSByte(seq, Float.NaN)
    testFloatSByte(seq, 0)
    testFloatSByte(seq, 1/255f)
    testFloatSByte(seq, 1/254f)
    testFloatSByte(seq, 1/127f)
    testFloatSByte(seq, 1/85f)
    testFloatSByte(seq, 1/84f)
    testFloatSByte(seq, 0.5f)
    testFloatSByte(seq, 1)
    testFloatSByte(seq, 2)
    testFloatSByte(seq, Float.MaxValue)
    testFloatSByte(seq, Float.PositiveInfinity)
  }

  private def testUByte(seq: DataSeq[Float1, UByte]) {
    testIndex(seq)

    testFloatUByte(seq, Float.NegativeInfinity)
    testFloatUByte(seq, -Float.MaxValue)
    testFloatUByte(seq, -1)
    testFloatUByte(seq, -0.5f)
    testFloatUByte(seq, Float.NaN)
    testFloatUByte(seq, 0)
    testFloatUByte(seq, 1/511f)
    testFloatUByte(seq, 1/510f)
    testFloatUByte(seq, 1/255f)
    testFloatUByte(seq, 1/171f)
    testFloatUByte(seq, 1/170f)
    testFloatUByte(seq, 0.25f)
    testFloatUByte(seq, 0.5f)
    testFloatUByte(seq, 1)
    testFloatUByte(seq, 2)
    testFloatUByte(seq, Float.MaxValue)
    testFloatUByte(seq, Float.PositiveInfinity)
  }

  private def testSShort(seq: DataSeq[Float1, SShort]) {
    testIndex(seq)

    seq.asBuffer().put(seq.offset, -32768); assert(seq(0) == -1)

    testFloatSShort(seq, Float.NegativeInfinity)
    testFloatSShort(seq, -Float.MaxValue)
    testFloatSShort(seq, -2)
    testFloatSShort(seq, -32768/32767f)
    testFloatSShort(seq, -1)
    testFloatSShort(seq, -0.5f)
    testFloatSShort(seq, -1/21844f)
    testFloatSShort(seq, -1/21845f)
    testFloatSShort(seq, -1/32767f)
    testFloatSShort(seq, -1/65534f)
    testFloatSShort(seq, -1/65535f)
    testFloatSShort(seq, Float.NaN)
    testFloatSShort(seq, 0)
    testFloatSShort(seq, 1/65535f)
    testFloatSShort(seq, 1/65534f)
    testFloatSShort(seq, 1/32767f)
    testFloatSShort(seq, 1/21845f)
    testFloatSShort(seq, 1/21844f)
    testFloatSShort(seq, 0.5f)
    testFloatSShort(seq, 1)
    testFloatSShort(seq, 2)
    testFloatSShort(seq, Float.MaxValue)
    testFloatSShort(seq, Float.PositiveInfinity)
  }

  private def testUShort(seq: DataSeq[Float1, UShort]) {
    testIndex(seq)

    testFloatUShort(seq, Float.NegativeInfinity)
    testFloatUShort(seq, -Float.MaxValue)
    testFloatUShort(seq, -1)
    testFloatUShort(seq, -0.5f)
    testFloatUShort(seq, Float.NaN)
    testFloatUShort(seq, 0)
    testFloatUShort(seq, 1/131071f)
    testFloatUShort(seq, 1/131070f)
    testFloatUShort(seq, 1/65535f)
    testFloatUShort(seq, 1/43691f)
    testFloatUShort(seq, 1/43690f)
    testFloatUShort(seq, 0.25f)
    testFloatUShort(seq, 0.5f)
    testFloatUShort(seq, 1)
    testFloatUShort(seq, 2)
    testFloatUShort(seq, Float.MaxValue)
    testFloatUShort(seq, Float.PositiveInfinity)
  }
  
  private def testSInt(seq: DataSeq[Float1, SInt]) {
    testIndex(seq)

    seq.asBuffer().put(seq.offset, Int.MinValue); assert(seq(0) == -1)

    testFloatSInt(seq, Float.NegativeInfinity)
    testFloatSInt(seq, -Float.MaxValue)
    testFloatSInt(seq, -2)
    testFloatSInt(seq, Int.MinValue/2147483647f)
    testFloatSInt(seq, -1)
    testFloatSInt(seq, -0.5f)
    testFloatSInt(seq, -1/858993440f)
    testFloatSInt(seq, -1/858993441f)
    testFloatSInt(seq, -2/2147483647f)
    testFloatSInt(seq, -1/1431655744f)
    testFloatSInt(seq, -1/1431655745f)
    testFloatSInt(seq, Float.NaN)
    testFloatSInt(seq, 0)
    testFloatSInt(seq, 1/1431655745f)
    testFloatSInt(seq, 1/1431655744f)
    testFloatSInt(seq, 2/2147483647f)
    testFloatSInt(seq, 1/858993441f)
    testFloatSInt(seq, 1/858993440f)
    testFloatSInt(seq, 0.5f)
    testFloatSInt(seq, 1)
    testFloatSInt(seq, 2)
    testFloatSInt(seq, Float.MaxValue)
    testFloatSInt(seq, Float.PositiveInfinity)
  }

  private def testUInt(seq: DataSeq[Float1, UInt]) {
    testIndex(seq)

    testFloatUInt(seq, Float.NegativeInfinity)
    testFloatUInt(seq, -Float.MaxValue)
    testFloatUInt(seq, -1)
    testFloatUInt(seq, -0.5f)
    testFloatUInt(seq, Float.NaN)
    testFloatUInt(seq, 0)
    testFloatUInt(seq, 1/2863311489f)
    testFloatUInt(seq, 1/2863311488f)
    testFloatUInt(seq, 2/4294967295f)
    testFloatUInt(seq, 1/1717986881f)
    testFloatUInt(seq, 1/1717986880f)
    testFloatUInt(seq, 0.25f)
    testFloatUInt(seq, 0.5f)
    testFloatUInt(seq, 1)
    testFloatUInt(seq, 2)
    testFloatUInt(seq, Float.MaxValue)
    testFloatUInt(seq, Float.PositiveInfinity)
  }
  
  private def testHalfFloat(seq: DataSeq[Float1, HalfFloat]) {
    testIndex(seq)

    // Inf.
    testFloatHalfFloat(seq, Float.NegativeInfinity)
    testFloatHalfFloat(seq, Float.PositiveInfinity)

    // NaN.
    testFloatHalfFloat(seq, Float.NaN)

    // More NaN.
    testFloatHalfFloat(seq, floatFromBits("01111111 11100000 00000000 00000000"))
    testFloatHalfFloat(seq, floatFromBits("01111111 10100000 00000000 00000000"))
    testFloatHalfFloat(seq, floatFromBits("11111111 11100000 00000000 00000000"))
    testFloatHalfFloat(seq, floatFromBits("11111111 10100000 00000000 00000000"))

    // Zero.
    testFloatHalfFloat(seq, -0f)
    testFloatHalfFloat(seq, 0)

    // Subnormal.
    testFloatHalfFloat(seq, floatFromBits("00000000 01000000 00000000 00000000"))
    testFloatHalfFloat(seq, floatFromBits("10000000 01000000 00000000 00000000"))

    // Out of range values.
    testFloatHalfFloat(seq, -Float.MaxValue)
    testFloatHalfFloat(seq, Float.MaxValue)

    // Min and Max
    testFloatHalfFloat(seq, -65504.0f)
    testFloatHalfFloat(seq, 65504.0f)

    // Closest to Zero.
    testFloatHalfFloat(seq, -6.103515625E-5f)
    testFloatHalfFloat(seq, 6.103515625E-5f)
    testFloatHalfFloat(seq, -java.lang.Float.MIN_VALUE)
    testFloatHalfFloat(seq, java.lang.Float.MIN_VALUE)

    // Rounding.
    testFloatHalfFloat(seq, floatFromBits("00111101 11000000 11100000 00000000"))
    testFloatHalfFloat(seq, floatFromBits("00111101 11000000 11110000 00000000"))
    testFloatHalfFloat(seq, floatFromBits("10111101 11000000 11100000 00000000"))
    testFloatHalfFloat(seq, floatFromBits("10111101 11000000 11110000 00000000"))

    // Normalized.
    testFloatHalfFloat(seq, -1)
    testFloatHalfFloat(seq, -0.5f)
    testFloatHalfFloat(seq, 0.5f)
    testFloatHalfFloat(seq, 1)

    // Round trip conversion excluding Subnormal and Zero.
    val buff = seq.asBuffer
    def putRaw(v: Short) { buff.put(seq.offset, v) }
    def getRaw() = buff.get(seq.offset + seq.stride)
    
    var i = 0; while (i < 65536) {
      if ((i & 0x7C00) != 0) {
        val h = i.toShort
        
        putRaw(h)
        seq(1) = seq(0)
        assert(h == getRaw())
      }

      i += 1
    }
  }
  
  private def testRawFloat(seq: DataSeq[Float1, RawFloat]) {
    testIndex(seq)

    testApplyUpdate(seq, Float.NegativeInfinity, Float.NegativeInfinity, Float.NegativeInfinity)
    testApplyUpdate(seq, Float.PositiveInfinity, Float.PositiveInfinity, Float.PositiveInfinity)

    testApplyUpdate(seq, -Float.MaxValue, -Float.MaxValue, -Float.MaxValue)
    testApplyUpdate(seq, Float.MaxValue, Float.MaxValue, Float.MaxValue)

    testApplyUpdate(seq, -java.lang.Float.MIN_VALUE, -java.lang.Float.MIN_VALUE, -java.lang.Float.MIN_VALUE)
    testApplyUpdate(seq, java.lang.Float.MIN_VALUE, java.lang.Float.MIN_VALUE, java.lang.Float.MIN_VALUE)

    testApplyUpdate(seq, Float.NaN, Float.NaN, Float.NaN)
    testApplyUpdate(seq, 0, 0, 0)

    testApplyUpdate(seq, -0.5f, -0.5f, -0.5f)
    testApplyUpdate(seq, -1.5f, -1.5f, -1.5f)
    testApplyUpdate(seq, 0.5f, 0.5f, 0.5f)
    testApplyUpdate(seq, 1.5f, 1.5f, 1.5f)
  }
}
