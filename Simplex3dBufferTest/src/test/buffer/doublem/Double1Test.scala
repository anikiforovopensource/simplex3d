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
package doublem

import org.scalatest._
import simplex3d.math.doublem._
import simplex3d.buffer._
import simplex3d.buffer.doublem._

import Descriptors._
import FactoryTest._
import TestUtil._
import ApplyUpdateTest._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Double1Test extends FunSuite {

  test("Factories") {
    testArrayFromSize(DataArray[Double1, SByte](_))
    testArrayFromData[Double1, SByte](DataArray[Double1, SByte](_))
    testBufferFromSize(DataBuffer[Double1, SByte](_))
    testBufferFromData(DataBuffer[Double1, SByte](_))
    testViewFromData(DataView[Double1, SByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Double1, SByte](_))
    testReadViewFromData(ReadDataView[Double1, SByte](_, _, _))
    testArrayFromCollection[Double1, SByte]((a: IndexedSeq[Double]) => DataArray[Double1, SByte](a: _*))
    testArrayFromCollection[Double1, SByte]((a: IndexedSeq[Double]) => DataArray[Double1, SByte](a))
    testBufferFromCollection[Double1, SByte]((a: IndexedSeq[Double]) => DataBuffer[Double1, SByte](a: _*))
    testBufferFromCollection[Double1, SByte]((a: IndexedSeq[Double]) => DataBuffer[Double1, SByte](a))

    testArrayFromSize(DataArray[Double1, UByte](_))
    testArrayFromData[Double1, UByte](DataArray[Double1, UByte](_))
    testBufferFromSize(DataBuffer[Double1, UByte](_))
    testBufferFromData(DataBuffer[Double1, UByte](_))
    testViewFromData(DataView[Double1, UByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Double1, UByte](_))
    testReadViewFromData(ReadDataView[Double1, UByte](_, _, _))
    testArrayFromCollection[Double1, UByte]((a: IndexedSeq[Double]) => DataArray[Double1, UByte](a: _*))
    testArrayFromCollection[Double1, UByte]((a: IndexedSeq[Double]) => DataArray[Double1, UByte](a))
    testBufferFromCollection[Double1, UByte]((a: IndexedSeq[Double]) => DataBuffer[Double1, UByte](a: _*))
    testBufferFromCollection[Double1, UByte]((a: IndexedSeq[Double]) => DataBuffer[Double1, UByte](a))

    testArrayFromSize(DataArray[Double1, SShort](_))
    testArrayFromData[Double1, SShort](DataArray[Double1, SShort](_))
    testBufferFromSize(DataBuffer[Double1, SShort](_))
    testBufferFromData(DataBuffer[Double1, SShort](_))
    testViewFromData(DataView[Double1, SShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Double1, SShort](_))
    testReadViewFromData(ReadDataView[Double1, SShort](_, _, _))
    testArrayFromCollection[Double1, SShort]((a: IndexedSeq[Double]) => DataArray[Double1, SShort](a: _*))
    testArrayFromCollection[Double1, SShort]((a: IndexedSeq[Double]) => DataArray[Double1, SShort](a))
    testBufferFromCollection[Double1, SShort]((a: IndexedSeq[Double]) => DataBuffer[Double1, SShort](a: _*))
    testBufferFromCollection[Double1, SShort]((a: IndexedSeq[Double]) => DataBuffer[Double1, SShort](a))

    testArrayFromSize(DataArray[Double1, UShort](_))
    testArrayFromData[Double1, UShort](DataArray[Double1, UShort](_))
    testBufferFromSize(DataBuffer[Double1, UShort](_))
    testBufferFromData(DataBuffer[Double1, UShort](_))
    testViewFromData(DataView[Double1, UShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Double1, UShort](_))
    testReadViewFromData(ReadDataView[Double1, UShort](_, _, _))
    testArrayFromCollection[Double1, UShort]((a: IndexedSeq[Double]) => DataArray[Double1, UShort](a: _*))
    testArrayFromCollection[Double1, UShort]((a: IndexedSeq[Double]) => DataArray[Double1, UShort](a))
    testBufferFromCollection[Double1, UShort]((a: IndexedSeq[Double]) => DataBuffer[Double1, UShort](a: _*))
    testBufferFromCollection[Double1, UShort]((a: IndexedSeq[Double]) => DataBuffer[Double1, UShort](a))

    testArrayFromSize(DataArray[Double1, SInt](_))
    testArrayFromData[Double1, SInt](DataArray[Double1, SInt](_))
    testBufferFromSize(DataBuffer[Double1, SInt](_))
    testBufferFromData(DataBuffer[Double1, SInt](_))
    testViewFromData(DataView[Double1, SInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Double1, SInt](_))
    testReadViewFromData(ReadDataView[Double1, SInt](_, _, _))
    testArrayFromCollection[Double1, SInt]((a: IndexedSeq[Double]) => DataArray[Double1, SInt](a: _*))
    testArrayFromCollection[Double1, SInt]((a: IndexedSeq[Double]) => DataArray[Double1, SInt](a))
    testBufferFromCollection[Double1, SInt]((a: IndexedSeq[Double]) => DataBuffer[Double1, SInt](a: _*))
    testBufferFromCollection[Double1, SInt]((a: IndexedSeq[Double]) => DataBuffer[Double1, SInt](a))

    testArrayFromSize(DataArray[Double1, UInt](_))
    testArrayFromData[Double1, UInt](DataArray[Double1, UInt](_))
    testBufferFromSize(DataBuffer[Double1, UInt](_))
    testBufferFromData(DataBuffer[Double1, UInt](_))
    testViewFromData(DataView[Double1, UInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Double1, UInt](_))
    testReadViewFromData(ReadDataView[Double1, UInt](_, _, _))
    testArrayFromCollection[Double1, UInt]((a: IndexedSeq[Double]) => DataArray[Double1, UInt](a: _*))
    testArrayFromCollection[Double1, UInt]((a: IndexedSeq[Double]) => DataArray[Double1, UInt](a))
    testBufferFromCollection[Double1, UInt]((a: IndexedSeq[Double]) => DataBuffer[Double1, UInt](a: _*))
    testBufferFromCollection[Double1, UInt]((a: IndexedSeq[Double]) => DataBuffer[Double1, UInt](a))
    
    testArrayFromSize(DataArray[Double1, HalfFloat](_))
    testArrayFromData[Double1, HalfFloat](DataArray[Double1, HalfFloat](_))
    testBufferFromSize(DataBuffer[Double1, HalfFloat](_))
    testBufferFromData(DataBuffer[Double1, HalfFloat](_))
    testViewFromData(DataView[Double1, HalfFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Double1, HalfFloat](_))
    testReadViewFromData(ReadDataView[Double1, HalfFloat](_, _, _))
    testArrayFromCollection[Double1, HalfFloat]((a: IndexedSeq[Double]) => DataArray[Double1, HalfFloat](a: _*))
    testArrayFromCollection[Double1, HalfFloat]((a: IndexedSeq[Double]) => DataArray[Double1, HalfFloat](a))
    testBufferFromCollection[Double1, HalfFloat]((a: IndexedSeq[Double]) => DataBuffer[Double1, HalfFloat](a: _*))
    testBufferFromCollection[Double1, HalfFloat]((a: IndexedSeq[Double]) => DataBuffer[Double1, HalfFloat](a))
    
    testArrayFromSize(DataArray[Double1, RawFloat](_))
    testArrayFromData[Double1, RawFloat](DataArray[Double1, RawFloat](_))
    testBufferFromSize(DataBuffer[Double1, RawFloat](_))
    testBufferFromData(DataBuffer[Double1, RawFloat](_))
    testViewFromData(DataView[Double1, RawFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Double1, RawFloat](_))
    testReadViewFromData(ReadDataView[Double1, RawFloat](_, _, _))
    testArrayFromCollection[Double1, RawFloat]((a: IndexedSeq[Double]) => DataArray[Double1, RawFloat](a: _*))
    testArrayFromCollection[Double1, RawFloat]((a: IndexedSeq[Double]) => DataArray[Double1, RawFloat](a))
    testBufferFromCollection[Double1, RawFloat]((a: IndexedSeq[Double]) => DataBuffer[Double1, RawFloat](a: _*))
    testBufferFromCollection[Double1, RawFloat]((a: IndexedSeq[Double]) => DataBuffer[Double1, RawFloat](a))
    
    testArrayFromSize(DataArray[Double1, RawDouble](_))
    testArrayFromData[Double1, RawDouble](DataArray[Double1, RawDouble](_))
    testBufferFromSize(DataBuffer[Double1, RawDouble](_))
    testBufferFromData(DataBuffer[Double1, RawDouble](_))
    testViewFromData(DataView[Double1, RawDouble](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Double1, RawDouble](_))
    testReadViewFromData(ReadDataView[Double1, RawDouble](_, _, _))
    testArrayFromCollection[Double1, RawDouble]((a: IndexedSeq[Double]) => DataArray[Double1, RawDouble](a: _*))
    testArrayFromCollection[Double1, RawDouble]((a: IndexedSeq[Double]) => DataArray[Double1, RawDouble](a))
    testBufferFromCollection[Double1, RawDouble]((a: IndexedSeq[Double]) => DataBuffer[Double1, RawDouble](a: _*))
    testBufferFromCollection[Double1, RawDouble]((a: IndexedSeq[Double]) => DataBuffer[Double1, RawDouble](a))
  }
  
  private val size = 20
  
  test("Apply/Update") {
    testSByte(DataArray[Double1, SByte](size))
    testSByte(DataBuffer[Double1, SByte](size))
    testSByte(DataView[Double1, SByte](genBuffer(size, Descriptors.Double1SByte)._1, 0, 2))
    testSByte(DataView[Double1, SByte](genBuffer(size, Descriptors.Double1SByte)._1, 1, 2))
    
    testUByte(DataArray[Double1, UByte](size))
    testUByte(DataBuffer[Double1, UByte](size))
    testUByte(DataView[Double1, UByte](genBuffer(size, Descriptors.Double1UByte)._1, 0, 2))
    testUByte(DataView[Double1, UByte](genBuffer(size, Descriptors.Double1UByte)._1, 1, 2))
    
    testSShort(DataArray[Double1, SShort](size))
    testSShort(DataBuffer[Double1, SShort](size))
    testSShort(DataView[Double1, SShort](genBuffer(size, Descriptors.Double1SShort)._1, 0, 2))
    testSShort(DataView[Double1, SShort](genBuffer(size, Descriptors.Double1SShort)._1, 1, 2))
    
    testUShort(DataArray[Double1, UShort](size))
    testUShort(DataBuffer[Double1, UShort](size))
    testUShort(DataView[Double1, UShort](genBuffer(size, Descriptors.Double1UShort)._1, 0, 2))
    testUShort(DataView[Double1, UShort](genBuffer(size, Descriptors.Double1UShort)._1, 1, 2))
    
    testSInt(DataArray[Double1, SInt](size))
    testSInt(DataBuffer[Double1, SInt](size))
    testSInt(DataView[Double1, SInt](genBuffer(size, Descriptors.Double1SInt)._1, 0, 2))
    testSInt(DataView[Double1, SInt](genBuffer(size, Descriptors.Double1SInt)._1, 1, 2))
    
    testUInt(DataArray[Double1, UInt](size))
    testUInt(DataBuffer[Double1, UInt](size))
    testUInt(DataView[Double1, UInt](genBuffer(size, Descriptors.Double1UInt)._1, 0, 2))
    testUInt(DataView[Double1, UInt](genBuffer(size, Descriptors.Double1UInt)._1, 1, 2))
    
    testHalfFloat(DataArray[Double1, HalfFloat](size))
    testHalfFloat(DataBuffer[Double1, HalfFloat](size))
    testHalfFloat(DataView[Double1, HalfFloat](genBuffer(size, Descriptors.Double1HalfFloat)._1, 0, 2))
    testHalfFloat(DataView[Double1, HalfFloat](genBuffer(size, Descriptors.Double1HalfFloat)._1, 1, 2))
    
    testRawFloat(DataArray[Double1, RawFloat](size))
    testRawFloat(DataBuffer[Double1, RawFloat](size))
    testRawFloat(DataView[Double1, RawFloat](genBuffer(size, Descriptors.Double1RawFloat)._1, 0, 2))
    testRawFloat(DataView[Double1, RawFloat](genBuffer(size, Descriptors.Double1RawFloat)._1, 1, 2))
    
    testRawDouble(DataArray[Double1, RawDouble](size))
    testRawDouble(DataBuffer[Double1, RawDouble](size))
    testRawDouble(DataView[Double1, RawDouble](genBuffer(size, Descriptors.Double1RawDouble)._1, 0, 2))
    testRawDouble(DataView[Double1, RawDouble](genBuffer(size, Descriptors.Double1RawDouble)._1, 1, 2))
  }

  private def testFloatSByte(seq: DataSeq[Double1, SByte], testValue: Double) {
    val converted = conversion.Double.toSByte(testValue)
    val convertedDack = conversion.Double.fromSByte(converted)
    testApplyUpdate(seq, testValue, convertedDack, converted)
  }
  private def testFloatUByte(seq: DataSeq[Double1, UByte], testValue: Double) {
    val converted = conversion.Double.toUByte(testValue)
    val convertedDack = conversion.Double.fromUByte(converted)
    testApplyUpdate(seq, testValue, convertedDack, converted)
  }
  private def testFloatSShort(seq: DataSeq[Double1, SShort], testValue: Double) {
    val converted = conversion.Double.toSShort(testValue)
    val convertedDack = conversion.Double.fromSShort(converted)
    testApplyUpdate(seq, testValue, convertedDack, converted)
  }
  private def testFloatUShort(seq: DataSeq[Double1, UShort], testValue: Double) {
    val converted = conversion.Double.toUShort(testValue)
    val convertedDack = conversion.Double.fromUShort(converted)
    testApplyUpdate(seq, testValue, convertedDack, converted)
  }
  private def testFloatSInt(seq: DataSeq[Double1, SInt], testValue: Double) {
    val converted = conversion.Double.toSInt(testValue)
    val convertedDack = conversion.Double.fromSInt(converted)
    testApplyUpdate(seq, testValue, convertedDack, converted)
  }
  private def testFloatUInt(seq: DataSeq[Double1, UInt], testValue: Double) {
    val converted = conversion.Double.toUInt(testValue)
    val convertedDack = conversion.Double.fromUInt(converted)
    testApplyUpdate(seq, testValue, convertedDack, converted)
  }
  private def testFloatHalfFloat(seq: DataSeq[Double1, HalfFloat], testValue: Double) {
    val converted = conversion.Double.toHalfFloat(testValue)
    val convertedDack = conversion.Double.fromHalfFloat(converted)
    testApplyUpdate(seq, testValue, convertedDack, converted)
  }

  private def testSByte(seq: DataSeq[Double1, SByte]) {
    testIndex(seq)

    seq.asBuffer().put(seq.offset, -128); assert(seq(0) == -1)

    testFloatSByte(seq, Double.NegativeInfinity)
    testFloatSByte(seq, -Double.MaxValue)
    testFloatSByte(seq, -2)
    testFloatSByte(seq, -128/127.0)
    testFloatSByte(seq, -1)
    testFloatSByte(seq, -0.5)
    testFloatSByte(seq, -1/84.0)
    testFloatSByte(seq, -1/85.0)
    testFloatSByte(seq, -1/127.0)
    testFloatSByte(seq, -1/254.0)
    testFloatSByte(seq, -1/255.0)
    testFloatSByte(seq, Double.NaN)
    testFloatSByte(seq, 0)
    testFloatSByte(seq, 1/255.0)
    testFloatSByte(seq, 1/254.0)
    testFloatSByte(seq, 1/127.0)
    testFloatSByte(seq, 1/85.0)
    testFloatSByte(seq, 1/84.0)
    testFloatSByte(seq, 0.5)
    testFloatSByte(seq, 1)
    testFloatSByte(seq, 2)
    testFloatSByte(seq, Double.MaxValue)
    testFloatSByte(seq, Double.PositiveInfinity)
  }

  private def testUByte(seq: DataSeq[Double1, UByte]) {
    testIndex(seq)

    testFloatUByte(seq, Double.NegativeInfinity)
    testFloatUByte(seq, -Double.MaxValue)
    testFloatUByte(seq, -1)
    testFloatUByte(seq, -0.5)
    testFloatUByte(seq, Double.NaN)
    testFloatUByte(seq, 0)
    testFloatUByte(seq, 1/511.0)
    testFloatUByte(seq, 1/510.0)
    testFloatUByte(seq, 1/255.0)
    testFloatUByte(seq, 1/171.0)
    testFloatUByte(seq, 1/170.0)
    testFloatUByte(seq, 0.25)
    testFloatUByte(seq, 0.5)
    testFloatUByte(seq, 1)
    testFloatUByte(seq, 2)
    testFloatUByte(seq, Double.MaxValue)
    testFloatUByte(seq, Double.PositiveInfinity)
  }

  private def testSShort(seq: DataSeq[Double1, SShort]) {
    testIndex(seq)

    seq.asBuffer().put(seq.offset, -32768); assert(seq(0) == -1)

    testFloatSShort(seq, Double.NegativeInfinity)
    testFloatSShort(seq, -Double.MaxValue)
    testFloatSShort(seq, -2)
    testFloatSShort(seq, -32768/32767.0)
    testFloatSShort(seq, -1)
    testFloatSShort(seq, -0.5)
    testFloatSShort(seq, -1/21844.0)
    testFloatSShort(seq, -1/21845.0)
    testFloatSShort(seq, -1/32767.0)
    testFloatSShort(seq, -1/65534.0)
    testFloatSShort(seq, -1/65535.0)
    testFloatSShort(seq, Double.NaN)
    testFloatSShort(seq, 0)
    testFloatSShort(seq, 1/65535.0)
    testFloatSShort(seq, 1/65534.0)
    testFloatSShort(seq, 1/32767.0)
    testFloatSShort(seq, 1/21845.0)
    testFloatSShort(seq, 1/21844.0)
    testFloatSShort(seq, 0.5)
    testFloatSShort(seq, 1)
    testFloatSShort(seq, 2)
    testFloatSShort(seq, Double.MaxValue)
    testFloatSShort(seq, Double.PositiveInfinity)
  }

  private def testUShort(seq: DataSeq[Double1, UShort]) {
    testIndex(seq)

    testFloatUShort(seq, Double.NegativeInfinity)
    testFloatUShort(seq, -Double.MaxValue)
    testFloatUShort(seq, -1)
    testFloatUShort(seq, -0.5)
    testFloatUShort(seq, Double.NaN)
    testFloatUShort(seq, 0)
    testFloatUShort(seq, 1/131071.0)
    testFloatUShort(seq, 1/131070.0)
    testFloatUShort(seq, 1/65535.0)
    testFloatUShort(seq, 1/43691.0)
    testFloatUShort(seq, 1/43690.0)
    testFloatUShort(seq, 0.25)
    testFloatUShort(seq, 0.5)
    testFloatUShort(seq, 1)
    testFloatUShort(seq, 2)
    testFloatUShort(seq, Double.MaxValue)
    testFloatUShort(seq, Double.PositiveInfinity)
  }
  
  private def testSInt(seq: DataSeq[Double1, SInt]) {
    testIndex(seq)

    seq.asBuffer().put(seq.offset, Int.MinValue); assert(seq(0) == -1)

    testFloatSInt(seq, Double.NegativeInfinity)
    testFloatSInt(seq, -Double.MaxValue)
    testFloatSInt(seq, -2)
    testFloatSInt(seq, Int.MinValue/2147483647.0)
    testFloatSInt(seq, -1)
    testFloatSInt(seq, -0.5)
    testFloatSInt(seq, -1/858993458.0)
    testFloatSInt(seq, -1/858993459.0)
    testFloatSInt(seq, -2/2147483647.0)
    testFloatSInt(seq, -1/1431655764.0)
    testFloatSInt(seq, -1/1431655765.0)
    testFloatSInt(seq, Double.NaN)
    testFloatSInt(seq, 0)
    testFloatSInt(seq, 1/1431655765.0)
    testFloatSInt(seq, 1/1431655764.0)
    testFloatSInt(seq, 2/2147483647.0)
    testFloatSInt(seq, 1/858993459.0)
    testFloatSInt(seq, 1/858993458.0)
    testFloatSInt(seq, 0.5)
    testFloatSInt(seq, 1)
    testFloatSInt(seq, 2)
    testFloatSInt(seq, Double.MaxValue)
    testFloatSInt(seq, Double.PositiveInfinity)
  }

  private def testUInt(seq: DataSeq[Double1, UInt]) {
    testIndex(seq)

    testFloatUInt(seq, Double.NegativeInfinity)
    testFloatUInt(seq, -Double.MaxValue)
    testFloatUInt(seq, -1)
    testFloatUInt(seq, -0.5)
    testFloatUInt(seq, Double.NaN)
    testFloatUInt(seq, 0)
    testFloatUInt(seq, 1/2863311531.0)
    testFloatUInt(seq, 1/2863311530.0)
    testFloatUInt(seq, 2/4294967295.0)
    testFloatUInt(seq, 1/1717986919.0)
    testFloatUInt(seq, 1/1717986918.0)
    testFloatUInt(seq, 0.25)
    testFloatUInt(seq, 0.5)
    testFloatUInt(seq, 1)
    testFloatUInt(seq, 2)
    testFloatUInt(seq, Double.MaxValue)
    testFloatUInt(seq, Double.PositiveInfinity)
  }
  
  private def testHalfFloat(seq: DataSeq[Double1, HalfFloat]) {
    testIndex(seq)

    // Inf.
    testFloatHalfFloat(seq, Double.NegativeInfinity)
    testFloatHalfFloat(seq, Double.PositiveInfinity)

    // NaN.
    testFloatHalfFloat(seq, Double.NaN)

    // Zero.
    testFloatHalfFloat(seq, -0.0)
    testFloatHalfFloat(seq, 0)

    // Subnormal.
    testFloatHalfFloat(seq, floatFromBits("00000000 01000000 00000000 00000000").toDouble)
    testFloatHalfFloat(seq, floatFromBits("10000000 01000000 00000000 00000000").toDouble)

    // Out of range values.
    testFloatHalfFloat(seq, -Double.MaxValue)
    testFloatHalfFloat(seq, Double.MaxValue)

    // Min and Max
    testFloatHalfFloat(seq, -65504.0)
    testFloatHalfFloat(seq, 65504.0)

    // Closest to Zero.
    testFloatHalfFloat(seq, -6.103515625E-5)
    testFloatHalfFloat(seq, 6.103515625E-5)
    testFloatHalfFloat(seq, -java.lang.Double.MIN_VALUE)
    testFloatHalfFloat(seq, java.lang.Double.MIN_VALUE)

    // Rounding.
    testFloatHalfFloat(seq, floatFromBits("00111101 11000000 11100000 00000000").toDouble)
    testFloatHalfFloat(seq, floatFromBits("00111101 11000000 11110000 00000000").toDouble)
    testFloatHalfFloat(seq, floatFromBits("10111101 11000000 11100000 00000000").toDouble)
    testFloatHalfFloat(seq, floatFromBits("10111101 11000000 11110000 00000000").toDouble)

    // Normalized.
    testFloatHalfFloat(seq, -1)
    testFloatHalfFloat(seq, -0.5)
    testFloatHalfFloat(seq, 0.5)
    testFloatHalfFloat(seq, 1)

    // Round trip conversion excluding Subnormal, Zero, and NaN
    val buff = seq.asBuffer
    def putRaw(v: Short) { buff.put(seq.offset, v) }
    def getRaw() = buff.get(seq.offset + seq.stride)
    
    var i = 0; while (i < 65536) {
      val t = (i & 0x7C00)
      if (t != 0 && t != 0x7C00) {
        val h = i.toShort
        
        putRaw(h)
        seq(1) = seq(0)
        assert(h == getRaw())
      }

      i += 1
    }
  }

  private def testRawFloat(seq: DataSeq[Double1, RawFloat]) {
    testIndex(seq)

    testApplyUpdate(seq, Float.NegativeInfinity.toDouble, Float.NegativeInfinity, Float.NegativeInfinity)
    testApplyUpdate(seq, Float.PositiveInfinity.toDouble, Float.PositiveInfinity, Float.PositiveInfinity)

    testApplyUpdate(seq, -Float.MaxValue.toDouble, -Float.MaxValue, -Float.MaxValue)
    testApplyUpdate(seq, Float.MaxValue.toDouble, Float.MaxValue, Float.MaxValue)

    testApplyUpdate(seq, -java.lang.Float.MIN_VALUE.toDouble, -java.lang.Float.MIN_VALUE, -java.lang.Float.MIN_VALUE)
    testApplyUpdate(seq, java.lang.Float.MIN_VALUE.toDouble, java.lang.Float.MIN_VALUE, java.lang.Float.MIN_VALUE)

    testApplyUpdate(seq, Float.NaN.toDouble, Float.NaN, Float.NaN)
    testApplyUpdate(seq, 0.0, 0, 0)

    testApplyUpdate(seq, -0.5, -0.5, -0.5f)
    testApplyUpdate(seq, -1.5, -1.5, -1.5f)
    testApplyUpdate(seq, 0.5, 0.5, 0.5f)
    testApplyUpdate(seq, 1.5, 1.5, 1.5f)
  }
  
  private def testRawDouble(seq: DataSeq[Double1, RawDouble]) {
    testIndex(seq)

    testApplyUpdate(seq, Double.NegativeInfinity, Double.NegativeInfinity, Double.NegativeInfinity)
    testApplyUpdate(seq, Double.PositiveInfinity, Double.PositiveInfinity, Double.PositiveInfinity)

    testApplyUpdate(seq, -Double.MaxValue, -Double.MaxValue, -Double.MaxValue)
    testApplyUpdate(seq, Double.MaxValue, Double.MaxValue, Double.MaxValue)

    testApplyUpdate(seq, -java.lang.Double.MIN_VALUE, -java.lang.Double.MIN_VALUE, -java.lang.Double.MIN_VALUE)
    testApplyUpdate(seq, java.lang.Double.MIN_VALUE, java.lang.Double.MIN_VALUE, java.lang.Double.MIN_VALUE)

    testApplyUpdate(seq, Double.NaN, Double.NaN, Double.NaN)
    testApplyUpdate(seq, 0, 0, 0)

    testApplyUpdate(seq, -0.5, -0.5, -0.5)
    testApplyUpdate(seq, -1.5, -1.5, -1.5)
    testApplyUpdate(seq, 0.5, 0.5, 0.5)
    testApplyUpdate(seq, 1.5, 1.5, 1.5)
  }
}
