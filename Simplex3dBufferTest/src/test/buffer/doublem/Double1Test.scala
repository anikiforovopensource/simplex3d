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
import FactoryTestUtil._
import TestUtil._
import ApplyUpdateTestUtil._
import CopyTestUtil._


/**
 * @author Aleksey Nikiforov (lex)
 */
class RDoubleTest extends FunSuite {

  test("Factories") {
    testArrayFromSize(DataArray[RDouble, SByte](_))
    testArrayFromData[RDouble, SByte](DataArray[RDouble, SByte](_))
    testBufferFromSize(DataBuffer[RDouble, SByte](_))
    testBufferFromData(DataBuffer[RDouble, SByte](_))
    testViewFromData(DataView[RDouble, SByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[RDouble, SByte](_))
    testReadViewFromData(ReadDataView[RDouble, SByte](_, _, _))
    testArrayFromCollection[RDouble, SByte]((a: IndexedSeq[Double]) => DataArray[RDouble, SByte](a: _*))
    testBufferFromCollection[RDouble, SByte]((a: IndexedSeq[Double]) => DataBuffer[RDouble, SByte](a: _*))

    testArrayFromSize(DataArray[RDouble, UByte](_))
    testArrayFromData[RDouble, UByte](DataArray[RDouble, UByte](_))
    testBufferFromSize(DataBuffer[RDouble, UByte](_))
    testBufferFromData(DataBuffer[RDouble, UByte](_))
    testViewFromData(DataView[RDouble, UByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[RDouble, UByte](_))
    testReadViewFromData(ReadDataView[RDouble, UByte](_, _, _))
    testArrayFromCollection[RDouble, UByte]((a: IndexedSeq[Double]) => DataArray[RDouble, UByte](a: _*))
    testBufferFromCollection[RDouble, UByte]((a: IndexedSeq[Double]) => DataBuffer[RDouble, UByte](a: _*))

    testArrayFromSize(DataArray[RDouble, SShort](_))
    testArrayFromData[RDouble, SShort](DataArray[RDouble, SShort](_))
    testBufferFromSize(DataBuffer[RDouble, SShort](_))
    testBufferFromData(DataBuffer[RDouble, SShort](_))
    testViewFromData(DataView[RDouble, SShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[RDouble, SShort](_))
    testReadViewFromData(ReadDataView[RDouble, SShort](_, _, _))
    testArrayFromCollection[RDouble, SShort]((a: IndexedSeq[Double]) => DataArray[RDouble, SShort](a: _*))
    testBufferFromCollection[RDouble, SShort]((a: IndexedSeq[Double]) => DataBuffer[RDouble, SShort](a: _*))

    testArrayFromSize(DataArray[RDouble, UShort](_))
    testArrayFromData[RDouble, UShort](DataArray[RDouble, UShort](_))
    testBufferFromSize(DataBuffer[RDouble, UShort](_))
    testBufferFromData(DataBuffer[RDouble, UShort](_))
    testViewFromData(DataView[RDouble, UShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[RDouble, UShort](_))
    testReadViewFromData(ReadDataView[RDouble, UShort](_, _, _))
    testArrayFromCollection[RDouble, UShort]((a: IndexedSeq[Double]) => DataArray[RDouble, UShort](a: _*))
    testBufferFromCollection[RDouble, UShort]((a: IndexedSeq[Double]) => DataBuffer[RDouble, UShort](a: _*))

    testArrayFromSize(DataArray[RDouble, SInt](_))
    testArrayFromData[RDouble, SInt](DataArray[RDouble, SInt](_))
    testBufferFromSize(DataBuffer[RDouble, SInt](_))
    testBufferFromData(DataBuffer[RDouble, SInt](_))
    testViewFromData(DataView[RDouble, SInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[RDouble, SInt](_))
    testReadViewFromData(ReadDataView[RDouble, SInt](_, _, _))
    testArrayFromCollection[RDouble, SInt]((a: IndexedSeq[Double]) => DataArray[RDouble, SInt](a: _*))
    testBufferFromCollection[RDouble, SInt]((a: IndexedSeq[Double]) => DataBuffer[RDouble, SInt](a: _*))

    testArrayFromSize(DataArray[RDouble, UInt](_))
    testArrayFromData[RDouble, UInt](DataArray[RDouble, UInt](_))
    testBufferFromSize(DataBuffer[RDouble, UInt](_))
    testBufferFromData(DataBuffer[RDouble, UInt](_))
    testViewFromData(DataView[RDouble, UInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[RDouble, UInt](_))
    testReadViewFromData(ReadDataView[RDouble, UInt](_, _, _))
    testArrayFromCollection[RDouble, UInt]((a: IndexedSeq[Double]) => DataArray[RDouble, UInt](a: _*))
    testBufferFromCollection[RDouble, UInt]((a: IndexedSeq[Double]) => DataBuffer[RDouble, UInt](a: _*))
    
    testArrayFromSize(DataArray[RDouble, HFloat](_))
    testArrayFromData[RDouble, HFloat](DataArray[RDouble, HFloat](_))
    testBufferFromSize(DataBuffer[RDouble, HFloat](_))
    testBufferFromData(DataBuffer[RDouble, HFloat](_))
    testViewFromData(DataView[RDouble, HFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[RDouble, HFloat](_))
    testReadViewFromData(ReadDataView[RDouble, HFloat](_, _, _))
    testArrayFromCollection[RDouble, HFloat]((a: IndexedSeq[Double]) => DataArray[RDouble, HFloat](a: _*))
    testBufferFromCollection[RDouble, HFloat]((a: IndexedSeq[Double]) => DataBuffer[RDouble, HFloat](a: _*))
    
    testArrayFromSize(DataArray[RDouble, RFloat](_))
    testArrayFromData[RDouble, RFloat](DataArray[RDouble, RFloat](_))
    testBufferFromSize(DataBuffer[RDouble, RFloat](_))
    testBufferFromData(DataBuffer[RDouble, RFloat](_))
    testViewFromData(DataView[RDouble, RFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[RDouble, RFloat](_))
    testReadViewFromData(ReadDataView[RDouble, RFloat](_, _, _))
    testArrayFromCollection[RDouble, RFloat]((a: IndexedSeq[Double]) => DataArray[RDouble, RFloat](a: _*))
    testBufferFromCollection[RDouble, RFloat]((a: IndexedSeq[Double]) => DataBuffer[RDouble, RFloat](a: _*))
    
    testArrayFromSize(DataArray[RDouble, RDouble](_))
    testArrayFromData[RDouble, RDouble](DataArray[RDouble, RDouble](_))
    testBufferFromSize(DataBuffer[RDouble, RDouble](_))
    testBufferFromData(DataBuffer[RDouble, RDouble](_))
    testViewFromData(DataView[RDouble, RDouble](_, _, _))
    testReadBufferFromData(ReadDataBuffer[RDouble, RDouble](_))
    testReadViewFromData(ReadDataView[RDouble, RDouble](_, _, _))
    testArrayFromCollection[RDouble, RDouble]((a: IndexedSeq[Double]) => DataArray[RDouble, RDouble](a: _*))
    testBufferFromCollection[RDouble, RDouble]((a: IndexedSeq[Double]) => DataBuffer[RDouble, RDouble](a: _*))
  }
  
  test("Copy") {
    testCopy(DataSeq[RDouble, UByte])
    testCopy(DataSeq[RDouble, SByte])
    testCopy(DataSeq[RDouble, UShort])
    testCopy(DataSeq[RDouble, SShort])
    testCopy(DataSeq[RDouble, UInt])
    testCopy(DataSeq[RDouble, SInt])
    testCopy(DataSeq[RDouble, HFloat])
    testCopy(DataSeq[RDouble, RFloat])
    testCopy(DataSeq[RDouble, RDouble])
  }
  
  private val size = 20
  
  test("Apply/Update") {
    testSByte(DataArray[RDouble, SByte](size))
    testSByte(DataBuffer[RDouble, SByte](size))
    testSByte(DataView[RDouble, SByte](genBuffer(size, Descriptors.RDoubleSByte)._1, 0, 2))
    testSByte(DataView[RDouble, SByte](genBuffer(size, Descriptors.RDoubleSByte)._1, 1, 2))
    
    testUByte(DataArray[RDouble, UByte](size))
    testUByte(DataBuffer[RDouble, UByte](size))
    testUByte(DataView[RDouble, UByte](genBuffer(size, Descriptors.RDoubleUByte)._1, 0, 2))
    testUByte(DataView[RDouble, UByte](genBuffer(size, Descriptors.RDoubleUByte)._1, 1, 2))
    
    testSShort(DataArray[RDouble, SShort](size))
    testSShort(DataBuffer[RDouble, SShort](size))
    testSShort(DataView[RDouble, SShort](genBuffer(size, Descriptors.RDoubleSShort)._1, 0, 2))
    testSShort(DataView[RDouble, SShort](genBuffer(size, Descriptors.RDoubleSShort)._1, 1, 2))
    
    testUShort(DataArray[RDouble, UShort](size))
    testUShort(DataBuffer[RDouble, UShort](size))
    testUShort(DataView[RDouble, UShort](genBuffer(size, Descriptors.RDoubleUShort)._1, 0, 2))
    testUShort(DataView[RDouble, UShort](genBuffer(size, Descriptors.RDoubleUShort)._1, 1, 2))
    
    testSInt(DataArray[RDouble, SInt](size))
    testSInt(DataBuffer[RDouble, SInt](size))
    testSInt(DataView[RDouble, SInt](genBuffer(size, Descriptors.RDoubleSInt)._1, 0, 2))
    testSInt(DataView[RDouble, SInt](genBuffer(size, Descriptors.RDoubleSInt)._1, 1, 2))
    
    testUInt(DataArray[RDouble, UInt](size))
    testUInt(DataBuffer[RDouble, UInt](size))
    testUInt(DataView[RDouble, UInt](genBuffer(size, Descriptors.RDoubleUInt)._1, 0, 2))
    testUInt(DataView[RDouble, UInt](genBuffer(size, Descriptors.RDoubleUInt)._1, 1, 2))
    
    testHFloat(DataArray[RDouble, HFloat](size))
    testHFloat(DataBuffer[RDouble, HFloat](size))
    testHFloat(DataView[RDouble, HFloat](genBuffer(size, Descriptors.RDoubleHFloat)._1, 0, 2))
    testHFloat(DataView[RDouble, HFloat](genBuffer(size, Descriptors.RDoubleHFloat)._1, 1, 2))
    
    testRFloat(DataArray[RDouble, RFloat](size))
    testRFloat(DataBuffer[RDouble, RFloat](size))
    testRFloat(DataView[RDouble, RFloat](genBuffer(size, Descriptors.RDoubleRFloat)._1, 0, 2))
    testRFloat(DataView[RDouble, RFloat](genBuffer(size, Descriptors.RDoubleRFloat)._1, 1, 2))
    
    testRDouble(DataArray[RDouble, RDouble](size))
    testRDouble(DataBuffer[RDouble, RDouble](size))
    testRDouble(DataView[RDouble, RDouble](genBuffer(size, Descriptors.RDoubleRDouble)._1, 0, 2))
    testRDouble(DataView[RDouble, RDouble](genBuffer(size, Descriptors.RDoubleRDouble)._1, 1, 2))
  }

  private def testFloatSByte(seq: DataSeq[RDouble, SByte], testValue: Double) {
    val converted = conversion.Double.toSByte(testValue)
    val convertedDack = conversion.Double.fromSByte(converted)
    testApplyUpdate(seq, testValue, convertedDack, converted)
  }
  private def testFloatUByte(seq: DataSeq[RDouble, UByte], testValue: Double) {
    val converted = conversion.Double.toUByte(testValue)
    val convertedDack = conversion.Double.fromUByte(converted)
    testApplyUpdate(seq, testValue, convertedDack, converted)
  }
  private def testFloatSShort(seq: DataSeq[RDouble, SShort], testValue: Double) {
    val converted = conversion.Double.toSShort(testValue)
    val convertedDack = conversion.Double.fromSShort(converted)
    testApplyUpdate(seq, testValue, convertedDack, converted)
  }
  private def testFloatUShort(seq: DataSeq[RDouble, UShort], testValue: Double) {
    val converted = conversion.Double.toUShort(testValue)
    val convertedDack = conversion.Double.fromUShort(converted)
    testApplyUpdate(seq, testValue, convertedDack, converted)
  }
  private def testFloatSInt(seq: DataSeq[RDouble, SInt], testValue: Double) {
    val converted = conversion.Double.toSInt(testValue)
    val convertedDack = conversion.Double.fromSInt(converted)
    testApplyUpdate(seq, testValue, convertedDack, converted)
  }
  private def testFloatUInt(seq: DataSeq[RDouble, UInt], testValue: Double) {
    val converted = conversion.Double.toUInt(testValue)
    val convertedDack = conversion.Double.fromUInt(converted)
    testApplyUpdate(seq, testValue, convertedDack, converted)
  }
  private def testFloatHFloat(seq: DataSeq[RDouble, HFloat], testValue: Double) {
    val converted = conversion.Double.toHFloat(testValue)
    val convertedDack = conversion.Double.fromHFloat(converted)
    testApplyUpdate(seq, testValue, convertedDack, converted)
  }

  private def testSByte(seq: DataSeq[RDouble, SByte]) {
    testIndex(seq)

    seq.buffer().put(seq.offset, -128); assert(seq(0) == -1)

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

  private def testUByte(seq: DataSeq[RDouble, UByte]) {
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

  private def testSShort(seq: DataSeq[RDouble, SShort]) {
    testIndex(seq)

    seq.buffer().put(seq.offset, -32768); assert(seq(0) == -1)

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

  private def testUShort(seq: DataSeq[RDouble, UShort]) {
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
  
  private def testSInt(seq: DataSeq[RDouble, SInt]) {
    testIndex(seq)

    seq.buffer().put(seq.offset, Int.MinValue); assert(seq(0) == -1)

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

  private def testUInt(seq: DataSeq[RDouble, UInt]) {
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
  
  private def testHFloat(seq: DataSeq[RDouble, HFloat]) {
    testIndex(seq)

    // Inf.
    testFloatHFloat(seq, Double.NegativeInfinity)
    testFloatHFloat(seq, Double.PositiveInfinity)

    // NaN.
    testFloatHFloat(seq, Double.NaN)

    // Zero.
    testFloatHFloat(seq, -0.0)
    testFloatHFloat(seq, 0)

    // Subnormal.
    testFloatHFloat(seq, floatFromBits("00000000 01000000 00000000 00000000").toDouble)
    testFloatHFloat(seq, floatFromBits("10000000 01000000 00000000 00000000").toDouble)

    // Out of range values.
    testFloatHFloat(seq, -Double.MaxValue)
    testFloatHFloat(seq, Double.MaxValue)

    // Min and Max
    testFloatHFloat(seq, -65504.0)
    testFloatHFloat(seq, 65504.0)

    // Closest to Zero.
    testFloatHFloat(seq, -6.103515625E-5)
    testFloatHFloat(seq, 6.103515625E-5)
    testFloatHFloat(seq, -java.lang.Double.MIN_VALUE)
    testFloatHFloat(seq, java.lang.Double.MIN_VALUE)

    // Rounding.
    testFloatHFloat(seq, floatFromBits("00111101 11000000 11100000 00000000").toDouble)
    testFloatHFloat(seq, floatFromBits("00111101 11000000 11110000 00000000").toDouble)
    testFloatHFloat(seq, floatFromBits("10111101 11000000 11100000 00000000").toDouble)
    testFloatHFloat(seq, floatFromBits("10111101 11000000 11110000 00000000").toDouble)

    // Normalized.
    testFloatHFloat(seq, -1)
    testFloatHFloat(seq, -0.5)
    testFloatHFloat(seq, 0.5)
    testFloatHFloat(seq, 1)

    // Round trip conversion excluding Subnormal, Zero, and NaN
    val buff = seq.buffer
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

  private def testRFloat(seq: DataSeq[RDouble, RFloat]) {
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
  
  private def testRDouble(seq: DataSeq[RDouble, RDouble]) {
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
