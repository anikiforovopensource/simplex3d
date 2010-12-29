/*
 * Simplex3d, DataTest package
 * Copyright (C) 2010, Simplex3d Team
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

package test.data
package double

import org.scalatest._
import simplex3d.math.doublex._
import simplex3d.data._
import simplex3d.data.double._

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

  private def testRDoubleSByte(seq: DataSeq[RDouble, SByte], testValue: Double) {
    val converted = conversion.Double.toSByte(testValue)
    val convertedDack = conversion.Double.fromSByte(converted)
    testApplyUpdate(seq, testValue, convertedDack, converted)
  }
  private def testRDoubleUByte(seq: DataSeq[RDouble, UByte], testValue: Double) {
    val converted = conversion.Double.toUByte(testValue)
    val convertedDack = conversion.Double.fromUByte(converted)
    testApplyUpdate(seq, testValue, convertedDack, converted)
  }
  private def testRDoubleSShort(seq: DataSeq[RDouble, SShort], testValue: Double) {
    val converted = conversion.Double.toSShort(testValue)
    val convertedDack = conversion.Double.fromSShort(converted)
    testApplyUpdate(seq, testValue, convertedDack, converted)
  }
  private def testRDoubleUShort(seq: DataSeq[RDouble, UShort], testValue: Double) {
    val converted = conversion.Double.toUShort(testValue)
    val convertedDack = conversion.Double.fromUShort(converted)
    testApplyUpdate(seq, testValue, convertedDack, converted)
  }
  private def testRDoubleSInt(seq: DataSeq[RDouble, SInt], testValue: Double) {
    val converted = conversion.Double.toSInt(testValue)
    val convertedDack = conversion.Double.fromSInt(converted)
    testApplyUpdate(seq, testValue, convertedDack, converted)
  }
  private def testRDoubleUInt(seq: DataSeq[RDouble, UInt], testValue: Double) {
    val converted = conversion.Double.toUInt(testValue)
    val convertedDack = conversion.Double.fromUInt(converted)
    testApplyUpdate(seq, testValue, convertedDack, converted)
  }
  private def testRDoubleHFloat(seq: DataSeq[RDouble, HFloat], testValue: Double) {
    val converted = conversion.Double.toHFloat(testValue)
    val convertedDack = conversion.Double.fromHFloat(converted)
    testApplyUpdate(seq, testValue, convertedDack, converted)
  }

  private def testSByte(seq: DataSeq[RDouble, SByte]) {
    testIndex(seq)

    seq.buffer().put(seq.offset, -128); assert(seq(0) == -1)

    testRDoubleSByte(seq, Double.NegativeInfinity)
    testRDoubleSByte(seq, -Double.MaxValue)
    testRDoubleSByte(seq, -2)
    testRDoubleSByte(seq, -128/127.0)
    testRDoubleSByte(seq, -1)
    testRDoubleSByte(seq, -0.5)
    testRDoubleSByte(seq, -1/84.0)
    testRDoubleSByte(seq, -1/85.0)
    testRDoubleSByte(seq, -1/127.0)
    testRDoubleSByte(seq, -1/254.0)
    testRDoubleSByte(seq, -1/255.0)
    testRDoubleSByte(seq, Double.NaN)
    testRDoubleSByte(seq, 0)
    testRDoubleSByte(seq, 1/255.0)
    testRDoubleSByte(seq, 1/254.0)
    testRDoubleSByte(seq, 1/127.0)
    testRDoubleSByte(seq, 1/85.0)
    testRDoubleSByte(seq, 1/84.0)
    testRDoubleSByte(seq, 0.5)
    testRDoubleSByte(seq, 1)
    testRDoubleSByte(seq, 2)
    testRDoubleSByte(seq, Double.MaxValue)
    testRDoubleSByte(seq, Double.PositiveInfinity)
  }

  private def testUByte(seq: DataSeq[RDouble, UByte]) {
    testIndex(seq)

    testRDoubleUByte(seq, Double.NegativeInfinity)
    testRDoubleUByte(seq, -Double.MaxValue)
    testRDoubleUByte(seq, -1)
    testRDoubleUByte(seq, -0.5)
    testRDoubleUByte(seq, Double.NaN)
    testRDoubleUByte(seq, 0)
    testRDoubleUByte(seq, 1/511.0)
    testRDoubleUByte(seq, 1/510.0)
    testRDoubleUByte(seq, 1/255.0)
    testRDoubleUByte(seq, 1/171.0)
    testRDoubleUByte(seq, 1/170.0)
    testRDoubleUByte(seq, 0.25)
    testRDoubleUByte(seq, 0.5)
    testRDoubleUByte(seq, 1)
    testRDoubleUByte(seq, 2)
    testRDoubleUByte(seq, Double.MaxValue)
    testRDoubleUByte(seq, Double.PositiveInfinity)
  }

  private def testSShort(seq: DataSeq[RDouble, SShort]) {
    testIndex(seq)

    seq.buffer().put(seq.offset, -32768); assert(seq(0) == -1)

    testRDoubleSShort(seq, Double.NegativeInfinity)
    testRDoubleSShort(seq, -Double.MaxValue)
    testRDoubleSShort(seq, -2)
    testRDoubleSShort(seq, -32768/32767.0)
    testRDoubleSShort(seq, -1)
    testRDoubleSShort(seq, -0.5)
    testRDoubleSShort(seq, -1/21844.0)
    testRDoubleSShort(seq, -1/21845.0)
    testRDoubleSShort(seq, -1/32767.0)
    testRDoubleSShort(seq, -1/65534.0)
    testRDoubleSShort(seq, -1/65535.0)
    testRDoubleSShort(seq, Double.NaN)
    testRDoubleSShort(seq, 0)
    testRDoubleSShort(seq, 1/65535.0)
    testRDoubleSShort(seq, 1/65534.0)
    testRDoubleSShort(seq, 1/32767.0)
    testRDoubleSShort(seq, 1/21845.0)
    testRDoubleSShort(seq, 1/21844.0)
    testRDoubleSShort(seq, 0.5)
    testRDoubleSShort(seq, 1)
    testRDoubleSShort(seq, 2)
    testRDoubleSShort(seq, Double.MaxValue)
    testRDoubleSShort(seq, Double.PositiveInfinity)
  }

  private def testUShort(seq: DataSeq[RDouble, UShort]) {
    testIndex(seq)

    testRDoubleUShort(seq, Double.NegativeInfinity)
    testRDoubleUShort(seq, -Double.MaxValue)
    testRDoubleUShort(seq, -1)
    testRDoubleUShort(seq, -0.5)
    testRDoubleUShort(seq, Double.NaN)
    testRDoubleUShort(seq, 0)
    testRDoubleUShort(seq, 1/131071.0)
    testRDoubleUShort(seq, 1/131070.0)
    testRDoubleUShort(seq, 1/65535.0)
    testRDoubleUShort(seq, 1/43691.0)
    testRDoubleUShort(seq, 1/43690.0)
    testRDoubleUShort(seq, 0.25)
    testRDoubleUShort(seq, 0.5)
    testRDoubleUShort(seq, 1)
    testRDoubleUShort(seq, 2)
    testRDoubleUShort(seq, Double.MaxValue)
    testRDoubleUShort(seq, Double.PositiveInfinity)
  }
  
  private def testSInt(seq: DataSeq[RDouble, SInt]) {
    testIndex(seq)

    seq.buffer().put(seq.offset, Int.MinValue); assert(seq(0) == -1)

    testRDoubleSInt(seq, Double.NegativeInfinity)
    testRDoubleSInt(seq, -Double.MaxValue)
    testRDoubleSInt(seq, -2)
    testRDoubleSInt(seq, Int.MinValue/2147483647.0)
    testRDoubleSInt(seq, -1)
    testRDoubleSInt(seq, -0.5)
    testRDoubleSInt(seq, -1/858993458.0)
    testRDoubleSInt(seq, -1/858993459.0)
    testRDoubleSInt(seq, -2/2147483647.0)
    testRDoubleSInt(seq, -1/1431655764.0)
    testRDoubleSInt(seq, -1/1431655765.0)
    testRDoubleSInt(seq, Double.NaN)
    testRDoubleSInt(seq, 0)
    testRDoubleSInt(seq, 1/1431655765.0)
    testRDoubleSInt(seq, 1/1431655764.0)
    testRDoubleSInt(seq, 2/2147483647.0)
    testRDoubleSInt(seq, 1/858993459.0)
    testRDoubleSInt(seq, 1/858993458.0)
    testRDoubleSInt(seq, 0.5)
    testRDoubleSInt(seq, 1)
    testRDoubleSInt(seq, 2)
    testRDoubleSInt(seq, Double.MaxValue)
    testRDoubleSInt(seq, Double.PositiveInfinity)
  }

  private def testUInt(seq: DataSeq[RDouble, UInt]) {
    testIndex(seq)

    testRDoubleUInt(seq, Double.NegativeInfinity)
    testRDoubleUInt(seq, -Double.MaxValue)
    testRDoubleUInt(seq, -1)
    testRDoubleUInt(seq, -0.5)
    testRDoubleUInt(seq, Double.NaN)
    testRDoubleUInt(seq, 0)
    testRDoubleUInt(seq, 1/2863311531.0)
    testRDoubleUInt(seq, 1/2863311530.0)
    testRDoubleUInt(seq, 2/4294967295.0)
    testRDoubleUInt(seq, 1/1717986919.0)
    testRDoubleUInt(seq, 1/1717986918.0)
    testRDoubleUInt(seq, 0.25)
    testRDoubleUInt(seq, 0.5)
    testRDoubleUInt(seq, 1)
    testRDoubleUInt(seq, 2)
    testRDoubleUInt(seq, Double.MaxValue)
    testRDoubleUInt(seq, Double.PositiveInfinity)
  }
  
  private def testHFloat(seq: DataSeq[RDouble, HFloat]) {
    testIndex(seq)

    // Inf.
    testRDoubleHFloat(seq, Double.NegativeInfinity)
    testRDoubleHFloat(seq, Double.PositiveInfinity)

    // NaN.
    testRDoubleHFloat(seq, Double.NaN)

    // Zero.
    testRDoubleHFloat(seq, -0.0)
    testRDoubleHFloat(seq, 0)

    // Subnormal.
    testRDoubleHFloat(seq, floatFromBits("00000000 01000000 00000000 00000000").toDouble)
    testRDoubleHFloat(seq, floatFromBits("10000000 01000000 00000000 00000000").toDouble)

    // Out of range values.
    testRDoubleHFloat(seq, -Double.MaxValue)
    testRDoubleHFloat(seq, Double.MaxValue)

    // Min and Max
    testRDoubleHFloat(seq, -65504.0)
    testRDoubleHFloat(seq, 65504.0)

    // Closest to Zero.
    testRDoubleHFloat(seq, -6.103515625E-5)
    testRDoubleHFloat(seq, 6.103515625E-5)
    testRDoubleHFloat(seq, -java.lang.Double.MIN_VALUE)
    testRDoubleHFloat(seq, java.lang.Double.MIN_VALUE)

    // Rounding.
    testRDoubleHFloat(seq, floatFromBits("00111101 11000000 11100000 00000000").toDouble)
    testRDoubleHFloat(seq, floatFromBits("00111101 11000000 11110000 00000000").toDouble)
    testRDoubleHFloat(seq, floatFromBits("10111101 11000000 11100000 00000000").toDouble)
    testRDoubleHFloat(seq, floatFromBits("10111101 11000000 11110000 00000000").toDouble)

    // Normalized.
    testRDoubleHFloat(seq, -1)
    testRDoubleHFloat(seq, -0.5)
    testRDoubleHFloat(seq, 0.5)
    testRDoubleHFloat(seq, 1)

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
