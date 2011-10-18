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

package test.data
package float

import org.scalatest._
import simplex3d.math.floatx._
import simplex3d.data._
import simplex3d.data.float._

import Descriptors._
import FactoryTestUtil._
import TestUtil._
import ApplyUpdateTestUtil._
import SubCopyTestUtil._


/**
 * @author Aleksey Nikiforov (lex)
 */
class RFloatTest extends FunSuite {

  test("Factories") {
    testArrayFromSize(DataArray[RFloat, SByte](_))
    testArrayFromData[RFloat, SByte](DataArray[RFloat, SByte](_))
    testBufferFromSize(DataBuffer[RFloat, SByte](_))
    testBufferFromData(DataBuffer[RFloat, SByte](_))
    testViewFromData(DataView[RFloat, SByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[RFloat, SByte](_))
    testReadViewFromData(ReadDataView[RFloat, SByte](_, _, _))
    testArrayFromCollection[RFloat, SByte]((a: IndexedSeq[Float]) => DataArray[RFloat, SByte](a: _*))
    testBufferFromCollection[RFloat, SByte]((a: IndexedSeq[Float]) => DataBuffer[RFloat, SByte](a: _*))

    testArrayFromSize(DataArray[RFloat, UByte](_))
    testArrayFromData[RFloat, UByte](DataArray[RFloat, UByte](_))
    testBufferFromSize(DataBuffer[RFloat, UByte](_))
    testBufferFromData(DataBuffer[RFloat, UByte](_))
    testViewFromData(DataView[RFloat, UByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[RFloat, UByte](_))
    testReadViewFromData(ReadDataView[RFloat, UByte](_, _, _))
    testArrayFromCollection[RFloat, UByte]((a: IndexedSeq[Float]) => DataArray[RFloat, UByte](a: _*))
    testBufferFromCollection[RFloat, UByte]((a: IndexedSeq[Float]) => DataBuffer[RFloat, UByte](a: _*))

    testArrayFromSize(DataArray[RFloat, SShort](_))
    testArrayFromData[RFloat, SShort](DataArray[RFloat, SShort](_))
    testBufferFromSize(DataBuffer[RFloat, SShort](_))
    testBufferFromData(DataBuffer[RFloat, SShort](_))
    testViewFromData(DataView[RFloat, SShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[RFloat, SShort](_))
    testReadViewFromData(ReadDataView[RFloat, SShort](_, _, _))
    testArrayFromCollection[RFloat, SShort]((a: IndexedSeq[Float]) => DataArray[RFloat, SShort](a: _*))
    testBufferFromCollection[RFloat, SShort]((a: IndexedSeq[Float]) => DataBuffer[RFloat, SShort](a: _*))

    testArrayFromSize(DataArray[RFloat, UShort](_))
    testArrayFromData[RFloat, UShort](DataArray[RFloat, UShort](_))
    testBufferFromSize(DataBuffer[RFloat, UShort](_))
    testBufferFromData(DataBuffer[RFloat, UShort](_))
    testViewFromData(DataView[RFloat, UShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[RFloat, UShort](_))
    testReadViewFromData(ReadDataView[RFloat, UShort](_, _, _))
    testArrayFromCollection[RFloat, UShort]((a: IndexedSeq[Float]) => DataArray[RFloat, UShort](a: _*))
    testBufferFromCollection[RFloat, UShort]((a: IndexedSeq[Float]) => DataBuffer[RFloat, UShort](a: _*))

    testArrayFromSize(DataArray[RFloat, SInt](_))
    testArrayFromData[RFloat, SInt](DataArray[RFloat, SInt](_))
    testBufferFromSize(DataBuffer[RFloat, SInt](_))
    testBufferFromData(DataBuffer[RFloat, SInt](_))
    testViewFromData(DataView[RFloat, SInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[RFloat, SInt](_))
    testReadViewFromData(ReadDataView[RFloat, SInt](_, _, _))
    testArrayFromCollection[RFloat, SInt]((a: IndexedSeq[Float]) => DataArray[RFloat, SInt](a: _*))
    testBufferFromCollection[RFloat, SInt]((a: IndexedSeq[Float]) => DataBuffer[RFloat, SInt](a: _*))

    testArrayFromSize(DataArray[RFloat, UInt](_))
    testArrayFromData[RFloat, UInt](DataArray[RFloat, UInt](_))
    testBufferFromSize(DataBuffer[RFloat, UInt](_))
    testBufferFromData(DataBuffer[RFloat, UInt](_))
    testViewFromData(DataView[RFloat, UInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[RFloat, UInt](_))
    testReadViewFromData(ReadDataView[RFloat, UInt](_, _, _))
    testArrayFromCollection[RFloat, UInt]((a: IndexedSeq[Float]) => DataArray[RFloat, UInt](a: _*))
    testBufferFromCollection[RFloat, UInt]((a: IndexedSeq[Float]) => DataBuffer[RFloat, UInt](a: _*))
    
    testArrayFromSize(DataArray[RFloat, HFloat](_))
    testArrayFromData[RFloat, HFloat](DataArray[RFloat, HFloat](_))
    testBufferFromSize(DataBuffer[RFloat, HFloat](_))
    testBufferFromData(DataBuffer[RFloat, HFloat](_))
    testViewFromData(DataView[RFloat, HFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[RFloat, HFloat](_))
    testReadViewFromData(ReadDataView[RFloat, HFloat](_, _, _))
    testArrayFromCollection[RFloat, HFloat]((a: IndexedSeq[Float]) => DataArray[RFloat, HFloat](a: _*))
    testBufferFromCollection[RFloat, HFloat]((a: IndexedSeq[Float]) => DataBuffer[RFloat, HFloat](a: _*))
    
    testArrayFromSize(DataArray[RFloat, RFloat](_))
    testArrayFromData[RFloat, RFloat](DataArray[RFloat, RFloat](_))
    testBufferFromSize(DataBuffer[RFloat, RFloat](_))
    testBufferFromData(DataBuffer[RFloat, RFloat](_))
    testViewFromData(DataView[RFloat, RFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[RFloat, RFloat](_))
    testReadViewFromData(ReadDataView[RFloat, RFloat](_, _, _))
    testArrayFromCollection[RFloat, RFloat]((a: IndexedSeq[Float]) => DataArray[RFloat, RFloat](a: _*))
    testBufferFromCollection[RFloat, RFloat]((a: IndexedSeq[Float]) => DataBuffer[RFloat, RFloat](a: _*))
  }


  private val size = 10
  
  test("Apply/Update") {
    testSByte(DataArray[RFloat, SByte](size))
    testSByte(DataBuffer[RFloat, SByte](size))
    testSByte(DataView[RFloat, SByte](genBuffer(size, Descriptors.RFloatSByte)._1, 0, 2))
    testSByte(DataView[RFloat, SByte](genBuffer(size, Descriptors.RFloatSByte)._1, 1, 2))
    
    testUByte(DataArray[RFloat, UByte](size))
    testUByte(DataBuffer[RFloat, UByte](size))
    testUByte(DataView[RFloat, UByte](genBuffer(size, Descriptors.RFloatUByte)._1, 0, 2))
    testUByte(DataView[RFloat, UByte](genBuffer(size, Descriptors.RFloatUByte)._1, 1, 2))
    
    testSShort(DataArray[RFloat, SShort](size))
    testSShort(DataBuffer[RFloat, SShort](size))
    testSShort(DataView[RFloat, SShort](genBuffer(size, Descriptors.RFloatSShort)._1, 0, 2))
    testSShort(DataView[RFloat, SShort](genBuffer(size, Descriptors.RFloatSShort)._1, 1, 2))
    
    testUShort(DataArray[RFloat, UShort](size))
    testUShort(DataBuffer[RFloat, UShort](size))
    testUShort(DataView[RFloat, UShort](genBuffer(size, Descriptors.RFloatUShort)._1, 0, 2))
    testUShort(DataView[RFloat, UShort](genBuffer(size, Descriptors.RFloatUShort)._1, 1, 2))
    
    testSInt(DataArray[RFloat, SInt](size))
    testSInt(DataBuffer[RFloat, SInt](size))
    testSInt(DataView[RFloat, SInt](genBuffer(size, Descriptors.RFloatSInt)._1, 0, 2))
    testSInt(DataView[RFloat, SInt](genBuffer(size, Descriptors.RFloatSInt)._1, 1, 2))
    
    testUInt(DataArray[RFloat, UInt](size))
    testUInt(DataBuffer[RFloat, UInt](size))
    testUInt(DataView[RFloat, UInt](genBuffer(size, Descriptors.RFloatUInt)._1, 0, 2))
    testUInt(DataView[RFloat, UInt](genBuffer(size, Descriptors.RFloatUInt)._1, 1, 2))
    
    testHFloat(DataArray[RFloat, HFloat](size))
    testHFloat(DataBuffer[RFloat, HFloat](size))
    testHFloat(DataView[RFloat, HFloat](genBuffer(size, Descriptors.RFloatHFloat)._1, 0, 2))
    testHFloat(DataView[RFloat, HFloat](genBuffer(size, Descriptors.RFloatHFloat)._1, 1, 2))
    
    testRFloat(DataArray[RFloat, RFloat](size))
    testRFloat(DataBuffer[RFloat, RFloat](size))
    testRFloat(DataView[RFloat, RFloat](genBuffer(size, Descriptors.RFloatRFloat)._1, 0, 2))
    testRFloat(DataView[RFloat, RFloat](genBuffer(size, Descriptors.RFloatRFloat)._1, 1, 2))
  }
  
  test("Sub Copy") {
    testSubCopy(DataSeq[RFloat, UByte])
    testSubCopy(DataSeq[RFloat, SByte])
    testSubCopy(DataSeq[RFloat, UShort])
    testSubCopy(DataSeq[RFloat, SShort])
    testSubCopy(DataSeq[RFloat, UInt])
    testSubCopy(DataSeq[RFloat, SInt])
    testSubCopy(DataSeq[RFloat, HFloat])
    testSubCopy(DataSeq[RFloat, RFloat])
  }

  private def testFloatSByte(seq: DataSeq[RFloat, SByte], testValue: Float) {
    val converted = conversion.Float.toSByte(testValue)
    val convertedDack = conversion.Float.fromSByte(converted)
    testApplyUpdate(seq, testValue, convertedDack, converted)
  }
  private def testFloatUByte(seq: DataSeq[RFloat, UByte], testValue: Float) {
    val converted = conversion.Float.toUByte(testValue)
    val convertedDack = conversion.Float.fromUByte(converted)
    testApplyUpdate(seq, testValue, convertedDack, converted)
  }
  private def testFloatSShort(seq: DataSeq[RFloat, SShort], testValue: Float) {
    val converted = conversion.Float.toSShort(testValue)
    val convertedDack = conversion.Float.fromSShort(converted)
    testApplyUpdate(seq, testValue, convertedDack, converted)
  }
  private def testFloatUShort(seq: DataSeq[RFloat, UShort], testValue: Float) {
    val converted = conversion.Float.toUShort(testValue)
    val convertedDack = conversion.Float.fromUShort(converted)
    testApplyUpdate(seq, testValue, convertedDack, converted)
  }
  private def testFloatSInt(seq: DataSeq[RFloat, SInt], testValue: Float) {
    val converted = conversion.Float.toSInt(testValue)
    val convertedDack = conversion.Float.fromSInt(converted)
    testApplyUpdate(seq, testValue, convertedDack, converted)
  }
  private def testFloatUInt(seq: DataSeq[RFloat, UInt], testValue: Float) {
    val converted = conversion.Float.toUInt(testValue)
    val convertedDack = conversion.Float.fromUInt(converted)
    testApplyUpdate(seq, testValue, convertedDack, converted)
  }
  private def testFloatHFloat(seq: DataSeq[RFloat, HFloat], testValue: Float) {
    val converted = conversion.Float.toHFloat(testValue)
    val convertedDack = conversion.Float.fromHFloat(converted)
    testApplyUpdate(seq, testValue, convertedDack, converted)
  }

  private def testSByte(seq: DataSeq[RFloat, SByte]) {
    testIndex(seq)

    seq.buffer().put(seq.offset, -128); assert(seq(0) == -1)

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

  private def testUByte(seq: DataSeq[RFloat, UByte]) {
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

  private def testSShort(seq: DataSeq[RFloat, SShort]) {
    testIndex(seq)

    seq.buffer().put(seq.offset, -32768); assert(seq(0) == -1)

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

  private def testUShort(seq: DataSeq[RFloat, UShort]) {
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
  
  private def testSInt(seq: DataSeq[RFloat, SInt]) {
    testIndex(seq)

    seq.buffer().put(seq.offset, Int.MinValue); assert(seq(0) == -1)

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

  private def testUInt(seq: DataSeq[RFloat, UInt]) {
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
  
  private def testHFloat(seq: DataSeq[RFloat, HFloat]) {
    testIndex(seq)

    // Inf.
    testFloatHFloat(seq, Float.NegativeInfinity)
    testFloatHFloat(seq, Float.PositiveInfinity)

    // NaN.
    testFloatHFloat(seq, Float.NaN)

    // More NaN.
    testFloatHFloat(seq, floatFromBits("01111111 11100000 00000000 00000000"))
    testFloatHFloat(seq, floatFromBits("11111111 11100000 00000000 00000000"))

    // Zero.
    testFloatHFloat(seq, -0f)
    testFloatHFloat(seq, 0)

    // Subnormal.
    testFloatHFloat(seq, floatFromBits("00000000 01000000 00000000 00000000"))
    testFloatHFloat(seq, floatFromBits("10000000 01000000 00000000 00000000"))

    // Out of range values.
    testFloatHFloat(seq, -Float.MaxValue)
    testFloatHFloat(seq, Float.MaxValue)

    // Min and Max
    testFloatHFloat(seq, -65504.0f)
    testFloatHFloat(seq, 65504.0f)

    // Closest to Zero.
    testFloatHFloat(seq, -6.103515625E-5f)
    testFloatHFloat(seq, 6.103515625E-5f)
    testFloatHFloat(seq, -java.lang.Float.MIN_VALUE)
    testFloatHFloat(seq, java.lang.Float.MIN_VALUE)

    // Rounding.
    testFloatHFloat(seq, floatFromBits("00111101 11000000 11100000 00000000"))
    testFloatHFloat(seq, floatFromBits("00111101 11000000 11110000 00000000"))
    testFloatHFloat(seq, floatFromBits("10111101 11000000 11100000 00000000"))
    testFloatHFloat(seq, floatFromBits("10111101 11000000 11110000 00000000"))

    // Normalized.
    testFloatHFloat(seq, -1)
    testFloatHFloat(seq, -0.5f)
    testFloatHFloat(seq, 0.5f)
    testFloatHFloat(seq, 1)

    // Round trip conversion excluding Subnormal and Zero.
    val buff = seq.buffer
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
  
  private def testRFloat(seq: DataSeq[RFloat, RFloat]) {
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
