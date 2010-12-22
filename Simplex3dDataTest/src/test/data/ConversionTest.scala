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

import org.scalatest._
import simplex3d.buffer._

import Descriptors._
import FactoryTestUtil._
import TestUtil._
import ApplyUpdateTestUtil._


/**
 * @author Aleksey Nikiforov (lex)
 */
class ConversionTest extends FunSuite {

  private def testFloatSByte(testValue: Float, convertedBack: Float, converted: Byte) {
    val c = conversion.Float.toSByte(testValue)
    assert(c == converted)
    assert(conversion.Float.fromSByte(c) == convertedBack)
  }
  private def testFloatUByte(testValue: Float, convertedBack: Float, converted: Byte) {
    val c = conversion.Float.toUByte(testValue)
    assert(c == converted)
    assert(conversion.Float.fromUByte(c) == convertedBack)
  }
  private def testFloatSShort(testValue: Float, convertedBack: Float, converted: Short) {
    val c = conversion.Float.toSShort(testValue)
    assert(c == converted)
    assert(conversion.Float.fromSShort(c) == convertedBack)
  }
  private def testFloatUShort(testValue: Float, convertedBack: Float, converted: Char) {
    val c = conversion.Float.toUShort(testValue)
    assert(c == converted)
    assert(conversion.Float.fromUShort(c) == convertedBack)
  }
  private def testFloatSInt(testValue: Float, convertedBack: Float, converted: Int) {
    val c = conversion.Float.toSInt(testValue)
    assert(c == converted)
    assert(conversion.Float.fromSInt(c) == convertedBack)
  }
  private def testFloatUInt(testValue: Float, convertedBack: Float, converted: Int) {
    val c = conversion.Float.toUInt(testValue)
    assert(c == converted)
    assert(conversion.Float.fromUInt(c) == convertedBack)
  }
  private def testFloatHFloat(testValue: Float, convertedBack: Float, converted: Short) {
    val c = conversion.Float.toHFloat(testValue)
    assert(c == converted)

    import simplex3d.math.floatm.FloatMath._
    if (isnan(testValue)) assert(isnan(convertedBack))
    else assert(conversion.Float.fromHFloat(c) == convertedBack)
  }
  
  test("Float-SByte") {
    assert(conversion.Float.fromSByte(-128) == -1)

    testFloatSByte(Float.NegativeInfinity, -1, -127)
    testFloatSByte(-Float.MaxValue, -1, -127)
    testFloatSByte(-2, -1, -127)
    testFloatSByte(-128/127f, -1, -127)
    testFloatSByte(-1, -1, -127)
    testFloatSByte(-0.5f, -64/127f, -64)
    testFloatSByte(-1/84f, -2/127f, -2)
    testFloatSByte(-1/85f, -1/127f, -1)
    testFloatSByte(-1/127f, -1/127f, -1)
    testFloatSByte(-1/254f, -1/127f, -1)
    testFloatSByte(-1/255f, 0, 0)
    testFloatSByte(Float.NaN, 0, 0)
    testFloatSByte(0, 0, 0)
    testFloatSByte(1/255f, 0, 0)
    testFloatSByte(1/254f, 1/127f, 1)
    testFloatSByte(1/127f, 1/127f, 1)
    testFloatSByte(1/85f, 1/127f, 1)
    testFloatSByte(1/84f, 2/127f, 2)
    testFloatSByte(0.5f, 64/127f, 64)
    testFloatSByte(1, 1, 127)
    testFloatSByte(2, 1, 127)
    testFloatSByte(Float.MaxValue, 1, 127)
    testFloatSByte(Float.PositiveInfinity, 1, 127)
  }

  test("Float-UByte") {
    testFloatUByte(Float.NegativeInfinity, 0, 0)
    testFloatUByte(-Float.MaxValue, 0, 0)
    testFloatUByte(-1, 0, 0)
    testFloatUByte(-0.5f, 0, 0)
    testFloatUByte(Float.NaN, 0, 0)
    testFloatUByte(0, 0, 0)
    testFloatUByte(1/511f, 0, 0)
    testFloatUByte(1/510f, 1/255f, 1)
    testFloatUByte(1/255f, 1/255f, 1)
    testFloatUByte(1/171f, 1/255f, 1)
    testFloatUByte(1/170f, 2/255f, 2)
    testFloatUByte(0.25f, 64/255f, 64)
    testFloatUByte(0.5f, 128/255f, -128)
    testFloatUByte(1, 1, -1)
    testFloatUByte(2, 1, -1)
    testFloatUByte(Float.MaxValue, 1, -1)
    testFloatUByte(Float.PositiveInfinity, 1, -1)
  }

  test("Float-SShort") {
    assert(conversion.Float.fromSShort(-32768) == -1)

    testFloatSShort(Float.NegativeInfinity, -1, -32767)
    testFloatSShort(-Float.MaxValue, -1, -32767)
    testFloatSShort(-2, -1, -32767)
    testFloatSShort(-32768/32767f, -1, -32767)
    testFloatSShort(-1, -1, -32767)
    testFloatSShort(-0.5f, -16384/32767f, -16384)
    testFloatSShort(-1/21844f, -2/32767f, -2)
    testFloatSShort(-1/21845f, -1/32767f, -1)
    testFloatSShort(-1/32767f, -1/32767f, -1)
    testFloatSShort(-1/65534f, -1/32767f, -1)
    testFloatSShort(-1/65535f, 0, 0)
    testFloatSShort(Float.NaN, 0, 0)
    testFloatSShort(0, 0, 0)
    testFloatSShort(1/65535f, 0, 0)
    testFloatSShort(1/65534f, 1/32767f, 1)
    testFloatSShort(1/32767f, 1/32767f, 1)
    testFloatSShort(1/21845f, 1/32767f, 1)
    testFloatSShort(1/21844f, 2/32767f, 2)
    testFloatSShort(0.5f, 16384/32767f, 16384)
    testFloatSShort(1, 1, 32767)
    testFloatSShort(2, 1, 32767)
    testFloatSShort(Float.MaxValue, 1, 32767)
    testFloatSShort(Float.PositiveInfinity, 1, 32767)
  }

  test("Float-UShort") {
    testFloatUShort(Float.NegativeInfinity, 0, 0)
    testFloatUShort(-Float.MaxValue, 0, 0)
    testFloatUShort(-1, 0, 0)
    testFloatUShort(-0.5f, 0, 0)
    testFloatUShort(Float.NaN, 0, 0)
    testFloatUShort(0, 0, 0)
    testFloatUShort(1/131071f, 0, 0)
    testFloatUShort(1/131070f, 1/65535f, 1)
    testFloatUShort(1/65535f, 1/65535f, 1)
    testFloatUShort(1/43691f, 1/65535f, 1)
    testFloatUShort(1/43690f, 2/65535f, 2)
    testFloatUShort(0.25f, 16384/65535f, 16384)
    testFloatUShort(0.5f, 32768/65535f, 32768)
    testFloatUShort(1, 1, 65535)
    testFloatUShort(2, 1, 65535)
    testFloatUShort(Float.MaxValue, 1, 65535)
    testFloatUShort(Float.PositiveInfinity, 1, 65535)
  }

  test("Float-SInt") {
    assert(conversion.Float.fromSInt(Int.MinValue) == -1)

    testFloatSInt(Float.NegativeInfinity, -1, -2147483647)
    testFloatSInt(-Float.MaxValue, -1, -2147483647)
    testFloatSInt(-2, -1, -2147483647)
    testFloatSInt(Int.MinValue/2147483647f, -1, -2147483647)
    testFloatSInt(-1, -1, -2147483647)
    testFloatSInt(-0.5f, -1073741824/2147483647f, -1073741824)
    testFloatSInt(-1/858993440f, -3/2147483647f, -3) //-1/858993458 without float rounding
    testFloatSInt(-1/858993441f, -2/2147483647f, -2) //-1/858993459 without float rounding
    testFloatSInt(-2/2147483647f, -2/2147483647f, -2)
    testFloatSInt(-1/1431655744f, -2/2147483647f, -2) //-1/1431655764 without float rounding
    testFloatSInt(-1/1431655745f, -1/2147483647f, -1) //-1/1431655765 without float rounding
    testFloatSInt(Float.NaN, 0, 0)
    testFloatSInt(0, 0, 0)
    testFloatSInt(1/1431655745f, 1/2147483647f, 1) //1/1431655765 without float rounding
    testFloatSInt(1/1431655744f, 2/2147483647f, 2) //1/1431655764 without float rounding
    testFloatSInt(2/2147483647f, 2/2147483647f, 2)
    testFloatSInt(1/858993441f, 2/2147483647f, 2) //1/858993459 without float rounding
    testFloatSInt(1/858993440f, 3/2147483647f, 3) //1/858993458 without float rounding
    testFloatSInt(0.5f, 1073741824/2147483647f, 1073741824)
    testFloatSInt(1, 1, 2147483647)
    testFloatSInt(2, 1, 2147483647)
    testFloatSInt(Float.MaxValue, 1, 2147483647)
    testFloatSInt(Float.PositiveInfinity, 1, 2147483647)
  }

  test("Float-UInt") {
    testFloatUInt(Float.NegativeInfinity, 0, 0)
    testFloatUInt(-Float.MaxValue, 0, 0)
    testFloatUInt(-1, 0, 0)
    testFloatUInt(-0.5f, 0, 0)
    testFloatUInt(Float.NaN, 0, 0)
    testFloatUInt(0, 0, 0)
    testFloatUInt(1/2863311489f, 1/4294967295f, 1) //1/2863311531 without float rounding
    testFloatUInt(1/2863311488f, 2/4294967295f, 2) //1/2863311530 without float rounding
    testFloatUInt(2/4294967295f, 2/4294967295f, 2)
    testFloatUInt(1/1717986881f, 2/4294967295f, 2) //1/1717986919 without float rounding
    testFloatUInt(1/1717986880f, 3/4294967295f, 3) //1/1717986918 without float rounding
    testFloatUInt(0.25f, 1073741824/4294967295f, 1073741824)
    testFloatUInt(0.5f, 2147483648L/4294967295f, Int.MinValue)
    testFloatUInt(1, 1, -1)
    testFloatUInt(2, 1, -1)
    testFloatUInt(Float.MaxValue, 1, -1)
    testFloatUInt(Float.PositiveInfinity, 1, -1)
  }
  
  test("Float-HFloat") {
    // Inf.
    testFloatHFloat(Float.NegativeInfinity, Float.NegativeInfinity, 0xFC00.toShort)
    testFloatHFloat(Float.PositiveInfinity, Float.PositiveInfinity, 0x7C00.toShort)

    // NaN.
    testFloatHFloat(Float.NaN, Float.NaN, 0x7E00)
    
    // More NaN.
    testFloatHFloat(
      floatFromBits("01111111 11100000 00000000 00000000"),
      floatFromBits("01111111 11100000 00000000 00000000"),
      intFromBits("01111111 00000000").toShort
    )
    testFloatHFloat(
      floatFromBits("01111111 10100000 00000000 00000000"),
      floatFromBits("01111111 10100000 00000000 00000000"),
      intFromBits("01111101 00000000").toShort
    )
    testFloatHFloat(
      floatFromBits("11111111 11100000 00000000 00000000"),
      floatFromBits("11111111 11100000 00000000 00000000"),
      intFromBits("11111111 00000000").toShort
    )
    testFloatHFloat(
      floatFromBits("11111111 10100000 00000000 00000000"),
      floatFromBits("11111111 10100000 00000000 00000000"),
      intFromBits("11111101 00000000").toShort
    )

    // Zero.
    testFloatHFloat(-0f, -0f, intFromBits("10000000 00000000").toShort)
    testFloatHFloat(0, 0, 0)

    // Subnormal.
    testFloatHFloat(
      floatFromBits("00000000 01000000 00000000 00000000"),
      floatFromBits("00000000 00000000 00000000 00000000"),
      intFromBits("00000000 00000000").toShort
    )
    testFloatHFloat(
      floatFromBits("10000000 01000000 00000000 00000000"),
      floatFromBits("10000000 00000000 00000000 00000000"),
      intFromBits("10000000 00000000").toShort
    )

    // Out of range values.
    testFloatHFloat(-Float.MaxValue, Float.NegativeInfinity, 0xFC00.toShort)
    testFloatHFloat(Float.MaxValue, Float.PositiveInfinity, 0x7C00.toShort)

    // Min and Max
    testFloatHFloat(-65504.0f, -65504.0f, 0xFBFF.toShort)
    testFloatHFloat(65504.0f, 65504.0f, 0x7BFF.toShort)

    // Closest to Zero.
    testFloatHFloat(-6.103515625E-5f, -6.103515625E-5f, 0x8400.toShort)
    testFloatHFloat(6.103515625E-5f, 6.103515625E-5f, 0x0400.toShort)
    testFloatHFloat(-java.lang.Float.MIN_VALUE, -0f, intFromBits("10000000 00000000").toShort)
    testFloatHFloat(java.lang.Float.MIN_VALUE, 0, 0)

    // Rounding.
    testFloatHFloat(
      floatFromBits("00111101 11000000 11100000 00000000"), //0.094177246
      floatFromBits("00111101 11000000 11100000 00000000"), //0.094177246
      intFromBits("00101110 00000111").toShort
    )
    testFloatHFloat(
      floatFromBits("00111101 11000000 11110000 00000000"), //0.09420776
      floatFromBits("00111101 11000001 00000000 00000000"), //0.09423828
      intFromBits("00101110 00001000").toShort
    )
    testFloatHFloat(
      floatFromBits("10111101 11000000 11100000 00000000"), //-0.094177246
      floatFromBits("10111101 11000000 11100000 00000000"), //-0.094177246
      intFromBits("10101110 00000111").toShort
    )
    testFloatHFloat(
      floatFromBits("10111101 11000000 11110000 00000000"), //-0.09420776
      floatFromBits("10111101 11000001 00000000 00000000"), //-0.09423828
      intFromBits("10101110 00001000").toShort
    )
    
    // Normalized.
    testFloatHFloat(-1, -1, intFromBits("10111100 00000000").toShort)
    testFloatHFloat(-0.5f, -0.5f, intFromBits("10111000 00000000").toShort)
    testFloatHFloat(0.5f, 0.5f, intFromBits("00111000 00000000").toShort)
    testFloatHFloat(1, 1, intFromBits("00111100 00000000").toShort)

    // Round trip conversion excluding Subnormal and Zero.
    var i = 0; while (i < 65536) {
      if ((i & 0x7C00) != 0) {
        val h = i.toShort
        val f = conversion.Float.fromHFloat(h)
        assert(h == conversion.Float.toHFloat(f))
      }

      i += 1
    }
  }
  
  
  private def testDoubleSByte(testValue: Double, convertedBack: Double, converted: Byte) {
    val c = conversion.Double.toSByte(testValue)
    assert(c == converted)
    assert(conversion.Double.fromSByte(c) == convertedBack)
  }
  private def testDoubleUByte(testValue: Double, convertedBack: Double, converted: Byte) {
    val c = conversion.Double.toUByte(testValue)
    assert(c == converted)
    assert(conversion.Double.fromUByte(c) == convertedBack)
  }
  private def testDoubleSShort(testValue: Double, convertedBack: Double, converted: Short) {
    val c = conversion.Double.toSShort(testValue)
    assert(c == converted)
    assert(conversion.Double.fromSShort(c) == convertedBack)
  }
  private def testDoubleUShort(testValue: Double, convertedBack: Double, converted: Char) {
    val c = conversion.Double.toUShort(testValue)
    assert(c == converted)
    assert(conversion.Double.fromUShort(c) == convertedBack)
  }
  private def testDoubleSInt(testValue: Double, convertedBack: Double, converted: Int) {
    val c = conversion.Double.toSInt(testValue)
    assert(c == converted)
    assert(conversion.Double.fromSInt(c) == convertedBack)
  }
  private def testDoubleUInt(testValue: Double, convertedBack: Double, converted: Int) {
    val c = conversion.Double.toUInt(testValue)
    assert(c == converted)
    assert(conversion.Double.fromUInt(c) == convertedBack)
  }
  private def testDoubleHFloat(testValue: Double, convertedBack: Double, converted: Short) {
    val c = conversion.Double.toHFloat(testValue)
    assert(c == converted)

    import simplex3d.math.doublem.DoubleMath._
    if (isnan(testValue)) assert(isnan(convertedBack))
    else assert(conversion.Double.fromHFloat(c) == convertedBack)
  }
  
  test("Double-SByte") {
    val div127 = 1/127.0
    
    assert(conversion.Double.fromSByte(-128) == -1)

    testDoubleSByte(Double.NegativeInfinity, -1, -127)
    testDoubleSByte(-Double.MaxValue, -1, -127)
    testDoubleSByte(-2, -1, -127)
    testDoubleSByte(-128/127.0, -1, -127)
    testDoubleSByte(-1, -1, -127)
    testDoubleSByte(-0.5, -64*div127, -64)
    testDoubleSByte(-1/84.0, -2*div127, -2)
    testDoubleSByte(-1/85.0, -1*div127, -1)
    testDoubleSByte(-1/127.0, -1*div127, -1)
    testDoubleSByte(-1/254.0, -1*div127, -1)
    testDoubleSByte(-1/255.0, 0, 0)
    testDoubleSByte(Double.NaN, 0, 0)
    testDoubleSByte(0, 0, 0)
    testDoubleSByte(1/255.0, 0, 0)
    testDoubleSByte(1/254.0, 1*div127, 1)
    testDoubleSByte(1/127.0, 1*div127, 1)
    testDoubleSByte(1/85.0, 1*div127, 1)
    testDoubleSByte(1/84.0, 2*div127, 2)
    testDoubleSByte(0.5, 64*div127, 64)
    testDoubleSByte(1, 1, 127)
    testDoubleSByte(2, 1, 127)
    testDoubleSByte(Double.MaxValue, 1, 127)
    testDoubleSByte(Double.PositiveInfinity, 1, 127)
  }

  test("Double-UByte") {
    val div255 = 1/255.0
    
    testDoubleUByte(Double.NegativeInfinity, 0, 0)
    testDoubleUByte(-Double.MaxValue, 0, 0)
    testDoubleUByte(-1, 0, 0)
    testDoubleUByte(-0.5, 0, 0)
    testDoubleUByte(Double.NaN, 0, 0)
    testDoubleUByte(0, 0, 0)
    testDoubleUByte(1/511.0, 0, 0)
    testDoubleUByte(1/510.0, 1*div255, 1)
    testDoubleUByte(1/255.0, 1*div255, 1)
    testDoubleUByte(1/171.0, 1*div255, 1)
    testDoubleUByte(1/170.0, 2*div255, 2)
    testDoubleUByte(0.25, 64*div255, 64)
    testDoubleUByte(0.5, 128*div255, -128)
    testDoubleUByte(1, 1, -1)
    testDoubleUByte(2, 1, -1)
    testDoubleUByte(Double.MaxValue, 1, -1)
    testDoubleUByte(Double.PositiveInfinity, 1, -1)
  }

  test("Double-SShort") {
    val div32767 = 1/32767.0

    assert(conversion.Double.fromSShort(-32768) == -1)

    testDoubleSShort(Double.NegativeInfinity, -1, -32767)
    testDoubleSShort(-Double.MaxValue, -1, -32767)
    testDoubleSShort(-2, -1, -32767)
    testDoubleSShort(-32768/32767.0, -1, -32767)
    testDoubleSShort(-1, -1, -32767)
    testDoubleSShort(-0.5, -16384*div32767, -16384)
    testDoubleSShort(-1/21844.0, -2*div32767, -2)
    testDoubleSShort(-1/21845.0, -1*div32767, -1)
    testDoubleSShort(-1/32767.0, -1*div32767, -1)
    testDoubleSShort(-1/65534.0, -1*div32767, -1)
    testDoubleSShort(-1/65535.0, 0, 0)
    testDoubleSShort(Double.NaN, 0, 0)
    testDoubleSShort(0, 0, 0)
    testDoubleSShort(1/65535.0, 0, 0)
    testDoubleSShort(1/65534.0, 1*div32767, 1)
    testDoubleSShort(1/32767.0, 1*div32767, 1)
    testDoubleSShort(1/21845.0, 1*div32767, 1)
    testDoubleSShort(1/21844.0, 2*div32767, 2)
    testDoubleSShort(0.5, 16384*div32767, 16384)
    testDoubleSShort(1, 1, 32767)
    testDoubleSShort(2, 1, 32767)
    testDoubleSShort(Double.MaxValue, 1, 32767)
    testDoubleSShort(Double.PositiveInfinity, 1, 32767)
  }

  test("Double-UShort") {
    val div65535 = 1/65535.0

    testDoubleUShort(Double.NegativeInfinity, 0, 0)
    testDoubleUShort(-Double.MaxValue, 0, 0)
    testDoubleUShort(-1, 0, 0)
    testDoubleUShort(-0.5, 0, 0)
    testDoubleUShort(Double.NaN, 0, 0)
    testDoubleUShort(0, 0, 0)
    testDoubleUShort(1/131071.0, 0, 0)
    testDoubleUShort(1/131070.0, 1*div65535, 1)
    testDoubleUShort(1/65535.0, 1*div65535, 1)
    testDoubleUShort(1/43691.0, 1*div65535, 1)
    testDoubleUShort(1/43690.0, 2*div65535, 2)
    testDoubleUShort(0.25, 16384*div65535, 16384)
    testDoubleUShort(0.5, 32768*div65535, 32768)
    testDoubleUShort(1, 1, 65535)
    testDoubleUShort(2, 1, 65535)
    testDoubleUShort(Double.MaxValue, 1, 65535)
    testDoubleUShort(Double.PositiveInfinity, 1, 65535)
  }

  test("Double-SInt") {
    val div2147483647 = 1/2147483647.0

    assert(conversion.Double.fromSInt(Int.MinValue) == -1)

    testDoubleSInt(Double.NegativeInfinity, -1, -2147483647)
    testDoubleSInt(-Double.MaxValue, -1, -2147483647)
    testDoubleSInt(-2, -1, -2147483647)
    testDoubleSInt(Int.MinValue/2147483647.0, -1, -2147483647)
    testDoubleSInt(-1, -1, -2147483647)
    testDoubleSInt(-0.5, -1073741824*div2147483647, -1073741824)
    testDoubleSInt(-1/858993458.0, -3*div2147483647, -3)
    testDoubleSInt(-1/858993459.0, -2*div2147483647, -2)
    testDoubleSInt(-2/2147483647.0, -2*div2147483647, -2)
    testDoubleSInt(-1/1431655764.0, -2*div2147483647, -2)
    testDoubleSInt(-1/1431655765.0, -1*div2147483647, -1)
    testDoubleSInt(Double.NaN, 0, 0)
    testDoubleSInt(0, 0, 0)
    testDoubleSInt(1/1431655765.0, 1*div2147483647, 1)
    testDoubleSInt(1/1431655764.0, 2*div2147483647, 2)
    testDoubleSInt(2/2147483647.0, 2*div2147483647, 2)
    testDoubleSInt(1/858993459.0, 2*div2147483647, 2)
    testDoubleSInt(1/858993458.0, 3*div2147483647, 3)
    testDoubleSInt(0.5, 1073741824*div2147483647, 1073741824)
    testDoubleSInt(1, 1, 2147483647)
    testDoubleSInt(2, 1, 2147483647)
    testDoubleSInt(Double.MaxValue, 1, 2147483647)
    testDoubleSInt(Double.PositiveInfinity, 1, 2147483647)
  }

  test("Double-UInt") {
    val div4294967295 = 1/4294967295.0

    testDoubleUInt(Double.NegativeInfinity, 0, 0)
    testDoubleUInt(-Double.MaxValue, 0, 0)
    testDoubleUInt(-1, 0, 0)
    testDoubleUInt(-0.5, 0, 0)
    testDoubleUInt(Double.NaN, 0, 0)
    testDoubleUInt(0, 0, 0)
    testDoubleUInt(1/2863311531.0, 1*div4294967295, 1)
    testDoubleUInt(1/2863311530.0, 2*div4294967295, 2)
    testDoubleUInt(2/4294967295.0, 2*div4294967295, 2)
    testDoubleUInt(1/1717986919.0, 2*div4294967295, 2)
    testDoubleUInt(1/1717986918.0, 3*div4294967295, 3)
    testDoubleUInt(0.25, 1073741824*div4294967295, 1073741824)
    testDoubleUInt(0.5, 2147483648L*div4294967295, Int.MinValue)
    testDoubleUInt(1, 1, -1)
    testDoubleUInt(2, 1, -1)
    testDoubleUInt(Double.MaxValue, 1, -1)
    testDoubleUInt(Double.PositiveInfinity, 1, -1)
  }
  
  test("Double-HFloat") {
    // Inf.
    testDoubleHFloat(Double.NegativeInfinity, Double.NegativeInfinity, 0xFC00.toShort)
    testDoubleHFloat(Double.PositiveInfinity, Double.PositiveInfinity, 0x7C00.toShort)

    // NaN.
    testDoubleHFloat(Double.NaN, Double.NaN, 0x7E00)

    // Zero.
    testDoubleHFloat(-0.0, -0.0, intFromBits("10000000 00000000").toShort)
    testDoubleHFloat(0, 0, 0)

    // Subnormal.
    testDoubleHFloat(
      floatFromBits("00000000 01000000 00000000 00000000").toDouble,
      floatFromBits("00000000 00000000 00000000 00000000").toDouble,
      intFromBits("00000000 00000000").toShort
    )
    testDoubleHFloat(
      floatFromBits("10000000 01000000 00000000 00000000").toDouble,
      floatFromBits("10000000 00000000 00000000 00000000").toDouble,
      intFromBits("10000000 00000000").toShort
    )

    // Out of range values.
    testDoubleHFloat(-Double.MaxValue, Double.NegativeInfinity, 0xFC00.toShort)
    testDoubleHFloat(Double.MaxValue, Double.PositiveInfinity, 0x7C00.toShort)

    // Min and Max
    testDoubleHFloat(-65504.0, -65504.0, 0xFBFF.toShort)
    testDoubleHFloat(65504.0, 65504.0, 0x7BFF.toShort)

    // Closest to Zero.
    testDoubleHFloat(-6.103515625E-5, -6.103515625E-5, 0x8400.toShort)
    testDoubleHFloat(6.103515625E-5, 6.103515625E-5, 0x0400.toShort)
    testDoubleHFloat(-java.lang.Double.MIN_VALUE, -0.0, intFromBits("10000000 00000000").toShort)
    testDoubleHFloat(java.lang.Double.MIN_VALUE, 0, 0)

    // Rounding.
    testDoubleHFloat(
      floatFromBits("00111101 11000000 11100000 00000000").toDouble, //0.094177246
      floatFromBits("00111101 11000000 11100000 00000000").toDouble, //0.094177246
      intFromBits("00101110 00000111").toShort
    )
    testDoubleHFloat(
      floatFromBits("00111101 11000000 11110000 00000000").toDouble, //0.09420776
      floatFromBits("00111101 11000001 00000000 00000000").toDouble, //0.09423828
      intFromBits("00101110 00001000").toShort
    )
    testDoubleHFloat(
      floatFromBits("10111101 11000000 11100000 00000000").toDouble, //-0.094177246
      floatFromBits("10111101 11000000 11100000 00000000").toDouble, //-0.094177246
      intFromBits("10101110 00000111").toShort
    )
    testDoubleHFloat(
      floatFromBits("10111101 11000000 11110000 00000000").toDouble, //-0.09420776
      floatFromBits("10111101 11000001 00000000 00000000").toDouble, //-0.09423828
      intFromBits("10101110 00001000").toShort
    )
    
    // Normalized.
    testDoubleHFloat(-1, -1, intFromBits("10111100 00000000").toShort)
    testDoubleHFloat(-0.5, -0.5, intFromBits("10111000 00000000").toShort)
    testDoubleHFloat(0.5, 0.5, intFromBits("00111000 00000000").toShort)
    testDoubleHFloat(1, 1, intFromBits("00111100 00000000").toShort)

    // Round trip conversion excluding Subnormal, Zero, and NaN
    var i = 0; while (i < 65536) {
      val t = (i & 0x7C00)
      if (t != 0 && t != 0x7C00) {
        val h = i.toShort
        val d = conversion.Double.fromHFloat(h)
        assert(h == conversion.Double.toHFloat(d))
      }

      i += 1
    }
  }
}
