/*
 * Simplex3d, CoreData module
 * Copyright (C) 2010-2011, Simplex3d Team
 *
 * This file is part of Simplex3dData.
 *
 * Simplex3dData is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dData is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.data.conversion


/**
 * @author Aleksey Nikiforov (lex)
 */
object Float {

  // SByte
  @inline final def fromSByte(x: Byte) :Float = {
    if (x < -127) -1 else (x*0.00787401574803149606).toFloat
  }
  @inline final def toSByte(x: Float) :Byte = {
    val c = if (x <= -1) -1 else if (x >= 1) 1 else x // clamp
    val s = c*127 // scale
    if (s >= 0) (s + 0.5f).toByte else (s - 0.5f).toByte // round
  }

  // UByte
  @inline final def fromUByte(x: Byte) :Float = {
    ((x & 0xFF)*0.00392156862745098039).toFloat
  }
  @inline final def toUByte(x: Float) :Byte = {
    val c = if (x <= 0) 0 else if (x >= 1) 1 else x // clamp
    val s = c*255 // scale
    (s + 0.5f).toByte // round
  }

  // SShort
  @inline final def fromSShort(x: Short) :Float = {
    if (x < -32767) -1 else (x*3.05185094759971922971e-5).toFloat
  }
  @inline final def toSShort(x: Float) :Short = {
    val c = if (x <= -1) -1 else if (x >= 1) 1 else x // clamp
    val s = c*32767 // scale
    if (s >= 0) (s + 0.5f).toShort else (s - 0.5f).toShort // round
  }

  // UShort
  @inline final def fromUShort(x: Char) :Float = {
    (x*1.52590218966964217594e-5).toFloat
  }
  @inline final def toUShort(x: Float) :Char = {
    val c = if (x <= 0) 0 else if (x >= 1) 1 else x // clamp
    val s = c*65535 // scale
    (s + 0.5f).toChar // round
  }
  
  // SInt
  @inline final def fromSInt(x: Int) :Float = {
    if (x < -2147483647) -1 else (x*4.65661287524579692411e-10).toFloat
  }
  @inline final def toSInt(x: Float) :Int = {
    val c = if (x <= -1) -1 else if (x >= 1) 1 else x // clamp
    val s = c*2147483647d // scale
    if (s >= 0) (s + 0.5).toInt else (s - 0.5).toInt // round
  }

  // UInt
  @inline final def fromUInt(x: Int) :Float = {
    ((x.toLong & 0xFFFFFFFFL)*2.32830643708079737543e-10).toFloat
  }
  @inline final def toUInt(x: Float) :Int = {
    val c = if (x <= 0) 0 else if (x >= 1) 1 else x // clamp
    val s = c*4294967295d // scale
    (s + 0.5).toLong.toInt // round
  }

  // HFloat
  @inline final def fromHFloat(x: Short) :Float = {
    val bits = x << 16
    val x7C = (bits & 0x7C000000)

    // subnormal
    if (x7C == 0) {
      java.lang.Float.intBitsToFloat(bits & 0x80000000)
    }

    // other
    else {
      val res = (bits & 0xC0000000) | ((bits >>> 3) & 0x07FFFFFF)
      val f =
        if ((res & 0x70000000) != 0 && x7C != 0x7C000000) res
        else res | 0x38000000

      java.lang.Float.intBitsToFloat(f)
    }
  }
  @inline final def toHFloat(x: Float) :Short = {
    val bits = java.lang.Float.floatToRawIntBits(x)
    val exponent = bits & 0x7F800000

    // subnormal
    if (exponent < (113 << 23)) {
      ((bits & 0x80000000) >> 16).toShort
    }

    // overflow, inf, or nan
    else if (exponent > (142 << 23)) {
      // inf or nan
      if (exponent == 0x7F800000) {
        val high = (bits & 0xC0000000) | ((bits << 3) & 0x3FFFFFFF)
        (high >> 16).toShort
      }
      // overflow
      else {
        (((bits & 0x80000000) | 0x7C000000) >> 16).toShort
      }
    }

    // normalized value
    else {
      val rounded = if ((bits & 0x00001000) != 0) bits + 0x00001000 else bits
      val high = (rounded & 0xC0000000) | ((rounded << 3) & 0x3FFFFFFF)
      (high >> 16).toShort
    }
  }
}
