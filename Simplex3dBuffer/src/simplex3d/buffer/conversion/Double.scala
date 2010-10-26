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

package simplex3d.buffer.conversion


/**
 * @author Aleksey Nikiforov (lex)
 */
object Double {

  // SByte
  @inline final def fromSByte(x: Byte) :Double = {
    if (x < -127) -1 else x*0.00787401574803149606
  }
  @inline final def toSByte(x: Double) :Byte = {
    val c = if (x <= -1) -1 else if (x >= 1) 1 else x // clamp
    val s = c*127 // scale
    if (s >= 0) (s + 0.5).toByte else (s - 0.5).toByte // round
  }

  // UByte
  @inline final def fromUByte(x: Byte) :Double = {
    (x & 0xFF)*0.00392156862745098039
  }
  @inline final def toUByte(x: Double) :Byte = {
    val c = if (x <= 0) 0 else if (x >= 1) 1 else x // clamp
    val s = c*255 // scale
    (s + 0.5).toByte // round
  }

  // SShort
  @inline final def fromSShort(x: Short) :Double = {
    if (x < -32767) -1 else x*3.05185094759971922971e-5
  }
  @inline final def toSShort(x: Double) :Short = {
    val c = if (x <= -1) -1 else if (x >= 1) 1 else x // clamp
    val s = c*32767 // scale
    if (s >= 0) (s + 0.5).toShort else (s - 0.5).toShort // round
  }

  // UShort
  @inline final def fromUShort(x: Char) :Double = {
    x*1.52590218966964217594e-5
  }
  @inline final def toUShort(x: Double) :Char = {
    val c = if (x <= 0) 0 else if (x >= 1) 1 else x // clamp
    val s = c*65535 // scale
    (s + 0.5).toChar // round
  }

  // SInt
  @inline final def fromSInt(x: Int) :Double = {
    if (x < -2147483647) -1 else x*4.65661287524579692411e-10
  }
  @inline final def toSInt(x: Double) :Int = {
    val c = if (x <= -1) -1 else if (x >= 1) 1 else x // clamp
    val s = c*2147483647 // scale
    if (s >= 0) (s + 0.5).toInt else (s - 0.5).toInt // round
  }

  // UInt
  @inline final def fromUInt(x: Int) :Double = {
    (x.toLong & 0xFFFFFFFFL)*2.32830643708079737543e-10
  }
  @inline final def toUInt(x: Double) :Int = {
    val c = if (x <= 0) 0 else if (x >= 1) 1 else x // clamp
    val s = c*4294967295d // scale
    (s + 0.5).toLong.toInt // round
  }

  // HalfFloat
  @inline final def fromHalfFloat(s: Short) :Double = {
    Float.fromHalfFloat(s)
  }
  @inline final def toHalfFloat(d: Double) :Short = {
    Float.toHalfFloat(d.toFloat)
  }
}
