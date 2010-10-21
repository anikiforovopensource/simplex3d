/*
 * Simplex3d, BaseBuffer module
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


// An empty class to make -Xno-forwarders work
private[buffer] class HalfFloat


/**
 * @author Aleksey Nikiforov (lex)
 */
object HalfFloat {
  @inline final def fromFloat(f: Float) :Short = {
    val bits = java.lang.Float.floatToRawIntBits(f)
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

  @inline final def toFloat(s: Short) :Float = {
    val bits = s << 16

    // subnormal
    if ((bits & 0x7C000000) == 0) {
      java.lang.Float.intBitsToFloat(bits & 0x80000000) //XXX change to if else
    }

    // other
    else {
      val res = (bits & 0xC0000000) | ((bits >>> 3) & 0x07FFFFFF)
      val f =
        if ((res & 0x70000000) != 0 && (bits & 0x7C000000) != 0x7C000000) {
          res
        }
        else {
          res | 0x38000000
        }
      java.lang.Float.intBitsToFloat(f)
    }
  }

  @inline final def fromDouble(d: Double) :Short = {
    fromFloat(d.toFloat)
  }
  @inline final def toDouble(s: Short) :Double = {
    toFloat(s)
  }
}
