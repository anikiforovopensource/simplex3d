/*
 * Simplex3d, BaseBuffer module
 * Copyright (C) 2010 Simplex3d Team
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

package simplex3d.buffer

import simplex3d.math._


/**
 * @author Aleksey Nikiforov (lex)
 */
object HalfFloat {
  def toHalfFloat(f: Float) :Short = {
    val bits = java.lang.Float.floatToRawIntBits(f)
    val exponent = bits & 0x7F800000

    // subnormal
    if (exponent < (113 << 23)) {
      return short(bits & 0x80000000)
    }

    val rounded = if ((bits & 0x00001000) != 0) bits + 0x00001000 else bits

    // infinite or nan
    if (exponent > (142 << 23)) {
      short((rounded & 0x80000000) | 0x7C00)
    }

    // normalized value
    else {
      val high = (rounded & 0xC0000000) | ((rounded << 3) & 0x3FFFFFFF)
      short(high >> 16)
    }
  }

  def fromHalfFloat(s: Short) :Float = {
    val bits = s << 16

    // subnormal
    if ((bits & 0x7C000000) == 0) {
      java.lang.Float.intBitsToFloat(bits & 0x80000000)
    }

    // other
    else {
      val res = (bits & 0xC0000000) | ((bits >>> 3) & 0x07FFFFFF)
      val f =
        if ((res & 0x70000000) != 0 && (s & 0x7C00) != 0x7C00) {
          res
        }
        else {
          res | 0x38000000
        }
      java.lang.Float.intBitsToFloat(f)
    }
  }
}
