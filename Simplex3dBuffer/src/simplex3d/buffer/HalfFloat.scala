/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package simplex3d.buffer

import simplex3d.math._


object HalfFloat {
  def toHalfFloat(f: Float) :Short = {
    val bits = java.lang.Float.floatToRawIntBits(f)
    val exponent = bits & 0x7F800000

    // subnormal
    if (exponent < (113 << 23)) {
      return short(bits & 0x80000000)
    }

    val rounded =
      if ((bits & 0x00001000) != 0) {
        if (bits > 0) bits + 0x00002000 else bits - 0x00002000
      } else {
        bits
      }

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
