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

package bench.buffer

import simplex3d.buffer.conversion.Float._


/**
 * @author Aleksey Nikiforov (lex)
 */
object HalfFloatBench {
  def main(args: Array[String]) {
    test()
    test()
    test()
  }

  val loops = 10000

  def test() {
    println("\nTesting...")
    var start = 0L

    start = System.currentTimeMillis
    testFromHalfFloat(loops)
    val fromHalfFloatTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testToHalfFloat(loops)
    val toHalfFloatTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testRoundTrip(loops)
    val roundTripTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testFromHalfFloatLookup(loops)
    val fromHalfFloatLookupTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testToHalfFloatLookup(loops)
    val toHalfFloatLookupTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testRoundTripLookup(loops)
    val roundTripLookupTime = System.currentTimeMillis - start

    println("\nResults:")
    println("From HaflFloat time: " + fromHalfFloatTime + ".")
    println("To HaflFloat time: " + toHalfFloatTime + ".")
    println("Round-trip time: " + roundTripTime + ".")
    println("From HaflFloat Lookup time: " + fromHalfFloatLookupTime + ".")
    println("To HaflFloat Lookup time: " + toHalfFloatLookupTime + ".")
    println("Round-trip Lookup time: " + roundTripLookupTime + ".")
  }

  final def testFromHalfFloat(loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < 65535) {
        answer += fromHalfFloat(i.toShort).toInt

        i += 1
      }
      l += 1
    }

    println(answer)
  }

  final def testToHalfFloat(loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < 65535) {
        answer += toHalfFloat(i*1.111f)

        i += 1
      }
      l += 1
    }

    println(answer)
  }

  final def testRoundTrip(loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < 65535) {
        answer += toHalfFloat(fromHalfFloat(i.toShort))

        i += 1
      }
      l += 1
    }

    println(answer)
  }

  final def testFromHalfFloatLookup(loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < 65535) {
        answer += HalfFloatTableLookup.fromHalfFloat(i.toShort).toInt

        i += 1
      }
      l += 1
    }

    println(answer)
  }

  final def testToHalfFloatLookup(loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < 65535) {
        answer += HalfFloatTableLookup.toHalfFloat(i*1.111f)

        i += 1
      }
      l += 1
    }

    println(answer)
  }

  final def testRoundTripLookup(loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < 65535) {
        answer += HalfFloatTableLookup.toHalfFloat(HalfFloatTableLookup.fromHalfFloat(i.toShort))

        i += 1
      }
      l += 1
    }

    println(answer)
  }
}

/* This implementation is uncomplete, untested, and probably buggy.
 */
object HalfFloatTableLookup {
  /* Base on the paper "Fast Half Float Conversions" by Jeroen van der Zijp */
  private def convertMantissa(i: Int) :Int = {
    var m = i << 13
    var e = 0

    while ((m & 0x00800000) == 0) {
      e -= 0x00800000
      m <<= 1
    }

    m &= ~0x00800000
    e += 0x38800000

    m | e
  }

  val mantissaTable = new Array[Int](2048); {
    mantissaTable(0) = 0

    var i = 1; while (i < 1024) {
      convertMantissa(i)

      i += 1
    }

    i = 1024; while (i < 2048) {
      mantissaTable(i) = 0x38000000

      i += 1
    }
  }

  val offsetTable = new Array[Short](64); {
    var i = 0; while (i < 64) {
      offsetTable(i) = 1024

      i += 1
    }

    offsetTable(0) = 0
    offsetTable(32) = 0
  }

  val exponentTable = new Array[Int](64); {
    exponentTable(0) = 0

    var i = 1; while (i < 31) {
      exponentTable(i) = i << 23

      i += 1
    }

    exponentTable(31) = 0x47800000
    exponentTable(32) = 0x80000000

    i = 33; while (i < 63) {
      exponentTable(i) = 0x80000000 + ((i - 32) << 23)

      i += 1
    }

    exponentTable(63) = 0xC7800000
  }

  def fromHalfFloat(x: Short) :Float = {
    val i = ((x & 0xFFFF) >> 10) & 0xFF
    val j = (offsetTable(i) + (x & 0x3FF)) & 0x7FF
    val m = mantissaTable(j)
    val e = exponentTable(i)
    java.lang.Float.intBitsToFloat(m + e)
  }


  val baseTable = new Array[Short](512)
  val shiftTable = new Array[Byte](512);
  {
    var i = 0; while (i < 256) {
      var e = i - 127

      if (e < -24) {
        baseTable(i | 0x000) = 0x0000.toShort
        baseTable(i | 0x100) = 0x8000.toShort
        shiftTable(i | 0x000) = 24.toByte
        shiftTable(i | 0x100) = 24.toByte
      }
      else if (e < -14) {
        baseTable(i | 0x000) = (0x0400 >> (-e - 14)).toShort
        baseTable(i | 0x100) = ((0x0400 >> (-e - 14)) | 0x8000).toShort
        shiftTable(i | 0x000) = (-e - 1).toByte
        shiftTable(i | 0x100) = (-e - 1).toByte
      }
      else if (e <= 15) {
        baseTable(i | 0x000) = ((e + 15) << 10).toShort
        baseTable(i | 0x100) = (((e + 15) << 10) | 0x8000).toShort
        shiftTable(i | 0x000) = 13.toByte
        shiftTable(i | 0x100) = 13.toByte
      }
      else if (e < 128) {
        baseTable(i | 0x000) = 0x7C00.toShort
        baseTable(i | 0x100) = 0xFC00.toShort
        shiftTable(i | 0x000) = 24.toByte
        shiftTable(i | 0x100) = 24.toByte
      }
      else {
        baseTable(i | 0x000) = 0x7C00.toShort
        baseTable(i | 0x100) = 0xFC00.toShort
        shiftTable(i | 0x000) = 13.toByte
        shiftTable(i | 0x100) = 13.toByte
      }

      i += 1
    }
  }

  def toHalfFloat(x: Float) :Short = {
    val b = java.lang.Float.floatToRawIntBits(x)
    val i = (b >> 23) & 0x1FF

    val base = baseTable(i)
    val shift = shiftTable(i)
    (base + ((b & 0x007FFFFF) >> shift)).toShort
  }
}
