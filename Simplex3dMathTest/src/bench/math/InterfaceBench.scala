/*
 * Simplex3d, MathTest package
 * Copyright (C) 2009-2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dMathTest.
 *
 * Simplex3dMathTest is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dMathTest is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package bench.math

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.floatx.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
object InterfaceBench {
  def main(args: Array[String]) {
    new InterfaceBenchCase().run()
  }
}

class InterfaceBenchCase {
  val length = 10000
  val loops = 20000
  
  val random = new java.util.Random(1)
  def rd = random.nextFloat()
  val data = new Array[Vec3](length)
  for (i <- 0 until length) {
    data(i) = Vec3(rd, rd, rd)
  }

  def run() {
    var start = 0L

    start = System.currentTimeMillis
    testInterface(length, loops)
    val interfaceTime = System.currentTimeMillis - start
    
    start = System.currentTimeMillis
    testCopy(length, loops)
    val copyTime = System.currentTimeMillis - start

    println("Interface time: " + interfaceTime +
            ", copy time: " + copyTime + ".")
  }

  def testCopy(length: Int, loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < length - 3) {

        // Bench code
        val m = Mat3x4m(data(i), data(i + 1), data(i + 2), data(i + 3))
        val q = IBMath.quatFrom(Mat3x3m(m))
        answer += toInt((q.a + q.b + q.c + q.d))

        i += 1
      }
      l += 1
    }

    println(answer)
  }

  def testInterface(length: Int, loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < length - 3) {

        // Bench code
        val m = Mat3x4m(data(i), data(i + 1), data(i + 2), data(i + 3))
        val q = Quat4(1, 0, 0, 0)
        IBMath.quatFrom(m, q)
        answer += toInt((q.a + q.b + q.c + q.d))

        i += 1
      }
      l += 1
    }

    println(answer)
  }

  
}

trait SubMat3m {
  // Column major order.
  var m00: Float; var m10: Float; var m20: Float // column
  var m01: Float; var m11: Float; var m21: Float // column
  var m02: Float; var m12: Float; var m22: Float // column

  def set(
    m00: Float, m10: Float, m20: Float,
    m01: Float, m11: Float, m21: Float,
    m02: Float, m12: Float, m22: Float
  ) {
    this.m00 = m00; this.m10 = m10; this.m20 = m20
    this.m01 = m01; this.m11 = m11; this.m21 = m21
    this.m02 = m02; this.m12 = m12; this.m22 = m22
  }
}

final class Mat3x4m (
  var m00: Float, var m10: Float, var m20: Float,
  var m01: Float, var m11: Float, var m21: Float,
  var m02: Float, var m12: Float, var m22: Float,
  var m03: Float, var m13: Float, var m23: Float
) extends SubMat3m

object Mat3x4m {
  def apply(s: Float) = new Mat3x4m(
    s, 0, 0,
    0, s, 0,
    0, 0, s,
    0, 0, 0
  )

  def apply(c0: ReadVec3, c1: ReadVec3, c2: ReadVec3, c3: ReadVec3) =
  new Mat3x4m(
    c0.x, c0.y, c0.z,
    c1.x, c1.y, c1.z,
    c2.x, c2.y, c2.z,
    c3.x, c3.y, c3.z
  )
}

object Mat3x3m {
  def apply(m: Mat3x4m) = Mat3(
    m.m00, m.m10, m.m20,
    m.m01, m.m11, m.m21,
    m.m02, m.m12, m.m22
  )
}

object IBMath {
  def quatFrom(m: SubMat3m, result: Quat4) {
    import m._

    val trace = m00 + m11 + m22

    if (trace > 0) {
      val t = trace + 1
      val s = inversesqrt(t)*0.5f
      result.a = s*t
      result.d = (m10 - m01)*s
      result.c = (m02 - m20)*s
      result.b = (m21 - m12)*s
    }
    else if (m00 > m11 && m00 > m22) {
      val t = m00 - m11 - m22 + 1
      val s = inversesqrt(t)*0.5f
      result.b = s*t
      result.c = (m10 + m01)*s
      result.d = (m02 + m20)*s
      result.a = (m21 - m12)*s
    }
    else if (m11 > m22) {
      val t = -m00 + m11 - m22 + 1
      val s = inversesqrt(t)*0.5f
      result.c = s*t
      result.b = (m10 + m01)*s
      result.a = (m02 - m20)*s
      result.d = (m21 + m12)*s
    }
    else {
      val t = -m00 - m11 + m22 + 1
      val s = inversesqrt(t)*0.5f
      result.d = s*t
      result.a = (m10 - m01)*s
      result.b = (m02 + m20)*s
      result.c = (m21 + m12)*s
    }
  }

  def quatFrom(m: Mat3) = {
    import m._

    val result = Quat4(1, 0, 0, 0)
    val trace = m00 + m11 + m22

    if (trace > 0) {
      val t = trace + 1
      val s = inversesqrt(t)*0.5f
      result.a = s*t
      result.d = (m10 - m01)*s
      result.c = (m02 - m20)*s
      result.b = (m21 - m12)*s
    }
    else if (m00 > m11 && m00 > m22) {
      val t = m00 - m11 - m22 + 1
      val s = inversesqrt(t)*0.5f
      result.b = s*t
      result.c = (m10 + m01)*s
      result.d = (m02 + m20)*s
      result.a = (m21 - m12)*s
    }
    else if (m11 > m22) {
      val t = -m00 + m11 - m22 + 1
      val s = inversesqrt(t)*0.5f
      result.c = s*t
      result.b = (m10 + m01)*s
      result.a = (m02 - m20)*s
      result.d = (m21 + m12)*s
    }
    else {
      val t = -m00 - m11 + m22 + 1
      val s = inversesqrt(t)*0.5f
      result.d = s*t
      result.a = (m10 - m01)*s
      result.b = (m02 + m20)*s
      result.c = (m21 + m12)*s
    }

    result
  }
}
