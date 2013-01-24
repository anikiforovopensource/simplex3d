/*
 * Simplex3dMath - Test Package
 * Copyright (C) 2010-2011, Aleksey Nikiforov
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

package simplex3d.bench.math

import scala.language.implicitConversions
import simplex3d.math._
import simplex3d.math.doublex._
import simplex3d.math.doublex.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
object MatSharedCodeBench {
  def main(args: Array[String]) {
    val tc = new MatSharedCodeBench()
//    tc.testMul()
//    tc.testMul()
//    tc.testMul()
//    tc.testMul()
//    tc.testScalarDiv()
//    tc.testScalarDiv()
//    tc.testScalarDiv()
//    tc.testScalarDiv()
    tc.testSubComponents()
    tc.testSubComponents()
    tc.testSubComponents()
    tc.testSubComponents()
  }
}

class MatSharedCodeBench {
  val length = 10000
  val loops = 10000

  val mc = 0.001

  def testMul() {
    var start = 0L

    start = System.currentTimeMillis
    testImplementedMul(length, loops)
    System.gc
    val testImplementedMulTime = System.currentTimeMillis - start
    
    start = System.currentTimeMillis
    testInlinedMul(length, loops)
    System.gc
    val testInlinedMulTime = System.currentTimeMillis - start

    // Test conclusion: shared implementation interferes with EscapeAnalysis.
    start = System.currentTimeMillis
    testSharedMul(length, loops)
    System.gc
    val testSharedMulTime = System.currentTimeMillis - start

    println("Inlined mul time: " + testInlinedMulTime + ".")
    println("Shared mul time: " + testSharedMulTime + ".")
    println("Implemented mul time: " + testImplementedMulTime + ".")
  }

  def testScalarDiv() {
    var start = 0L

    start = System.currentTimeMillis
    testImplementedSDiv(length, loops)
    System.gc
    val testImplementedSDivTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testInlinedSDiv(length, loops)
    System.gc
    val testInlinedSDivTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testSharedSDiv(length, loops)
    System.gc
    val testSharedSDivTime = System.currentTimeMillis - start

    println("Inlined scalar div time: " + testInlinedSDivTime + ".")
    println("Shared scalar div time: " + testSharedSDivTime + ".")
    println("Implemented scalar div time: " + testImplementedSDivTime + ".")
  }

  def testSubComponents() {
    var start = 0L

    start = System.currentTimeMillis
    testImplementedSub(length, loops)
    System.gc
    val implementedTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testDedicatedSub(length, loops)
    System.gc
    val dedicatedTime = System.currentTimeMillis - start

    println("Implemented sub: " + implementedTime + ".")
    println("Dedicated sub: " + dedicatedTime + ".")
  }


  def testInlinedMul(length: Int, loops: Int) {
    val res = new Mat3m(1, 0, 0, 0, 1, 0, 0, 0, 1)
    val m = new Mat3m(mc, mc, mc, mc, mc, mc, mc, mc, mc)

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {

        // Bench code
        val a = new Mat3m(i, i + 1, i + 2, i + 3, i + 4, i + 5, i + 6, i + 7, i + 8)
        res += a*m

        i += 1
      }

      l += 1
    }

    println(res)
  }

  def testSharedMul(length: Int, loops: Int) {
    val res = new Mat3s(1, 0, 0, 0, 1, 0, 0, 0, 1)
    val m = new Mat3s(mc, mc, mc, mc, mc, mc, mc, mc, mc)

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {

        // Bench code
        val a = new Mat3s(i, i + 1, i + 2, i + 3, i + 4, i + 5, i + 6, i + 7, i + 8)
        res += a*m

        i += 1
      }

      l += 1
    }

    println(res)
  }

  def testImplementedMul(length: Int, loops: Int) {
    val res = Mat3d(1, 0, 0, 0, 1, 0, 0, 0, 1)
    val m = Mat3d(mc, mc, mc, mc, mc, mc, mc, mc, mc)

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {

        // Bench code
        val a = Mat3d(i, i + 1, i + 2, i + 3, i + 4, i + 5, i + 6, i + 7, i + 8)
        res += a*m

        i += 1
      }

      l += 1
    }

    println(res)
  }

  def testInlinedSDiv(length: Int, loops: Int) {
    val res = new Mat3m(1, 0, 0, 0, 1, 0, 0, 0, 1)

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {

        // Bench code
        val a = new Mat3m(i, i + 1, i + 2, i + 3, i + 4, i + 5, i + 6, i + 7, i + 8)
        res += a/toDouble(i + 10)

        i += 1
      }

      l += 1
    }

    println(res)
  }

  def testSharedSDiv(length: Int, loops: Int) {
    val res = new Mat3s(1, 0, 0, 0, 1, 0, 0, 0, 1)

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {

        // Bench code
        val a = new Mat3s(i, i + 1, i + 2, i + 3, i + 4, i + 5, i + 6, i + 7, i + 8)
        res += a/toDouble(i + 10)

        i += 1
      }

      l += 1
    }

    println(res)
  }

  def testImplementedSDiv(length: Int, loops: Int) {
    val res = Mat3d(1, 0, 0, 0, 1, 0, 0, 0, 1)

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {

        // Bench code
        val a = Mat3d(i, i + 1, i + 2, i + 3, i + 4, i + 5, i + 6, i + 7, i + 8)
        res += a/toDouble(i + 10)

        i += 1
      }

      l += 1
    }

    println(res)
  }

  def testImplementedSub(length: Int, loops: Int) {
    val res = Mat3d(1, 0, 0, 0, 1, 0, 0, 0, 1)

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {

        // Bench code
        val a = Mat3d(i, i + 1, i + 2, i + 3, i + 4, i + 5, i + 6, i + 7, i + 8)
        res += 2 - a

        i += 1
      }

      l += 1
    }

    println(res)
  }
  
  class EextInt(val i: Int) { def -(m: Mat3m) = m.subComponents(i) }
  implicit def int2EI(x: Int) = new EextInt(x)
  def testDedicatedSub(length: Int, loops: Int) {
    val res = new Mat3m(1, 0, 0, 0, 1, 0, 0, 0, 1)

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {

        // Bench code
        val a = new Mat3m(i, i + 1, i + 2, i + 3, i + 4, i + 5, i + 6, i + 7, i + 8)
        res += 2 - a

        i += 1
      }

      l += 1
    }

    println(res)
  }
}

sealed abstract class ReadMat3m {
  // Column major order.
  def m00: Double; def m01: Double; def m02: Double // column
  def m10: Double; def m11: Double; def m12: Double // column
  def m20: Double; def m21: Double; def m22: Double // column

  final def subComponents(s: Double) = new Mat3m(
    s - m00, s - m01, s - m02,
    s - m10, s - m11, s - m12,
    s - m20, s - m21, s - m22
  )
  
  final def /(s: Double) = { val inv = 1/s; new Mat3m(
    inv*m00, inv*m01, inv*m02,
    inv*m10, inv*m11, inv*m12,
    inv*m20, inv*m21, inv*m22
  )}

  final def +(s: Double) = new Mat3m(
    m00 + s, m01 + s, m02 + s,
    m10 + s, m11 + s, m12 + s,
    m20 + s, m21 + s, m22 + s
  )

  final def +(m: ReadMat3m) = new Mat3m(
    m00 + m.m00, m01 + m.m01, m02 + m.m02,
    m10 + m.m10, m11 + m.m11, m12 + m.m12,
    m20 + m.m20, m21 + m.m21, m22 + m.m22
  )

  final def *(m: ReadMat3m) = new Mat3m(
    m00*m.m00 + m10*m.m01 + m20*m.m02,
    m01*m.m00 + m11*m.m01 + m21*m.m02,
    m02*m.m00 + m12*m.m01 + m22*m.m02,

    m00*m.m10 + m10*m.m11 + m20*m.m12,
    m01*m.m10 + m11*m.m11 + m21*m.m12,
    m02*m.m10 + m12*m.m11 + m22*m.m12,

    m00*m.m20 + m10*m.m21 + m20*m.m22,
    m01*m.m20 + m11*m.m21 + m21*m.m22,
    m02*m.m20 + m12*m.m21 + m22*m.m22
  )

  final override def toString :String = {
    this.getClass.getSimpleName +
    "(" +
      m00 + ", " + m01 + ", " + m02 + "; " +
      m10 + ", " + m11 + ", " + m12 + "; " +
      m20 + ", " + m21 + ", " + m22 +
    ")"
  }
}

final class Mat3m (
  var m00: Double, var m01: Double, var m02: Double,
  var m10: Double, var m11: Double, var m12: Double,
  var m20: Double, var m21: Double, var m22: Double
) extends ReadMat3m with Mutable
{
  def +=(m: ReadMat3m) {
    m00 += m.m00; m01 += m.m01; m02 += m.m02;
    m10 += m.m10; m11 += m.m11; m12 += m.m12;
    m20 += m.m20; m21 += m.m21; m22 += m.m22
  }

  def *=(m: ReadMat3m) {
    val a00 = m00*m.m00 + m10*m.m01 + m20*m.m02
    val a01 = m01*m.m00 + m11*m.m01 + m21*m.m02
    val a02 = m02*m.m00 + m12*m.m01 + m22*m.m02

    val a10 = m00*m.m10 + m10*m.m11 + m20*m.m12
    val a11 = m01*m.m10 + m11*m.m11 + m21*m.m12
    val a12 = m02*m.m10 + m12*m.m11 + m22*m.m12

    val a20 = m00*m.m20 + m10*m.m21 + m20*m.m22
    val a21 = m01*m.m20 + m11*m.m21 + m21*m.m22
    val a22 = m02*m.m20 + m12*m.m21 + m22*m.m22

    m00 = a00; m01 = a01; m02 = a02
    m10 = a10; m11 = a11; m12 = a12
    m20 = a20; m21 = a21; m22 = a22
  }

  def set(
    m00: Double, m01: Double, m02: Double,
    m10: Double, m11: Double, m12: Double,
    m20: Double, m21: Double, m22: Double
  ) {
    this.m00 = m00; this.m01 = m01; this.m02 = m02;
    this.m10 = m10; this.m11 = m11; this.m12 = m12;
    this.m20 = m20; this.m21 = m21; this.m22 = m22
  }
}

sealed abstract class ReadMat3s {
  // Column major order.
  def m00: Double; def m01: Double; def m02: Double // column
  def m10: Double; def m11: Double; def m12: Double // column
  def m20: Double; def m21: Double; def m22: Double // column

  final def *(s: Double) = new Mat3s(
    s*m00, s*m01, s*m02,
    s*m10, s*m11, s*m12,
    s*m20, s*m21, s*m22
  )
  final def /(s: Double) = this*(1/s)

  // Test conclusion: this code interferes with EscapeAnalysis.
  final def +(m: ReadMat3s) = { val res = new Mat3s(); add(m, res); res }
  final def *(m: ReadMat3s) = { val res = new Mat3s(); mul(m, res); res }

  protected final def add(m: ReadMat3s, res: Mat3s) {
    res.m00 = m00 + m.m00; res.m01 = m01 + m.m01; res.m02 = m02 + m.m02;
    res.m10 = m10 + m.m10; res.m11 = m11 + m.m11; res.m12 = m12 + m.m12;
    res.m20 = m20 + m.m20; res.m21 = m21 + m.m21; res.m22 = m22 + m.m22
  }
  protected final def mul(m: ReadMat3s, res: Mat3s) {
    val a00 = m00*m.m00 + m10*m.m01 + m20*m.m02
    val a01 = m01*m.m00 + m11*m.m01 + m21*m.m02
    val a02 = m02*m.m00 + m12*m.m01 + m22*m.m02

    val a10 = m00*m.m10 + m10*m.m11 + m20*m.m12
    val a11 = m01*m.m10 + m11*m.m11 + m21*m.m12
    val a12 = m02*m.m10 + m12*m.m11 + m22*m.m12

    val a20 = m00*m.m20 + m10*m.m21 + m20*m.m22
    val a21 = m01*m.m20 + m11*m.m21 + m21*m.m22
    val a22 = m02*m.m20 + m12*m.m21 + m22*m.m22

    res.m00 = a00; res.m01 = a01; res.m02 = a02
    res.m10 = a10; res.m11 = a11; res.m12 = a12
    res.m20 = a20; res.m21 = a21; res.m22 = a22
  }

  final override def toString :String = {
    this.getClass.getSimpleName +
    "(" +
      m00 + ", " + m01 + ", " + m02 + "; " +
      m10 + ", " + m11 + ", " + m12 + "; " +
      m20 + ", " + m21 + ", " + m22 +
    ")"
  }
}

final class Mat3s (
  var m00: Double, var m01: Double, var m02: Double,
  var m10: Double, var m11: Double, var m12: Double,
  var m20: Double, var m21: Double, var m22: Double
) extends ReadMat3s with Mutable
{
  def this() = this(0, 0, 0, 0, 0, 0, 0, 0, 0)

  def +=(m: ReadMat3s) { add(m, this) }
  def *=(m: ReadMat3s) { mul(m, this) }

  def set(
    m00: Double, m01: Double, m02: Double,
    m10: Double, m11: Double, m12: Double,
    m20: Double, m21: Double, m22: Double
  ) {
    this.m00 = m00; this.m01 = m01; this.m02 = m02;
    this.m10 = m10; this.m11 = m11; this.m12 = m12;
    this.m20 = m20; this.m21 = m21; this.m22 = m22
  }
}
