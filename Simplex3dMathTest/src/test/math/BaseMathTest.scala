/*
 * Simplex3d, MathTest package
 * Copyright (C) 2010, Simplex3d Team
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

package test.math

import org.scalatest._
import simplex3d.math._


/**
 * @author Aleksey Nikiforov (lex)
 */
class BaseMathTest extends FunSuite {

  test("Casting") {
    val t0: Boolean = false
    val b0: Byte = 0
    val s0: Short = 0
    val i0: Int = 0
    val l0: Long = 0
    val f0: Float = 0
    val d0: Double = 0

    val t1: Boolean = true
    val b1: Byte = 1
    val s1: Short = 1
    val i1: Int = 1
    val l1: Long = 1
    val f1: Float = 1
    val d1: Double = 1

    val b2: Byte = 2
    val s2: Short = 2
    val i2: Int = 2
    val l2: Long = 2
    val f2: Float = 2
    val d2: Double = 2

    val b3: Byte = -1
    val s3: Short = -1
    val i3: Int = -1
    val l3: Long = -1
    val f3: Float = -1
    val d3: Double = -1

    assert(bool(t1).isInstanceOf[Boolean])
    assert(bool(b1).isInstanceOf[Boolean])
    assert(bool(s1).isInstanceOf[Boolean])
    assert(bool(i1).isInstanceOf[Boolean])
    assert(bool(l1).isInstanceOf[Boolean])
    assert(bool(f1).isInstanceOf[Boolean])
    assert(bool(d1).isInstanceOf[Boolean])

    assert(bool(t1))
    assert(bool(b1))
    assert(bool(s1))
    assert(bool(i1))
    assert(bool(l1))
    assert(bool(f1))
    assert(bool(d1))

    assert(bool(b2))
    assert(bool(s2))
    assert(bool(i2))
    assert(bool(l2))
    assert(bool(f2))
    assert(bool(d2))

    assert(bool(b3))
    assert(bool(s3))
    assert(bool(i3))
    assert(bool(l3))
    assert(bool(f3))
    assert(bool(d3))

    assert(!bool(t0))
    assert(!bool(b0))
    assert(!bool(s0))
    assert(!bool(i0))
    assert(!bool(l0))
    assert(!bool(f0))
    assert(!bool(d0))

    assert(byte(t1).isInstanceOf[Byte])
    assert(byte(b1).isInstanceOf[Byte])
    assert(byte(s1).isInstanceOf[Byte])
    assert(byte(i1).isInstanceOf[Byte])
    assert(byte(l1).isInstanceOf[Byte])
    assert(byte(f1).isInstanceOf[Byte])
    assert(byte(d1).isInstanceOf[Byte])

    assert(byte(t1) == b1)
    assert(byte(b1) == b1)
    assert(byte(s1) == b1)
    assert(byte(i1) == b1)
    assert(byte(l1) == b1)
    assert(byte(f1) == b1)
    assert(byte(d1) == b1)

    assert(byte(t0) == b0)
    assert(byte(b0) == b0)
    assert(byte(s0) == b0)
    assert(byte(i0) == b0)
    assert(byte(l0) == b0)
    assert(byte(f0) == b0)
    assert(byte(d0) == b0)

    assert(short(t1).isInstanceOf[Short])
    assert(short(b1).isInstanceOf[Short])
    assert(short(s1).isInstanceOf[Short])
    assert(short(i1).isInstanceOf[Short])
    assert(short(l1).isInstanceOf[Short])
    assert(short(f1).isInstanceOf[Short])
    assert(short(d1).isInstanceOf[Short])

    assert(short(t1) == s1)
    assert(short(b1) == s1)
    assert(short(s1) == s1)
    assert(short(i1) == s1)
    assert(short(l1) == s1)
    assert(short(f1) == s1)
    assert(short(d1) == s1)

    assert(short(t0) == s0)
    assert(short(b0) == s0)
    assert(short(s0) == s0)
    assert(short(i0) == s0)
    assert(short(l0) == s0)
    assert(short(f0) == s0)
    assert(short(d0) == s0)

    assert(int(t1).isInstanceOf[Int])
    assert(int(b1).isInstanceOf[Int])
    assert(int(s1).isInstanceOf[Int])
    assert(int(i1).isInstanceOf[Int])
    assert(int(l1).isInstanceOf[Int])
    assert(int(f1).isInstanceOf[Int])
    assert(int(d1).isInstanceOf[Int])

    assert(int(t1) == i1)
    assert(int(b1) == i1)
    assert(int(s1) == i1)
    assert(int(i1) == i1)
    assert(int(l1) == i1)
    assert(int(f1) == i1)
    assert(int(d1) == i1)

    assert(int(t0) == i0)
    assert(int(b0) == i0)
    assert(int(s0) == i0)
    assert(int(i0) == i0)
    assert(int(l0) == i0)
    assert(int(f0) == i0)
    assert(int(d0) == i0)

    assert(long(t1).isInstanceOf[Long])
    assert(long(b1).isInstanceOf[Long])
    assert(long(s1).isInstanceOf[Long])
    assert(long(i1).isInstanceOf[Long])
    assert(long(l1).isInstanceOf[Long])
    assert(long(f1).isInstanceOf[Long])
    assert(long(d1).isInstanceOf[Long])

    assert(long(t1) == l1)
    assert(long(b1) == l1)
    assert(long(s1) == l1)
    assert(long(i1) == l1)
    assert(long(l1) == l1)
    assert(long(f1) == l1)
    assert(long(d1) == l1)

    assert(long(t0) == l0)
    assert(long(b0) == l0)
    assert(long(s0) == l0)
    assert(long(i0) == l0)
    assert(long(l0) == l0)
    assert(long(f0) == l0)
    assert(long(d0) == l0)

    assert(float(t1).isInstanceOf[Float])
    assert(float(b1).isInstanceOf[Float])
    assert(float(s1).isInstanceOf[Float])
    assert(float(i1).isInstanceOf[Float])
    assert(float(l1).isInstanceOf[Float])
    assert(float(f1).isInstanceOf[Float])
    assert(float(d1).isInstanceOf[Float])

    assert(float(t1) == f1)
    assert(float(b1) == f1)
    assert(float(s1) == f1)
    assert(float(i1) == f1)
    assert(float(l1) == f1)
    assert(float(f1) == f1)
    assert(float(d1) == f1)

    assert(float(t0) == f0)
    assert(float(b0) == f0)
    assert(float(s0) == f0)
    assert(float(i0) == f0)
    assert(float(l0) == f0)
    assert(float(f0) == f0)
    assert(float(d0) == f0)

    assert(double(t1).isInstanceOf[Double])
    assert(double(b1).isInstanceOf[Double])
    assert(double(s1).isInstanceOf[Double])
    assert(double(i1).isInstanceOf[Double])
    assert(double(l1).isInstanceOf[Double])
    assert(double(f1).isInstanceOf[Double])
    assert(double(d1).isInstanceOf[Double])

    assert(double(t1) == d1)
    assert(double(b1) == d1)
    assert(double(s1) == d1)
    assert(double(i1) == d1)
    assert(double(l1) == d1)
    assert(double(f1) == d1)
    assert(double(d1) == d1)

    assert(double(t0) == d0)
    assert(double(b0) == d0)
    assert(double(s0) == d0)
    assert(double(i0) == d0)
    assert(double(l0) == d0)
    assert(double(f0) == d0)
    assert(double(d0) == d0)
  }

  test("Boolean functions") {
    BooleanCombinations.test { (x, y, z, w) =>
      assert(any(Vec2b(x, y)) == (x || y))
      assert(all(Vec2b(x, y)) == (x && y))
      assert(not(Vec2b(x, y)) == Vec2b(!x, !y))

      assert(any(Vec3b(x, y, z)) == (x || y || z))
      assert(all(Vec3b(x, y, z)) == (x && y && z))
      assert(not(Vec3b(x, y, z)) == Vec3b(!x, !y, !z))

      assert(any(Vec4b(x, y, z, w)) == (x || y || z || w))
      assert(all(Vec4b(x, y, z, w)) == (x && y && z && w))
      assert(not(Vec4b(x, y, z, w)) == Vec4b(!x, !y, !z, !w))
    }
  }
}
