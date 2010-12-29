/*
 * Simplex3d, MathTest package
 * Copyright (C) 2009-2010, Simplex3d Team
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

package test.math.doublex

import org.scalatest._
import java.util.Random

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.doublex.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
class DoubleMathExtraTest extends FunSuite {

  test("Mat lerp") {
    for (i <- 0 until 1000) {
      val random = new Random(i)
      def r = random.nextDouble

      val amount = r

      val a2x2 = Mat2x2(r, r, r, r)
      val b2x2 = Mat2x2(r, r, r, r)
      assert(lerp(a2x2, b2x2, amount) == Mat2x2(
          mix(a2x2(0), b2x2(0), amount),
          mix(a2x2(1), b2x2(1), amount)
        )
      )

      val a2x3 = Mat2x3(r, r, r, r, r, r)
      val b2x3 = Mat2x3(r, r, r, r, r, r)
      assert(lerp(a2x3, b2x3, amount) == Mat2x3(
          mix(a2x3(0), b2x3(0), amount),
          mix(a2x3(1), b2x3(1), amount),
          mix(a2x3(2), b2x3(2), amount)
        )
      )

      val a2x4 = Mat2x4(r, r, r, r, r, r, r, r)
      val b2x4 = Mat2x4(r, r, r, r, r, r, r, r)
      assert(lerp(a2x4, b2x4, amount) == Mat2x4(
          mix(a2x4(0), b2x4(0), amount),
          mix(a2x4(1), b2x4(1), amount),
          mix(a2x4(2), b2x4(2), amount),
          mix(a2x4(3), b2x4(3), amount)
        )
      )

      val a3x2 = Mat3x2(r, r, r, r, r, r)
      val b3x2 = Mat3x2(r, r, r, r, r, r)
      assert(lerp(a3x2, b3x2, amount) == Mat3x2(
          mix(a3x2(0), b3x2(0), amount),
          mix(a3x2(1), b3x2(1), amount)
        )
      )

      val a3x3 = Mat3x3(r, r, r, r, r, r, r, r, r)
      val b3x3 = Mat3x3(r, r, r, r, r, r, r, r, r)
      assert(lerp(a3x3, b3x3, amount) == Mat3x3(
          mix(a3x3(0), b3x3(0), amount),
          mix(a3x3(1), b3x3(1), amount),
          mix(a3x3(2), b3x3(2), amount)
        )
      )

      val a3x4 = Mat3x4(r, r, r, r, r, r, r, r, r, r, r, r)
      val b3x4 = Mat3x4(r, r, r, r, r, r, r, r, r, r, r, r)
      assert(lerp(a3x4, b3x4, amount) == Mat3x4(
          mix(a3x4(0), b3x4(0), amount),
          mix(a3x4(1), b3x4(1), amount),
          mix(a3x4(2), b3x4(2), amount),
          mix(a3x4(3), b3x4(3), amount)
        )
      )

      val a4x2 = Mat4x2(r, r, r, r, r, r, r, r)
      val b4x2 = Mat4x2(r, r, r, r, r, r, r, r)
      assert(lerp(a4x2, b4x2, amount) == Mat4x2(
          mix(a4x2(0), b4x2(0), amount),
          mix(a4x2(1), b4x2(1), amount)
        )
      )

      val a4x3 = Mat4x3(r, r, r, r, r, r, r, r, r, r, r, r)
      val b4x3 = Mat4x3(r, r, r, r, r, r, r, r, r, r, r, r)
      assert(lerp(a4x3, b4x3, amount) == Mat4x3(
          mix(a4x3(0), b4x3(0), amount),
          mix(a4x3(1), b4x3(1), amount),
          mix(a4x3(2), b4x3(2), amount)
        )
      )

      val a4x4 = Mat4x4(r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r)
      val b4x4 = Mat4x4(r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r)
      assert(lerp(a4x4, b4x4, amount) == Mat4x4(
          mix(a4x4(0), b4x4(0), amount),
          mix(a4x4(1), b4x4(1), amount),
          mix(a4x4(2), b4x4(2), amount),
          mix(a4x4(3), b4x4(3), amount)
        )
      )
    }
  }

  test("hasErrors") {
    assert(!hasErrors(0))
    assert(!hasErrors(1))
    assert(!hasErrors(-1))
    assert(hasErrors(Double.NaN))
    assert(hasErrors(Double.PositiveInfinity))
    assert(hasErrors(Double.NegativeInfinity))

    
    val random = new Random(1)
    def makeErrors(id: Int) :(Double, Double, Double, Double) = {
      val v = id%4 match {
        case 0 => random.nextDouble
        case 1 => Double.NaN
        case 2 => Double.PositiveInfinity
        case 3 => Double.NegativeInfinity
      }
      val j = (id >> 2)%4
      val seq = for (i <- 0 until 4) yield {
        if (i == j) v
        else random.nextDouble
      }
      (seq(0), seq(1), seq(2), seq(3))
    }

    var i = 0L; while(i < 16*16*16*16) {
      val (a1, a2, a3, a4) = makeErrors(toInt(i))
      val (b1, b2, b3, b4) = makeErrors(toInt(i >> 4))
      val (c1, c2, c3, c4) = makeErrors(toInt(i >> 8))
      val (d1, d2, d3, d4) = makeErrors(toInt(i >> 12))

      assert(
        hasErrors(Vec2(a1, a2)) ==
        (hasErrors(a1) || hasErrors(a2))
      )

      assert(
        hasErrors(Vec3(a1, a2, a3)) ==
        (hasErrors(a1) || hasErrors(a2) || hasErrors(a3))
      )

      assert(
        hasErrors(Vec4(a1, a2, a3, a4)) ==
        (hasErrors(a1) || hasErrors(a2) || hasErrors(a3) || hasErrors(a4))
     )

      assert(
        hasErrors(Quat4(a1, a2, a3, a4)) ==
        (hasErrors(a1) || hasErrors(a2) || hasErrors(a3) || hasErrors(a4))
      )

      val m2x2 = Mat2x2(a1, a2, b1, b2)
      assert(
        hasErrors(m2x2) ==
        (hasErrors(m2x2(0)) || hasErrors(m2x2(1)))
      )

      val m2x3 = Mat2x3(a1, a2, b1, b2, c1, c2)
      assert(
        hasErrors(m2x3) ==
        (hasErrors(m2x3(0)) || hasErrors(m2x3(1)) || hasErrors(m2x3(2)))
      )

      val m2x4 = Mat2x4(a1, a2, b1, b2, c1, c2, d1, d2)
      assert(
        hasErrors(m2x4) ==
        (
          hasErrors(m2x4(0)) || hasErrors(m2x4(1)) ||
          hasErrors(m2x4(2)) || hasErrors(m2x4(3))
        )
      )

      val m3x2 = Mat3x2(a1, a2, a3, b1, b2, b3)
      assert(
        hasErrors(m3x2) ==
        (hasErrors(m3x2(0)) || hasErrors(m3x2(1)))
      )

      val m3x3 = Mat3x3(a1, a2, a3, b1, b2, b3, c1, c2, c3)
      assert(
        hasErrors(m3x3) ==
        (hasErrors(m3x3(0)) || hasErrors(m3x3(1)) || hasErrors(m3x3(2)))
      )

      val m3x4 = Mat3x4(a1, a2, a3, b1, b2, b3, c1, c2, c3, d1, d2, d3)
      assert(
        hasErrors(m3x4) ==
        (
          hasErrors(m3x4(0)) || hasErrors(m3x4(1)) ||
          hasErrors(m3x4(2)) || hasErrors(m3x4(3))
        )
      )

      val m4x2 = Mat4x2(a1, a2, a3, a4, b1, b2, b3, b4)
      assert(
        hasErrors(m4x2) ==
        (hasErrors(m4x2(0)) || hasErrors(m4x2(1)))
      )

      val m4x3 = Mat4x3(a1, a2, a3, a4, b1, b2, b3, b4, c1, c2, c3, c4)
      assert(
        hasErrors(m4x3) ==
        (hasErrors(m4x3(0)) || hasErrors(m4x3(1)) || hasErrors(m4x3(2)))
      )

      val m4x4 = Mat4x4(
        a1, a2, a3, a4, b1, b2, b3, b4,
        c1, c2, c3, c4, d1, d2, d3, d4
      )
      assert(
        hasErrors(m4x4) ==
        (
          hasErrors(m4x4(0)) || hasErrors(m4x4(1)) ||
          hasErrors(m4x4(2)) || hasErrors(m4x4(3))
        )
      )

      i += 1
    }
  }

  test("approxEqual") {
    val d = 1d/(1024L*1024*1024*1024*1024)*16
    val e = d*2

    assert(approxEqual(1, 1+d, e))
    assert(!approxEqual(1, 1+e, e))
    assert(approxEqual(1, 1-d, e))
    assert(!approxEqual(1, 1-e, e))

    assert(approxEqual(Vec2(1, 2), Vec2(1+d, 2+d), e))
    assert(!approxEqual(Vec2(1, 2), Vec2(1+e, 2+e), e))
    assert(approxEqual(Vec2(1, 2), Vec2(1-d, 2-d), e))
    assert(!approxEqual(Vec2(1, 2), Vec2(1-e, 2-e), e))

    assert(approxEqual(Vec3(1, 2, 3), Vec3(1+d, 2+d, 3+d), e))
    assert(!approxEqual(Vec3(1, 2, 3), Vec3(1+e, 2+e, 3+e), e))
     assert(approxEqual(Vec3(1, 2, 3), Vec3(1-d, 2-d, 3-d), e))
    assert(!approxEqual(Vec3(1, 2, 3), Vec3(1-e, 2-e, 3-e), e))

    assert(approxEqual(Vec4(1, 2, 3, 4), Vec4(1+d, 2+d, 3+d, 4+d), e))
    assert(!approxEqual(Vec4(1, 2, 3, 4), Vec4(1+e, 2+e, 3+e, 4+e), e))
    assert(approxEqual(Vec4(1, 2, 3, 4), Vec4(1-d, 2-d, 3-d, 4-d), e))
    assert(!approxEqual(Vec4(1, 2, 3, 4), Vec4(1-e, 2-e, 3-e, 4-e), e))

    assert(approxEqual(Quat4(1, 2, 3, 4), Quat4(1+d, 2+d, 3+d, 4+d), e))
    assert(!approxEqual(Quat4(1, 2, 3, 4), Quat4(1+e, 2+e, 3+e, 4+e), e))
    assert(approxEqual(Quat4(1, 2, 3, 4), Quat4(1-d, 2-d, 3-d, 4-d), e))
    assert(!approxEqual(Quat4(1, 2, 3, 4), Quat4(1-e, 2-e, 3-e, 4-e), e))

    assert(approxEqual(
        Mat2x2(1, 2, 3, 4),
        Mat2x2(1+d, 2+d, 3+d, 4+d),
        e)
    )
    assert(!approxEqual(
        Mat2x2(1, 2, 3, 4),
        Mat2x2(1+e, 2+e, 3+e, 4+e),
        e)
    )
    assert(approxEqual(
        Mat2x2(1, 2, 3, 4),
        Mat2x2(1-d, 2-d, 3-d, 4-d),
        e)
    )
    assert(!approxEqual(
        Mat2x2(1, 2, 3, 4),
        Mat2x2(1-e, 2-e, 3-e, 4-e),
        e)
    )

    assert(approxEqual(
        Mat2x3(1, 2, 3, 4, 5, 6),
        Mat2x3(1+d, 2+d, 3+d, 4+d, 5+d, 6+d),
        e)
    )
    assert(!approxEqual(
        Mat2x3(1, 2, 3, 4, 5, 6),
        Mat2x3(1+e, 2+e, 3+e, 4+e, 5+e, 6+e),
        e)
    )
    assert(approxEqual(
        Mat2x3(1, 2, 3, 4, 5, 6),
        Mat2x3(1-d, 2-d, 3-d, 4-d, 5-d, 6-d),
        e)
    )
    assert(!approxEqual(
        Mat2x3(1, 2, 3, 4, 5, 6),
        Mat2x3(1-e, 2-e, 3-e, 4-e, 5-e, 6-e),
        e)
    )

    assert(approxEqual(
        Mat2x4(1, 2, 3, 4, 5, 6, 7, 8),
        Mat2x4(1+d, 2+d, 3+d, 4+d, 5+d, 6+d, 7+d, 8+d),
        e)
    )
    assert(!approxEqual(
        Mat2x4(1, 2, 3, 4, 5, 6, 7, 8),
        Mat2x4(1+e, 2+e, 3+e, 4+e, 5+e, 6+e, 7+e, 8+e),
        e)
    )
    assert(approxEqual(
        Mat2x4(1, 2, 3, 4, 5, 6, 7, 8),
        Mat2x4(1-d, 2-d, 3-d, 4-d, 5-d, 6-d, 7-d, 8-d),
        e)
    )
    assert(!approxEqual(
        Mat2x4(1, 2, 3, 4, 5, 6, 7, 8),
        Mat2x4(1-e, 2-e, 3-e, 4-e, 5-e, 6-e, 7-e, 8-e),
        e)
    )

    assert(approxEqual(
        Mat3x2(1, 2, 3, 4, 5, 6),
        Mat3x2(1+d, 2+d, 3+d, 4+d, 5+d, 6+d),
        e)
    )
    assert(!approxEqual(
        Mat3x2(1, 2, 3, 4, 5, 6),
        Mat3x2(1+e, 2+e, 3+e, 4+e, 5+e, 6+e),
        e)
    )
    assert(approxEqual(
        Mat3x2(1, 2, 3, 4, 5, 6),
        Mat3x2(1-d, 2-d, 3-d, 4-d, 5-d, 6-d),
        e)
    )
    assert(!approxEqual(
        Mat3x2(1, 2, 3, 4, 5, 6),
        Mat3x2(1-e, 2-e, 3-e, 4-e, 5-e, 6-e),
        e)
    )

    assert(approxEqual(
        Mat3x3(1, 2, 3, 4, 5, 6, 7, 8, 9),
        Mat3x3(1+d, 2+d, 3+d, 4+d, 5+d, 6+d, 7+d, 8+d, 9+d),
        e)
    )
    assert(!approxEqual(
        Mat3x3(1, 2, 3, 4, 5, 6, 7, 8, 9),
        Mat3x3(1+e, 2+e, 3+e, 4+e, 5+e, 6+e, 7+e, 8+e, 9+e),
        e)
    )
    assert(approxEqual(
        Mat3x3(1, 2, 3, 4, 5, 6, 7, 8, 9),
        Mat3x3(1-d, 2-d, 3-d, 4-d, 5-d, 6-d, 7-d, 8-d, 9-d),
        e)
    )
    assert(!approxEqual(
        Mat3x3(1, 2, 3, 4, 5, 6, 7, 8, 9),
        Mat3x3(1-e, 2-e, 3-e, 4-e, 5-e, 6-e, 7-e, 8-e, 9-e),
        e)
    )

    assert(approxEqual(
        Mat3x4(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
        Mat3x4(1+d, 2+d, 3+d, 4+d, 5+d, 6+d, 7+d, 8+d, 9+d, 10+d, 11+d, 12+d),
        e)
    )
    assert(!approxEqual(
        Mat3x4(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
        Mat3x4(1+e, 2+e, 3+e, 4+e, 5+e, 6+e, 7+e, 8+e, 9+e, 10+e, 11+e, 12+e),
        e)
    )
    assert(approxEqual(
        Mat3x4(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
        Mat3x4(1-d, 2-d, 3-d, 4-d, 5-d, 6-d, 7-d, 8-d, 9-d, 10-d, 11-d, 12-d),
        e)
    )
    assert(!approxEqual(
        Mat3x4(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
        Mat3x4(1-e, 2-e, 3-e, 4-e, 5-e, 6-e, 7-e, 8-e, 9-e, 10-e, 11-e, 12-e),
        e)
    )

    assert(approxEqual(
        Mat4x2(1, 2, 3, 4, 5, 6, 7, 8),
        Mat4x2(1+d, 2+d, 3+d, 4+d, 5+d, 6+d, 7+d, 8+d),
        e)
    )
    assert(!approxEqual(
        Mat4x2(1, 2, 3, 4, 5, 6, 7, 8),
        Mat4x2(1+e, 2+e, 3+e, 4+e, 5+e, 6+e, 7+e, 8+e),
        e)
    )
    assert(approxEqual(
        Mat4x2(1, 2, 3, 4, 5, 6, 7, 8),
        Mat4x2(1-d, 2-d, 3-d, 4-d, 5-d, 6-d, 7-d, 8-d),
        e)
    )
    assert(!approxEqual(
        Mat4x2(1, 2, 3, 4, 5, 6, 7, 8),
        Mat4x2(1-e, 2-e, 3-e, 4-e, 5-e, 6-e, 7-e, 8-e),
        e)
    )

    assert(approxEqual(
        Mat4x3(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
        Mat4x3(1+d, 2+d, 3+d, 4+d, 5+d, 6+d, 7+d, 8+d, 9+d, 10+d, 11+d, 12+d),
        e)
    )
    assert(!approxEqual(
        Mat4x3(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
        Mat4x3(1+e, 2+e, 3+e, 4+e, 5+e, 6+e, 7+e, 8+e, 9+e, 10+e, 11+e, 12+e),
        e)
    )
    assert(approxEqual(
        Mat4x3(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
        Mat4x3(1-d, 2-d, 3-d, 4-d, 5-d, 6-d, 7-d, 8-d, 9-d, 10-d, 11-d, 12-d),
        e)
    )
    assert(!approxEqual(
        Mat4x3(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
        Mat4x3(1-e, 2-e, 3-e, 4-e, 5-e, 6-e, 7-e, 8-e, 9-e, 10-e, 11-e, 12-e),
        e)
    )

    assert(approxEqual(
        Mat4(
          1, 2, 3, 4, 5, 6, 7, 8,
          9, 10, 11, 12, 13, 14, 15, 16
        ),
        Mat4(
          1+d, 2+d, 3+d, 4+d, 5+d, 6+d, 7+d, 8+d,
          9+d, 10+d, 11+d, 12+d, 13+d, 14+d, 15+d, 16+d
        ),
        e)
    )
    assert(!approxEqual(
        Mat4(
          1, 2, 3, 4, 5, 6, 7, 8,
          9, 10, 11, 12, 13, 14, 15, 16
        ),
        Mat4(
          1+e, 2+e, 3+e, 4+e, 5+e, 6+e, 7+e, 8+e,
          9+e, 10+e, 11+e, 12+e, 13+e, 14+e, 15+e, 16+e
        ),
        e)
    )
    assert(approxEqual(
        Mat4(
          1, 2, 3, 4, 5, 6, 7, 8,
          9, 10, 11, 12, 13, 14, 15, 16
        ),
        Mat4(
          1-d, 2-d, 3-d, 4-d, 5-d, 6-d, 7-d, 8-d,
          9-d, 10-d, 11-d, 12-d, 13-d, 14-d, 15-d, 16-d
        ),
        e)
    )
    assert(!approxEqual(
        Mat4(
          1, 2, 3, 4, 5, 6, 7, 8,
          9, 10, 11, 12, 13, 14, 15, 16
        ),
        Mat4(
          1-e, 2-e, 3-e, 4-e, 5-e, 6-e, 7-e, 8-e,
          9-e, 10-e, 11-e, 12-e, 13-e, 14-e, 15-e, 16-e
        ),
        e)
    )
  }

  test("Quaternion math") {
    assert(normSquare(Quat4(2, 3, 4, 5)) == 54)
    assert(approxEqual(norm(Quat4(2, 3, 4, 5)), sqrt(54), 1e-15))
    assert(conjugate(Quat4(2, 3, 4, 5)) == Quat4(2, -3, -4, -5))
    assert(normalize(Quat4(2, 3, 4, 5)) == Quat4(2, 3, 4, 5)/sqrt(54))
    assert(inverse(Quat4(2, 3, 4, 5)) == Quat4(2, -3, -4, -5)/54)

    // slerp
    val q = Quat4 rotateX(radians(90))

    assert(approxEqual(
        slerp(Quat4.Identity, q, 0),
        Quat4 rotateX(radians(0)),
        1e-15)
    )
    assert(approxEqual(
        slerp(Quat4.Identity, q, 1/3d),
        Quat4 rotateX(radians(30)),
        1e-15)
    )
    assert(approxEqual(
        slerp(Quat4.Identity, q, 0.5),
        Quat4 rotateX(radians(45)),
        1e-15)
    )
    assert(approxEqual(
        slerp(Quat4.Identity, q, 2/3d),
        Quat4 rotateX(radians(60)),
        1e-15)
    )
    assert(approxEqual(
        slerp(Quat4.Identity, q, 1),
        Quat4 rotateX(radians(90)),
        1e-15)
    )
    // slerp branches: lerp branch
    assert(approxEqual(
        slerp(Quat4.Identity, Quat4 rotateX(radians(0.1)), 0.5),
        Quat4 rotateX(radians(0.05)),
        1e-7)
    )
    // slerp branches: no negation
    assert(approxEqual(
        slerp(Quat4.Identity, Quat4 rotateX(radians(179)), 0.5),
        Quat4 rotateX(radians(179/2d)),
        1e-15)
    )
    // slerp branches: negation
    assert(approxEqual(
        slerp(Quat4.Identity, Quat4 rotateX(radians(181)), 0.5),
        Quat4 rotateX(radians(-179/2d)),
        1e-15)
    )

    // rotate
    assert(approxEqual(
        rotateVector(Vec3.UnitY, Quat4 rotateX(radians(0))),
        Vec3(0, 1, 0),
        1e-15)
    )
    assert(approxEqual(
        rotateVector(Vec3.UnitY, Quat4 rotateX(radians(30))),
        Vec3(0, sqrt(3)/2, 0.5),
        1e-15)
    )
    assert(approxEqual(
        rotateVector(Vec3.UnitY, Quat4 rotateX(radians(45))),
        normalize(Vec3(0, 1, 1)),
        1e-15)
    )
    assert(approxEqual(
        rotateVector(Vec3.UnitY, Quat4 rotateX(radians(60))),
        Vec3(0, 0.5, sqrt(3)/2),
        1e-15)
    )
    assert(approxEqual(
        rotateVector(Vec3.UnitY, Quat4 rotateX(radians(90))),
        Vec3(0, 0, 1),
        1e-15)
    )
  }

  test("Extra Inverse") {
    val m23 = Mat2x3(2, 4, 5, 3, 5, 3)
    val m23i = inverse(inverse(m23))
    assert(!hasErrors(m23i))
    assert(approxEqual(m23, m23i, 1e-15))

    val m34 = Mat3x4(2, 4, 5, 3, 3, 6, 4, 3, 2, 6, 2, 4)
    val m34i = inverse(inverse(m34))
    assert(!hasErrors(m34i))
    assert(approxEqual(m34, m34i, 1e-15))
  }

  test("2D rotation") {
    assert(approxEqual(
        rotationMat(radians(0))*Vec2(1, 0),
        Vec2(1, 0),
        1e-15)
    )
    assert(approxEqual(
        rotationMat(radians(30))*Vec2(1, 0),
        Vec2(sqrt(3)/2, 0.5),
        1e-15)
    )
    assert(approxEqual(
        rotationMat(radians(45))*Vec2(1, 0),
        normalize(Vec2(1, 1)),
        1e-15)
    )
    assert(approxEqual(
        rotationMat(radians(60))*Vec2(1, 0),
        Vec2(0.5, sqrt(3)/2),
        1e-15)
    )
    assert(approxEqual(
        rotationMat(radians(90))*Vec2(1, 0),
        Vec2(0, 1),
        1e-15)
    )

    assert(approxEqual(
        rotationAngle(rotationMat(radians(0))),
        radians(0),
        1e-15)
    )
    assert(approxEqual(
        rotationAngle(rotationMat(radians(30))),
        radians(30),
        1e-15)
    )
    assert(approxEqual(
        rotationAngle(rotationMat(radians(45))),
        radians(45),
        1e-15)
    )
    assert(approxEqual(
        rotationAngle(rotationMat(radians(60))),
        radians(60),
        1e-15)
    )
    assert(approxEqual(
        rotationAngle(rotationMat(radians(90))),
        radians(90),
        1e-15)
    )
  }

  test("Convert to quat") {
    def testMatrix(a: Double) {
      val m0: ConstMat3 = rotationMat(radians(a),normalize(Vec3(1, 2, 3)))
      val q: ConstQuat4 = quaternion(m0)
      val m1: ConstMat3 = rotationMat(q)

      assert(approxEqual(m0, m1, 1e-15))
    }

    def testAngleAxis(angle: Double) {
      val angle0 = radians(angle)
      val axis0 = ConstVec3(0, 1, 0)

      val q = quaternion(angle0, axis0)

      val axis1 = Vec3(0)
      val angle1 = angleAxis(q, axis1)

      if (approxEqual(abs(angle0), 2*Pi, 1e-15)) {
        assert(approxEqual(0, angle1, 1e-15))
        assert(approxEqual(Vec3.UnitX, axis1, 1e-15))
      }
      else if (approxEqual(angle0, 0, 1e-15)) {
        assert(approxEqual(angle0, angle1, 1e-15))
        assert(approxEqual(Vec3.UnitX, axis1, 1e-15))
      }
      else {
        assert(
          (
            approxEqual(angle0, angle1, 1e-15) &&
            approxEqual(axis0, axis1, 1e-15)
          )||
          (
            approxEqual(angle0, -angle1, 1e-15) &&
            approxEqual(axis0, -axis1, 1e-15)
          )||
          (
            approxEqual(angle0, -(radians(360) - angle1), 1e-15) &&
            approxEqual(axis0, axis1, 1e-15)
          )||
          (
            approxEqual(angle0, (radians(360) - angle1), 1e-15) &&
            approxEqual(axis0, -axis1, 1e-15)
          )
        )
      }
    }

    testMatrix(-360)
    testMatrix(-270)
    testMatrix(-180)
    testMatrix(-90)
    testMatrix(-44)
    testMatrix(0)
    testMatrix(36)
    testMatrix(90)
    testMatrix(180)
    testMatrix(270)
    testMatrix(360)

    testAngleAxis(-360)
    testAngleAxis(-270)
    testAngleAxis(-180)
    testAngleAxis(-90)
    testAngleAxis(-44)
    testAngleAxis(0)
    testAngleAxis(36)
    testAngleAxis(90)
    testAngleAxis(180)
    testAngleAxis(270)
    testAngleAxis(360)
  }

  test("Convert to matrix") {
    def testQuaternion(a: Double) {
      val q0: ConstQuat4 = quaternion(radians(a),normalize(Vec3(4, 5, 6)))
      val m: ConstMat3 = rotationMat(q0)
      val q1: ConstQuat4 = quaternion(m)

      assert(approxEqual(q0, q1, 1e-15) || approxEqual(q0, -q1, 1e-15))
    }

    def testAngleAxis(angle: Double) {
      val angle0 = radians(angle)
      val axis0 = ConstVec3(0, 0, 1)

      val m: ConstMat3 = rotationMat(angle0, axis0)

      val axis1 = Vec3(0)
      val angle1 = angleAxis(m, axis1)

      if (approxEqual(abs(angle0), 2*Pi, 1e-15)) {
        assert(approxEqual(0, angle1, 1e-15))
        assert(approxEqual(Vec3.UnitX, axis1, 1e-15))
      }
      else if (approxEqual(angle0, 0, 1e-15)) {
        assert(approxEqual(angle0, angle1, 1e-15))
        assert(approxEqual(Vec3.UnitX, axis1, 1e-15))
      }
      else {
        assert(
          (
            approxEqual(angle0, angle1, 1e-15) &&
            approxEqual(axis0, axis1, 1e-15)
          )||
          (
            approxEqual(angle0, -angle1, 1e-15) &&
            approxEqual(axis0, -axis1, 1e-15)
          )||
          (
            approxEqual(angle0, -(radians(360) - angle1), 1e-15) &&
            approxEqual(axis0, axis1, 1e-15)
          )||
          (
            approxEqual(angle0, (radians(360) - angle1), 1e-15) &&
            approxEqual(axis0, -axis1, 1e-15)
          )
        )
      }
    }

    testQuaternion(-360)
    testQuaternion(-270)
    testQuaternion(-180)
    testQuaternion(-90)
    testQuaternion(-44)
    testQuaternion(0)
    testQuaternion(36)
    testQuaternion(90)
    testQuaternion(180)
    testQuaternion(270)
    testQuaternion(360)

    testAngleAxis(-360)
    testAngleAxis(-270)
    testAngleAxis(-180)
    testAngleAxis(-90)
    testAngleAxis(-44)
    testAngleAxis(0)
    testAngleAxis(36)
    testAngleAxis(90)
    testAngleAxis(180)
    testAngleAxis(270)
    testAngleAxis(360)
  }

  test("Convert to angleAxis") {
    def testQuaternion(a: Double) {
      val q0: ConstQuat4 = quaternion(radians(a),normalize(Vec3(4, 5, 6)))

      val axis = Vec3(0)
      val angle = angleAxis(q0, axis)

      val q1: ConstQuat4 = quaternion(angle, axis)

      assert(approxEqual(q0, q1, 1e-15) || approxEqual(q0, -q1, 1e-15))
    }

    def testMatrix(a: Double) {
      val m0: ConstMat3 = rotationMat(radians(a),normalize(Vec3(1, 2, 3)))

      val axis = Vec3(0)
      val angle = angleAxis(m0, axis)

      val m1: ConstMat3 = rotationMat(angle, axis)

      assert(approxEqual(m0, m1, 1e-15))
    }

    testQuaternion(-360)
    testQuaternion(-270)
    testQuaternion(-180)
    testQuaternion(-90)
    testQuaternion(-44)
    testQuaternion(0)
    testQuaternion(36)
    testQuaternion(90)
    testQuaternion(180)
    testQuaternion(270)
    testQuaternion(360)

    testMatrix(-360)
    testMatrix(-270)
    testMatrix(-180)
    testMatrix(-90)
    testMatrix(-44)
    testMatrix(0)
    testMatrix(36)
    testMatrix(90)
    testMatrix(180)
    testMatrix(270)
    testMatrix(360)
  }

  test("All if branches, quat from mat") {
    def testMatrix(angle: Double, axis: ReadVec3) {
      val m0: ConstMat3 = rotationMat(
        radians(angle), normalize(axis)
      )
      val q: ConstQuat4 = quaternion(m0)
      val m1: ConstMat3 = rotationMat(q)

      assert(approxEqual(m0, m1, 1e-15))
    }

    // branch 1
    testMatrix(336.7842, Vec3(0.42147923, -0.98776567, -0.6945276))

    // branch 2
    testMatrix(-210.44534, Vec3(-0.92752934, -0.334566, 0.31773436))

    // branch 3
    testMatrix(175.66881, Vec3(0.41001987, -0.71595466, -0.4499402))
    
    // branch 4
    testMatrix(-231.80054, Vec3(-0.024970055, 0.080794096, -0.74685013))
  }

  test("All if branches, angle axis from mat") {
    def testMatrix(ax: ReadVec3) {
      val m0: ConstMat3 = rotationMat(radians(180), normalize(ax))

      val axis = Vec3(0)
      val angle = angleAxis(m0, axis)

      val m1: ConstMat3 = rotationMat(angle, axis)

      assert(approxEqual(m0, m1, 1e-15))
    }

    // sub-branch 1
    testMatrix(Vec3(0.39596772, -0.080019474, 0.35837686))

    // sub-branch 2
    testMatrix(Vec3(-0.11932039, 0.7943535, -0.09355426))

    // sub-branch 3
    testMatrix(Vec3(-0.19381118, 0.30482996, -0.4189638))
  }

  test("lookAt") {
    def test(x: Double, y: Double) {
      val m = rotationMat(radians(y), Vec3.UnitY)*
          rotationMat(radians(x), Vec3.UnitX)

      val dir = Vec3(0, 0, 100)
      assert(approxEqual(m, lookAt(m*dir, Vec3.UnitY), 1e-15))
    }

    test(0, 0)
    test(30, 0)
    test(30, 60)
    test(30, 90)
    test(30, 180)
    test(30, 270)
    test(30, 360)
  }

  test("Projection") {
    //gl.glMatrixMode(GL.GL_PROJECTION);
    //gl.glPushMatrix();
    //GLU g = new GLU();
    //float[] mat = new float[16];
    //
    //gl.glLoadIdentity();
    //g.gluPerspective(90, 640/480f, 10, 1000);
    //gl.glGetFloatv(GL.GL_PROJECTION_MATRIX, mat, 0);
    //System.out.println("Proj1: " + java.util.Arrays.toString(mat));
    //
    //gl.glLoadIdentity();
    //g.gluPerspective(120, 800/600f, 10, 10000);
    //gl.glGetFloatv(GL.GL_PROJECTION_MATRIX, mat, 0);
    //System.out.println("Proj2: " + java.util.Arrays.toString(mat));
    //
    //gl.glLoadIdentity();
    //g.gluPerspective(100, 1680/1050f, 1, 800);
    //gl.glGetFloatv(GL.GL_PROJECTION_MATRIX, mat, 0);
    //System.out.println("Proj3: " + java.util.Arrays.toString(mat));
    //
    //gl.glLoadIdentity();
    //gl.glOrtho(-100, 100, -100, 100, -100, 100);
    //gl.glGetFloatv(GL.GL_PROJECTION_MATRIX, mat, 0);
    //System.out.println("Proj4: " + java.util.Arrays.toString(mat));
    //
    //gl.glLoadIdentity();
    //gl.glOrtho(0, 300, -200, 400, -20, 500);
    //gl.glGetFloatv(GL.GL_PROJECTION_MATRIX, mat, 0);
    //System.out.println("Proj5: " + java.util.Arrays.toString(mat));
    //
    //gl.glLoadIdentity();
    //gl.glOrtho(-500, 22, -800, -222, 100, 1000);
    //gl.glGetFloatv(GL.GL_PROJECTION_MATRIX, mat, 0);
    //System.out.println("Proj6: " + java.util.Arrays.toString(mat));
    //
    //gl.glPopMatrix();

    def generalPerspectiveProj(
      fov: Double, aspect: Double,
      near: Double, far: Double
    ) = {
      val top = tan(fov*0.5)*near
      val bottom = -top

      val left = aspect*bottom
      val right = aspect*top

      perspectiveProj(left, right, bottom, top, near, far)
    }

    {
      // gluPerspective(90, 640/480f, 10, 1000)
      val p1 = Mat4(
          0.75, 0, 0, 0,
          0, 1.0, 0, 0,
          0, 0, -1.020202, -1.0,
          0, 0, -20.20202, 0.0
      )
      val m1 = perspectiveProj(radians(90), 640/480d, 10, 1000)
      assert(approxEqual(m1, p1, 1e-6))

      // gluPerspective(120, 800/600f, 10, 10000)
      val p2 = Mat4(
          0.4330127, 0, 0, 0,
          0, 0.57735026, 0, 0,
          0, 0, -1.002002, -1.0,
          0, 0, -20.02002, 0
      )
      val m2 = perspectiveProj(radians(120), 800/600d, 10, 10000)
      assert(approxEqual(m2, p2, 1e-7))

      // gluPerspective(100, 1680/1050f, 1, 800)
      val p3 = Mat4(
          0.52443725, 0, 0,
          0, 0, 0.83909965, 0,
          0, 0, 0, -1.0025032, -1.0,
          0, 0, -2.0025032, 0
      )
      val m3 = perspectiveProj(radians(100), 1680/1050d, 1, 800)
      assert(approxEqual(m3, p3, 1e-7))
    }

    {
      // gluPerspective(90, 640/480f, 10, 1000)
        val p1 = Mat4(
            0.75, 0, 0, 0,
            0, 1.0, 0, 0,
            0, 0, -1.020202, -1.0,
            0, 0, -20.20202, 0.0
        )
        val m1 = generalPerspectiveProj(radians(90), 640/480d, 10, 1000)
        assert(approxEqual(m1, p1, 1e-6))

        // gluPerspective(120, 800/600f, 10, 10000)
        val p2 = Mat4(
            0.4330127, 0, 0, 0,
            0, 0.57735026, 0, 0,
            0, 0, -1.002002, -1.0,
            0, 0, -20.02002, 0
        )
        val m2 = generalPerspectiveProj(radians(120), 800/600d, 10, 10000)
        assert(approxEqual(m2, p2, 1e-7))

        // gluPerspective(100, 1680/1050f, 1, 800)
        val p3 = Mat4(
            0.52443725, 0, 0,
            0, 0, 0.83909965, 0,
            0, 0, 0, -1.0025032, -1.0,
            0, 0, -2.0025032, 0
        )
        val m3 = generalPerspectiveProj(radians(100), 1680/1050d, 1, 800)
        assert(approxEqual(m3, p3, 1e-7))
    }

    {
      // glOrtho(-100, 100, -100, 100, -100, 100)
      val o1 = Mat4(
          0.01, 0, 0, 0,
          0, 0.01, 0, 0,
          0, 0, -0.01, 0,
          0, 0, 0, 1.0
      )
      val n1 = orthoProj(-100, 100, -100, 100, -100, 100)
      assert(approxEqual(n1, o1, 1e-7))

      // gl.glOrtho(0, 300, -200, 400, -20, 500)
      val o2 = Mat4(
          0.006666667, 0, 0, 0,
          0, 0.0033333334, 0, 0,
          0, 0, -0.0038461538, 0,
          -1.0, -0.33333334, -0.9230769, 1.0
      )
      val n2 = orthoProj(0, 300, -200, 400, -20, 500)
      assert(approxEqual(n2, o2, 1e-7))

      // gl.glOrtho(-500, 22, -800, -222, 100, 1000)
      val o3 = Mat4(
          0.0038314175, 0, 0, 0,
          0, 0.0034602077, 0, 0,
          0, 0, -0.0022222223, 0,
          0.91570884, 1.7681661, -1.2222222, 1.0
      )
      val n3 = orthoProj(-500, 22, -800, -222, 100, 1000)
      assert(approxEqual(n3, o3, 1e-6))
    }
  }

  test("Transformation") {
    val random = new Random(1)
    def r = random.nextDouble

    for (i <- 0 until 100) {
      {
        val s = Vec2(r, r)
        val angle = r
        val m = rotationMat(angle)
        val t = Vec2(r, r)

        assert(approxEqual(
            transformation(s, m, t),
            Mat2x3 scale(s) rotate(angle) translate(t),
            1e-13f)
        )

        assert(approxEqual(
            inverseTransformation(s, m, t),
            Mat2x3 translate(-t) rotate(-angle) scale(1/s),
            1e-13f)
        )
      }

      {
        val s = Vec3(r, r, r)
        val q = normalize(Quat4(r, r, r, r))
        val m = rotationMat(q)
        val t = Vec3(r, r, r)

        assert(approxEqual(
            transformation(s, m, t),
            Mat3x4 scale(s) rotate(q) translate(t),
            1e-12f)
        )

        assert(approxEqual(
            inverseTransformation(s, m, t),
            Mat3x4 translate(-t) rotate(conjugate(q)) scale(1/s),
            1e-12f)
        )
      }
    }
  }
}
