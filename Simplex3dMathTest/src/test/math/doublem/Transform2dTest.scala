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

package test.math.doublem

import org.scalatest._

import simplex3d.math.doublem.renamed._
import simplex3d.math.doublem.DoubleMath._
import simplex3d.math.floatm._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Transform2dTest extends FunSuite {

  def scaleMat(v: Vec2) = {
    val m = Mat2x3(1)
    m(0, 0) = v.x
    m(1, 1) = v.y
    m
  }
  def translationMat(v: Vec2) = {
    val m = Mat2x3(1)
    m(2) = v
    m
  }

  test("2D Transform") {
    val random = new java.util.Random(1)
    def r = random.nextDouble

    // test object
    for (i <- 0 until 100) {
      val s = r
      assert(Mat2x3.scale(s) == Mat2x3.Identity.scale(s))

      val sv = Vec2(r, r)
      assert(Mat2x3.scale(sv) == Mat2x3.Identity.scale(sv))

      val a = r
      assert(Mat2x3.rotate(a) == Mat2x3.Identity.rotate(a))

      val p = Vec2(r, r)
      assert(Mat2x3.translate(p) == Mat2x3.Identity.translate(p))

      val m23 = Mat2x3(r, r, r, r, r, r)
      assert(Mat2x3.concatenate(m23) == m23)

      val m22 = Mat2x2(r, r, r, r)
      assert(Mat2x3.concatenate(m22) == Mat2x3(m22))
    }

    def assertTransform(a: ReadMat2x3, m: ReadMat2x3, b: ReadMat2x3) {
      assert(a.ne(b))
      assert(m*Mat3(a) == b)

      for (i <- 0 until 100) {
        val v = Vec2(r, r)

        {
          val t = a.transformPoint(v)
          val u = m*Vec3(t, 1)
          assert(approxEqual(u, b.transformPoint(v), 1e-14))
        }

        {
          val t = a.transformVector(v)
          val u = m*Vec3(t, 0)
          assert(approxEqual(u, b.transformVector(v), 1e-14))
        }
      }
    }
    def test(t: ReadMat2x3) {
      val s = r
      assertTransform(t, Mat2x3(s), t scale(s))

      val sv = Vec2(r, r)
      assertTransform(t, scaleMat(sv), t scale(sv))

      val a = r
      assertTransform(t, Mat2x3(rotationMat(a)), t rotate(a))

      val p = Vec2(r, r)
      assertTransform(t, translationMat(p), t translate(p))

      val m2 = ConstMat2(r, r, r, r)
      assertTransform(t, Mat2x3(m2), t concatenate(m2))

      val m2x3 = ConstMat2x3(r, r, r, r, r, r)
      assertTransform(t, m2x3, t concatenate(m2x3))
    }

    // test transform classes
    for (i <- 0 until 100) {
      test(Mat2x3(r, r, r, r, r, r))
      test(Mat2x3(Mat2x2(r, r, r, r)))
      test(Mat2x3 scale(Vec2(r, r)))
      test(Mat2x3 translate(Vec2(r, r)))
      test(Mat2x3.Identity)
    }
  }
}
