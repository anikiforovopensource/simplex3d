/*
 * Simplex3d, MathTest package
 * Copyright (C) 2010-2011, Simplex3d Team
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

package test.math.floatx

import org.scalatest._

import simplex3d.math.float._
import simplex3d.math.floatx.functions._
import simplex3d.math.doublex._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Transform3fTest extends FunSuite {

  def scaleMat(v: Vec3) = {
    val m = Mat3x4(1)
    m(0, 0) = v.x
    m(1, 1) = v.y
    m(2, 2) = v.z
    m
  }
  def translationMat(v: Vec3) = {
    val m = Mat3x4(1)
    m(3) = v
    m
  }

  test("3D Transform") {
    val random = new java.util.Random(1)
    def r = random.nextFloat

    // test object
    for (i <- 0 until 100) {
      val s = r
      assert(Mat3x4.scale(s) == Mat3x4.Identity.scale(s))

      val sv = Vec3(r, r, r)
      assert(Mat3x4.scale(sv) == Mat3x4.Identity.scale(sv))

      val rq = normalize(Quat4(r, r, r, r))
      assert(Mat3x4.rotate(rq) == Mat3x4.Identity.rotate(rq))

      val rx = r
      assert(Mat3x4.rotateX(rx) == Mat3x4.Identity.rotateX(rx))

      val ry = r
      assert(Mat3x4.rotateY(ry) == Mat3x4.Identity.rotateY(ry))

      val rz = r
      assert(Mat3x4.rotateZ(rz) == Mat3x4.Identity.rotateZ(rz))

      val p = Vec3(r, r, r)
      assert(Mat3x4.translate(p) == Mat3x4.Identity.translate(p))

      val m34 = Mat3x4(r, r, r, r, r, r, r, r, r, r, r, r)
      assert(Mat3x4.concat(m34) == m34)

      val m33 = Mat3x3(r, r, r, r, r, r, r, r, r)
      assert(Mat3x4.concat(m33) == Mat3x4(m33))
    }

    def assertTransform(a: inMat3x4, m: inMat3x4, b: inMat3x4) {
      assert(a.ne(b))
      assert(m*Mat4(a) == b)

      for (i <- 0 until 100) {
        val v = Vec3(r, r, r)

        {
          val t = a.transformPoint(v)
          val u = m*Vec4(t, 1)
          assert(approxEqual(u, b.transformPoint(v), 1e-5f))
        }

        {
          val t = a.transformVector(v)
          val u = m*Vec4(t, 0)
          assert(approxEqual(u, b.transformVector(v), 1e-5f))
        }
      }
    }
    def test(t: inMat3x4) {
      val s = r
      assertTransform(t, Mat3x4(s), t scale(s))

      val sv = Vec3(r, r, r)
      assertTransform(t, scaleMat(sv), t scale(sv))

      val rq = Quat4(r, r, r, r)
      assertTransform(t, Mat3x4(rotationMat(normalize(rq))), t rotate(rq))

      val rx = r
      assertTransform(t, Mat3x4(rotationMat(rx, Vec3.UnitX)), t rotateX(rx))

      val ry = r
      assertTransform(t, Mat3x4(rotationMat(ry, Vec3.UnitY)), t rotateY(ry))

      val rz = r
      assertTransform(t, Mat3x4(rotationMat(rz, Vec3.UnitZ)), t rotateZ(rz))

      val p = Vec3(r, r, r)
      assertTransform(t, translationMat(p), t translate(p))

      val m3 = ConstMat3(r, r, r, r, r, r, r, r, r)
      assertTransform(t, Mat3x4(m3), t concat(m3))

      val m3x4 = ConstMat3x4(r, r, r, r, r, r, r, r, r, r, r, r)
      assertTransform(t, m3x4, t concat(m3x4))
    }

    // test transform classes
    for (i <- 0 until 100) {
      test(Mat3x4(r, r, r, r, r, r, r, r, r, r, r, r))
      test(Mat3x4(Mat3x3(r, r, r, r, r, r, r, r, r)))
      test(Mat3x4 scale(Vec3(r, r, r)))
      test(Mat3x4 translate(Vec3(r, r, r)))
      test(Mat3x4.Identity)
    }
  }
}
