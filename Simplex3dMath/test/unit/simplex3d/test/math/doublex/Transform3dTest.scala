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

package simplex3d.test.math.doublex

import org.scalatest._

import simplex3d.math.double._
import simplex3d.math.doublex.functions._
import simplex3d.math.floatx._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Transform3dTest extends FunSuite {

  def scaleMat(v: Vec3) = {
    val m = Mat4x3(1)
    m(0, 0) = v.x
    m(1, 1) = v.y
    m(2, 2) = v.z
    m
  }
  def translationMat(v: Vec3) = {
    val m = Mat4x3(1)
    m(3) = v
    m
  }

  test("3D Transform") {
    val random = new java.util.Random(1)
    def r = random.nextDouble

    // test object
    for (i <- 0 until 100) {
      val s = r
      assert(Mat4x3.scale(s) == Mat4x3.Identity.scale(s))

      val sv = Vec3(r, r, r)
      assert(Mat4x3.scale(sv) == Mat4x3.Identity.scale(sv))

      val rq = normalize(Quat4(r, r, r, r))
      assert(Mat4x3.rotate(rq) == Mat4x3.Identity.rotate(rq))

      val rx = r
      assert(Mat4x3.rotateX(rx) == Mat4x3.Identity.rotateX(rx))

      val ry = r
      assert(Mat4x3.rotateY(ry) == Mat4x3.Identity.rotateY(ry))

      val rz = r
      assert(Mat4x3.rotateZ(rz) == Mat4x3.Identity.rotateZ(rz))

      val p = Vec3(r, r, r)
      assert(Mat4x3.translate(p) == Mat4x3.Identity.translate(p))

      val m4x3 = Mat4x3(r, r, r, r, r, r, r, r, r, r, r, r)
      assert(Mat4x3.concat(m4x3) == m4x3)

      val m3x3 = Mat3x3(r, r, r, r, r, r, r, r, r)
      assert(Mat4x3.concat(m3x3) == Mat4x3(m3x3))
    }

    def assertTransform(a: inMat4x3, m: inMat4x3, b: inMat4x3) {
      assert(a.ne(b))
      assert(approxEqual(m*Mat4(a), b, 1e-15))

      for (i <- 0 until 100) {
        val v = Vec3(r, r, r)

        {
          val t = a.transformPoint(v)
          val u = m*Vec4(t, 1)
          assert(approxEqual(u, b.transformPoint(v), 1e-14))
        }

        {
          val t = a.transformVector(v)
          val u = m*Vec4(t, 0)
          assert(approxEqual(u, b.transformVector(v), 1e-14))
        }
      }
    }
    def test(t: inMat4x3) {
      val s = r
      assertTransform(t, Mat4x3(s), t scale(s))

      val sv = Vec3(r, r, r)
      assertTransform(t, scaleMat(sv), t scale(sv))

      val rq = Quat4(r, r, r, r)
      assertTransform(t, Mat4x3(rotationMat(normalize(rq))), t rotate(rq))

      val rx = r
      assertTransform(t, Mat4x3(rotationMat(rx, Vec3.UnitX)), t rotateX(rx))

      val ry = r
      assertTransform(t, Mat4x3(rotationMat(ry, Vec3.UnitY)), t rotateY(ry))

      val rz = r
      assertTransform(t, Mat4x3(rotationMat(rz, Vec3.UnitZ)), t rotateZ(rz))

      val p = Vec3(r, r, r)
      assertTransform(t, translationMat(p), t translate(p))

      val m3 = ConstMat3(r, r, r, r, r, r, r, r, r)
      assertTransform(t, Mat4x3(m3), t concat(m3))

      val m4x3 = ConstMat4x3(r, r, r, r, r, r, r, r, r, r, r, r)
      assertTransform(t, m4x3, t concat(m4x3))
    }

    // test transform classes
    for (i <- 0 until 100) {
      test(Mat4x3(r, r, r, r, r, r, r, r, r, r, r, r))
      test(Mat4x3(Mat3x3(r, r, r, r, r, r, r, r, r)))
      test(Mat4x3 scale(Vec3(r, r, r)))
      test(Mat4x3 translate(Vec3(r, r, r)))
      test(Mat4x3.Identity)
    }


    // test applyTransformation
    def testApply(t: Mat4x3) {
      val s = r
      val ts = t.clone; ts.applyScale(s)
      assert(approxEqual(ts, t scale(s), 1e-15))

      val sv = Vec3(r, r, r)
      val tsv = t.clone; tsv.applyScale(sv)
      assert(approxEqual(tsv, t scale(sv), 1e-15))

      val rq = Quat4(r, r, r, r)
      val trq = t.clone; trq.applyRotation(rq)
      assert(approxEqual(trq, t rotate(rq), 1e-15))

      val rx = r
      val trx = t.clone; trx.applyRotationX(rx)
      assert(approxEqual(trx, t rotateX(rx), 1e-15))

      val ry = r
      val _try = t.clone; _try.applyRotationY(ry)
      assert(approxEqual(_try, t rotateY(ry), 1e-15))

      val rz = r
      val trz = t.clone; trz.applyRotationZ(rz)
      assert(approxEqual(trz, t rotateZ(rz), 1e-15))

      val p = Vec3(r, r, r)
      val tp = t.clone; tp.applyTranslation(p)
      assert(approxEqual(tp, t translate(p), 1e-15))

      val m3 = ConstMat3(r, r, r, r, r, r, r, r, r)
      val tm3 = t.clone; tm3.applyTransformation(m3)
      assert(approxEqual(tm3, t concat(m3), 1e-15))

      val m4x3 = ConstMat4x3(r, r, r, r, r, r, r, r, r, r, r, r)
      val tm4x3 = t.clone; tm4x3.applyTransformation(m4x3)
      assert(approxEqual(tm4x3, t concat(m4x3), 1e-15))

      val self = t.clone; self.applyTransformation(self)
      assert(approxEqual(self, t concat(t), 1e-15))
    }

    for (i <- 0 until 100) {
      testApply(Mat4x3(r, r, r, r, r, r, r, r, r, r, r, r))
      testApply(Mat4x3(Mat3x3(r, r, r, r, r, r, r, r, r)))
      testApply(Mat4x3 scale(Vec3(r, r, r)))
      testApply(Mat4x3 translate(Vec3(r, r, r)))
    }
  }
}
