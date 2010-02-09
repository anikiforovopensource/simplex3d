/*
 * Simplex3d, MathTest package
 * Copyright (C) 2009-2010 Simplex3d Team
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

package test.math.floatm

import org.scalatest._

import simplex3d.math.floatm.renamed._
import simplex3d.math.floatm.FloatMath._
import simplex3d.math.doublem._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Transform2fTest extends FunSuite {

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
        def r = random.nextFloat

        // test object
        for (i <- 0 until 100) {
            val m2 = Mat2(r, r, r, r)
            assert(Transform2(m2).toMatrix() == Mat2x3(m2))
            assert(Transform2(Mat2d(m2)).toMatrix() == Mat2x3(m2))

            val m2x3 = Mat2x3(r, r, r, r, r, r)
            assert(Transform2(m2x3).toMatrix() == m2x3)
            assert(Transform2(Mat2x3d(m2x3)).toMatrix() == m2x3)
            assert(Transform2(m2x3).toMatrix().ne(m2x3))

            val s = r
            assert(Transform2.scale(s) == Transform2.Identity.scale(s))

            val sv = Vec2(r, r)
            assert(Transform2.scale(sv) == Transform2.Identity.scale(sv))

            val a = r
            assert(Transform2.rotate(a) == Transform2.Identity.rotate(a))

            val p = Vec2(r, r)
            assert(Transform2.translate(p) == Transform2.Identity.translate(p))
        }

        def assertTransform(a: Transform2, m: Mat2x3, b: Transform2) {
            assert(a.ne(b))
            assert(m*Mat3(a.toMatrix) == b.toMatrix)

            for (i <- 0 until 100) {
                val v = Vec2(r, r)

                {
                    val t = a.transformPoint(v)
                    val u = m*Vec3(t, 1)
                    assert(approxEqual(u, b.transformPoint(v), 1e-5f))
                }

                {
                    val t = a.transformVector(v)
                    val u = m*Vec3(t, 0)
                    assert(approxEqual(u, b.transformVector(v), 1e-5f))
                }
            }
        }
        def test(t: Transform2) {
            val s = r
            assertTransform(t, Mat2x3(s), t scale(s))

            val sv = Vec2(r, r)
            assertTransform(t, scaleMat(sv), t scale(sv))

            val a = r
            assertTransform(t, Mat2x3(rotationMat(a)), t rotate(a))

            val p = Vec2(r, r)
            assertTransform(t, translationMat(p), t translate(p))

            val m2 = ConstMat2(r, r, r, r)
            val m2x3 = ConstMat2x3(r, r, r, r, r, r)
            val t2 = Transform2(m2x3)
            assertTransform(t, m2x3, t concatenate(t2))
            assertTransform(t, m2x3, t concatenate(m2x3))
            assertTransform(t, Mat2x3(m2), t concatenate(m2))

            assert(approxEqual(t.invert().toMatrix, inverse(t.toMatrix), 1e-2f))

            assert(t == Transform2(t.toMatrix))
            assert(t != Transform2(t.toMatrix + Mat2x3.Identity))

            assert(t.equals(Transform2(t.toMatrix)))
            assert(!t.equals(Nil))
        }

        // test transform classes
        for (i <- 0 until 100) {
            test(Transform2(Mat2x3(r, r, r, r, r, r)))
            test(Transform2(rotationMat(r)))
            test(Transform2 scale(Vec2(r, r)))
            test(Transform2 translate(Vec2(r, r)))
            test(Transform2.Identity)
        }
    }
}
