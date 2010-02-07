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


/**
 * @author Aleksey Nikiforov (lex)
 */
class TransformfTest extends FunSuite {

    test("2D: Transform chain") {
        val x = 1.5f
        val y = 2.4f
        val u = ConstVec2(x, y)

        var t = Transform2()
        assert(approxEqual(t.transformPoint(u), Vec2(x, y), 1e-6f))

        t = Transform2() scale(Vec2(2, 3))
        assert(approxEqual(t.transformPoint(u), Vec2(2*x, 3*y), 1e-6f))

        t = Transform2() rotate(radians(90))
        assert(approxEqual(t.transformPoint(u), Vec2(-y, x), 1e-6f))

        t = Transform2() concatenate(rotationMat(radians(90)))
        assert(approxEqual(t.transformPoint(u), Vec2(-y, x), 1e-6f))

        t = Transform2() translate(Vec2(1, 2))
        assert(approxEqual(t.transformPoint(u), Vec2(x + 1, y + 2), 1e-6f))

        t = Transform2() scale(Vec2(2, 3)) rotate(radians(90))
        assert(approxEqual(t.transformPoint(u), Vec2(-3*y, 2*x), 1e-6f))

        t = Transform2() scale(Vec2(2, 3)) translate(Vec2(1, 2))
        assert(approxEqual(t.transformPoint(u), Vec2(2*x + 1, 3*y + 2), 1e-6f))

        t = Transform2() rotate(radians(90)) translate(Vec2(1, 2))
        assert(approxEqual(t.transformPoint(u), Vec2(-y + 1, x + 2), 1e-6f))

        t = Transform2() scale(Vec2(2, 3)) rotate(radians(90)) translate(Vec2(1, 2))
        assert(approxEqual(t.transformPoint(u), Vec2(-3*y + 1, 2*x + 2), 1e-6f))
    }

    test("2D: Transform and InverseTransform")
    {
        val vectorsPerTransform = 100
        val randomRuns = 1000
        val vectorTolerance = 1e-5f;

        var total = 0
        var badCount = 0

        val random = new java.util.Random
        import random._
        def float = nextFloat
        def vec2 = Vec2(float, float)
        // Note: from this point on, all the float and vec2 are random.

        def testTransforms2(m: Transform2, mcheck: Transform2, invm: Transform2) {
            assert(approxEqual(m.matrix, mcheck.matrix, 1e-6f))
            assert(!approxEqual(m.matrix, invm.matrix, 1e-2f))
            assert(approxEqual(Mat3(m.matrix)*Mat3(invm.matrix), Mat3.Identity, 1e-5f))

            for (i <- 0 until vectorsPerTransform) {
                total += 1
                val v0 = vec2
                val tv = m.transformPoint(v0)
                val v1 = invm.transformPoint(tv)
                if (!approxEqual(v0, v1, vectorTolerance)) badCount += 1
            }
        }

        var t: Transform2 = null
        var tc: Transform2 = null
        var invt: Transform2 = null

        for (s <- 0 until randomRuns) {

        // Transform2(scale: Vec2, rotation: Mat2, translation: Vec2)
        setSeed(s); t = Transform2(vec2, rotationMat(float), vec2)
        setSeed(s); tc = Transform2() scale(vec2) concatenate(rotationMat(float)) translate(vec2)
        setSeed(s); invt = Transform2.inverse(vec2, rotationMat(float), vec2)
        testTransforms2(t, tc, invt)

        }

        info(
           "Large error after 2xTransform: 2d = " +
           badCount + " / " + total + "."
        )
    }
    
    test("3D: Transform and InverseTransform")
    {
        val vectorsPerTransform = 100
        val randomRuns = 1000
        val vectorTolerance = 1e-5f;

        var total = 0
        var badCount = 0

        val random = new java.util.Random
        import random._
        def float = nextFloat
        def vec3 = Vec3(float, float, float)
        def quat4 = normalize(Quat4(float, float, float, float))
        def axis = normalize(vec3)
        // Note: from this point on, all the float, vec3, quat4, and axis
        // are random.

        def testTransforms3(m: Transform3, mcheck: Transform3, invm: Transform3) {
            assert(approxEqual(m.toMatrix, mcheck.toMatrix, 1e-6f))
            assert(!approxEqual(m.toMatrix, invm.toMatrix, 1e-2f))
            assert(approxEqual(Mat4(m.toMatrix)*Mat4(invm.toMatrix), Mat4.Identity, 1e-5f))

            for (i <- 0 until vectorsPerTransform) {
                total += 1
                val v0 = vec3
                val tv = m.transformPoint(v0)
                val v1 = invm.transformPoint(tv)
                if (!approxEqual(v0, v1, vectorTolerance)) badCount += 1
            }
        }

        var t: Transform3 = null
        var tc: Transform3 = null
        var invt: Transform3 = null

        for (s <- 0 until randomRuns) {

        // Transform3(scale: Vec3, rotation: Mat3, translation: Vec3)
        setSeed(s); t =  Transform3 scale(vec3) rotate(float, axis) translate(vec3)
        setSeed(s); tc = Transform3 scale(vec3) concatenate(rotationMat(float, axis)) translate(vec3)
        setSeed(s); invt = inverseTransform(vec3, rotationMat(float, axis), vec3)
        testTransforms3(t, tc, invt)

        }

        info(
           "Large error after 2xTransform: 3d = " +
           badCount + " / " + total + "."
        )
    }
}
