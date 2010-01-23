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

package test.math.doublem

import org.scalatest._

import simplex3d.math.doublem.renamed._
import simplex3d.math.doublem.DoubleMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
class TransformdTest extends FunSuite {

    test("Uniform scale factories") {
        assert(Mat2x3(2) == Scale2(2))
        assert(Mat2x3(5) == Scale2(5))
        assert(Mat3x4(2) == Scale3(2))
        assert(Mat3x4(5) == Scale3(5))
    }

    test("2D: Transform, InverseTransform, Translation, Rotation, and Scale factories")
    {
        val vectorsPerTransform = 100
        val randomRuns = 1000
        val vectorTolerance = 1e-14;

        var total = 0
        var badCount = 0

        val random = new java.util.Random
        import random._
        def double = nextDouble
        def vec2 = Vec2(double, double)
        // Note: from this point on, all the float and vec2 are random.

        def testTransforms2(m: AnyMat2x3, mcheck: AnyMat2x3, invm: AnyMat2x3) {
            assert(approxEqual(m, mcheck, 1e-15))
            assert(!approxEqual(m, invm, 1e-11))
            assert(approxEqual(m*invm, Mat2x3(1), 1e-14))

            for (i <- 0 until vectorsPerTransform) {
                total += 1
                val v0 = vec2
                val tv = m.transformPoint(v0)
                val v1 = invm.transformPoint(tv)
                if (!approxEqual(v0, v1, vectorTolerance)) badCount += 1
            }
        }

        var m2 = Mat2x3(1)
        var mc2 = Mat2x3(1)
        var invm2 = Mat2x3(1)

        for (s <- 0 until randomRuns) {

// 2D
// Transform2(translation: AnyVec2, angle: Float, scale: AnyVec2)
setSeed(s); m2 = Transform2(vec2, double, vec2)
setSeed(s); mc2 = Translation2(vec2)*Rotation2(double)*Scale2(vec2)
setSeed(s); invm2 = InverseTransform2(vec2, double, vec2)
testTransforms2(m2, mc2, invm2)

// Transform2(translation: AnyVec2, rotation: AnyMat2, scale: AnyVec2)
setSeed(s); m2 = Transform2(vec2, rotationMatFrom(double), vec2)
setSeed(s); mc2 = Translation2(vec2)*Rotation2(double)*Scale2(vec2)
setSeed(s); invm2 = InverseTransform2(vec2, rotationMatFrom(double), vec2)
testTransforms2(m2, mc2, invm2)

// Transform2(translation: AnyVec2, angle: Float)
setSeed(s); m2 = Transform2(vec2, double)
setSeed(s); mc2 = Translation2(vec2)*Rotation2(double)
setSeed(s); invm2 = InverseTransform2(vec2, double)
testTransforms2(m2, mc2, invm2)

// Transform2(translation: AnyVec2, rotation: AnyMat2)
setSeed(s); m2 = Transform2(vec2, rotationMatFrom(double))
setSeed(s); mc2 = Translation2(vec2)*Rotation2(double)
setSeed(s); invm2 = InverseTransform2(vec2, rotationMatFrom(double))
testTransforms2(m2, mc2, invm2)

// Transform2(translation: AnyVec2, scale: AnyVec2)
setSeed(s); m2 = Transform2(vec2, vec2)
setSeed(s); mc2 = Translation2(vec2)*Scale2(vec2)
setSeed(s); invm2 = InverseTransform2(vec2, vec2)
testTransforms2(m2, mc2, invm2)

// Transform2(angle: Float, scale: AnyVec2)
setSeed(s); m2 = Transform2(double, vec2)
setSeed(s); mc2 = Rotation2(double)*Scale2(vec2)
setSeed(s); invm2 = InverseTransform2(double, vec2)
testTransforms2(m2, mc2, invm2)

// Transform2(rotation: AnyMat2, scale: AnyVec2)
setSeed(s); m2 = Transform2(rotationMatFrom(double), vec2)
setSeed(s); mc2 = Rotation2(double)*Scale2(vec2)
setSeed(s); invm2 = InverseTransform2(rotationMatFrom(double), vec2)
testTransforms2(m2, mc2, invm2)

        }

        info(
           "Large error after 2xTransform: 2d = " +
           badCount + " / " + total + "."
        )
    }
    
    test("3D: Transform, InverseTransform, Translation, Rotation, and Scale factories")
    {
        val vectorsPerTransform = 100
        val randomRuns = 1000
        val vectorTolerance = 1e-14;

        var total = 0
        var badCount = 0

        val random = new java.util.Random
        import random._
        def double = nextDouble
        def vec3 = Vec3(double, double, double)
        def quat4 = normalize(Quat4(double, double, double, double))
        def axis = normalize(vec3)
        // Note: from this point on, all the float, vec3, quat4, and axis
        // are random.
        
        def testTransforms3(m: AnyMat3x4, mcheck: AnyMat3x4, invm: AnyMat3x4) {
            assert(approxEqual(m, mcheck, 1e-15))
            assert(!approxEqual(m, invm, 1e-11))
            assert(approxEqual(m*invm, Mat3x4(1), 1e-14))

            for (i <- 0 until vectorsPerTransform) {
                total += 1
                val v0 = vec3
                val tv = m.transformPoint(v0)
                val v1 = invm.transformPoint(tv)
                if (!approxEqual(v0, v1, vectorTolerance)) badCount += 1
            }
        }

        var m3 = Mat3x4(1)
        var mc3 = Mat3x4(1)
        var invm3 = Mat3x4(1)

        for (s <- 0 until randomRuns) {

// 3D
// Transform3(translation: AnyVec3, rotation: AnyQuat4, scale: AnyVec3)
setSeed(s); m3 = Transform3(vec3, quat4, vec3)
setSeed(s); mc3 = Translation3(vec3)*Rotation3(quat4)*Scale3(vec3)
setSeed(s); invm3 = InverseTransform3(vec3, quat4, vec3)
testTransforms3(m3, mc3, invm3)

// Transform3(translation: AnyVec3, angle: Float, axis: AnyVec3, scale: AnyVec3)
setSeed(s); m3 = Transform3(vec3, double, axis, vec3)
setSeed(s); mc3 = Translation3(vec3)*Rotation3(double, axis)*Scale3(vec3)
setSeed(s); invm3 = InverseTransform3(vec3, double, axis, vec3)
testTransforms3(m3, mc3, invm3)

// Transform3(translation: AnyVec3, rotation: AnyMat3, scale: AnyVec3)
setSeed(s); m3 = Transform3(vec3, rotationMatFrom(quat4), vec3)
setSeed(s); mc3 = Translation3(vec3)*Rotation3(quat4)*Scale3(vec3)
setSeed(s); invm3 = InverseTransform3(vec3, rotationMatFrom(quat4), vec3)
testTransforms3(m3, mc3, invm3)

// Transform3(translation: AnyVec3, rotation: AnyQuat4)
setSeed(s); m3 = Transform3(vec3, quat4)
setSeed(s); mc3 = Translation3(vec3)*Rotation3(quat4)
setSeed(s); invm3 = InverseTransform3(vec3, quat4)
testTransforms3(m3, mc3, invm3)

// Transform3(translation: AnyVec3, angle: Float, axis: AnyVec3)
setSeed(s); m3 = Transform3(vec3, double, axis)
setSeed(s); mc3 = Translation3(vec3)*Rotation3(double, axis)
setSeed(s); invm3 = InverseTransform3(vec3, double, axis)
testTransforms3(m3, mc3, invm3)

// Transform3(translation: AnyVec3, rotation: AnyMat3)
setSeed(s); m3 = Transform3(vec3, rotationMatFrom(quat4))
setSeed(s); mc3 = Translation3(vec3)*Rotation3(quat4)
setSeed(s); invm3 = InverseTransform3(vec3, rotationMatFrom(quat4))
testTransforms3(m3, mc3, invm3)

// Transform3(translation: AnyVec3, scale: AnyVec3)
setSeed(s); m3 = Transform3(vec3, vec3)
setSeed(s); mc3 = Translation3(vec3)*Scale3(vec3)
setSeed(s); invm3 = InverseTransform3(vec3, vec3)
testTransforms3(m3, mc3, invm3)

// Transform3(rotation: AnyQuat4, scale: AnyVec3)
setSeed(s); m3 = Transform3(quat4, vec3)
setSeed(s); mc3 = Rotation3(quat4)*Scale3(vec3)
setSeed(s); invm3 = InverseTransform3(quat4, vec3)
testTransforms3(m3, mc3, invm3)

// Transform3(angle: Float, axis: AnyVec3, scale: AnyVec3)
setSeed(s); m3 = Transform3(double, axis, vec3)
setSeed(s); mc3 = Rotation3(double, axis)*Scale3(vec3)
setSeed(s); invm3 = InverseTransform3(double, axis, vec3)
testTransforms3(m3, mc3, invm3)

// Transform3(rotation: AnyMat3, scale: AnyVec3)
setSeed(s); m3 = Transform3(rotationMatFrom(quat4), vec3)
setSeed(s); mc3 = Rotation3(quat4)*Scale3(vec3)
setSeed(s); invm3 = InverseTransform3(rotationMatFrom(quat4), vec3)
testTransforms3(m3, mc3, invm3)

        }

        info(
           "Large error after 2xTransform: 3d = " +
           badCount + " / " + total + "."
        )
    }
}
