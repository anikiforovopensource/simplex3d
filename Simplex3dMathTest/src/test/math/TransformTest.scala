/*
 * Simplex3D, Math tests
 * Copyright (C) 2009 Simplex3D team
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
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
import simplex3d.math.BaseMath._
import simplex3d.math.floatm._
import simplex3d.math.floatm.FloatMath._



/**
 * @author Aleksey Nikiforov (lex)
 */
class TransformTest extends FunSuite {

    test("Transform, InverseTransform, Translation, Rotation, and Scale factories")
    {
        val vectorsPerTransform = 100
        val randomRuns = 1000
        val vectorTolerance = 1e-5f;

        var total2 = 0
        var total3 = 0

        var badCount2 = 0
        var badCount3 = 0

        def vec2 = nextVec2
        def vec3 = nextVec3
        def vec4 = nextVec4
        def float = nextFloat
        def quat4 = normalize(Quat4(vec4))
        def axis = normalize(vec3)
        def setSeed(s: Int) { random.setSeed(s) }
        // Note: from this point on, all the float, vec2, and vec3
        // are coming from random.float, random.vec2, and random.vec3

        def testTransforms2(m: AnyMat2x3, mcheck: AnyMat2x3, invm: AnyMat2x3) {
            assert(approxEqual(m, mcheck, 1e-6f))
            assert(!approxEqual(m, invm, 1e-2f))
            assert(approxEqual(m*invm, Mat2x3(1), 1e-5f))
            
            for (i <- 0 until vectorsPerTransform) {
                total2 += 1
                val v0 = vec2
                val tv = m.transformPoint(v0)
                val v1 = invm.transformPoint(tv)
                if (!approxEqual(v0, v1, vectorTolerance)) badCount2 += 1
            }
        }
        
        def testTransforms3(m: AnyMat3x4, mcheck: AnyMat3x4, invm: AnyMat3x4) {
            assert(approxEqual(m, mcheck, 1e-6f))
            assert(!approxEqual(m, invm, 1e-2f))
            assert(approxEqual(m*invm, Mat3x4(1), 1e-6f))

            for (i <- 0 until vectorsPerTransform) {
                total3 += 1
                val v0 = vec3
                val tv = m.transformPoint(v0)
                val v1 = invm.transformPoint(tv)
                if (!approxEqual(v0, v1, vectorTolerance)) badCount3 += 1
            }
        }

        var m2 = Mat2x3(1)
        var mc2 = Mat2x3(1)
        var invm2 = Mat2x3(1)

        var m3 = Mat3x4(1)
        var mc3 = Mat3x4(1)
        var invm3 = Mat3x4(1)

        // 28 Transform.apply total

        for (s <- 0 until randomRuns) {

// 2D
// Transform(translation: AnyVec2, angle: Float, scale: AnyVec2)
setSeed(s); m2 = Transform(vec2, float, vec2)
setSeed(s); mc2 = Translation(vec2)*Rotation(float)*Scale(vec2)
setSeed(s); invm2 = InverseTransform(vec2, float, vec2)
testTransforms2(m2, mc2, invm2)

// Transform(translation: AnyVec2, rotation: AnyMat2, scale: AnyVec2)
setSeed(s); m2 = Transform(vec2, rotationMatFrom(float), vec2)
setSeed(s); mc2 = Translation(vec2)*rotationMatFrom(float)*Scale(vec2)
setSeed(s); invm2 = InverseTransform(vec2, rotationMatFrom(float), vec2)
testTransforms2(m2, mc2, invm2)

// Transform(translation: AnyVec2, angle: Float, scale: Float)
setSeed(s); m2 = Transform(vec2, float, float)
setSeed(s); mc2 = Translation(vec2)*Rotation(float)*Scale(Vec2(float))
setSeed(s); invm2 = InverseTransform(vec2, float, float)
testTransforms2(m2, mc2, invm2)

// Transform(translation: AnyVec2, rotation: AnyMat2, scale: Float)
setSeed(s); m2 = Transform(vec2, rotationMatFrom(float), float)
setSeed(s); mc2 = Translation(vec2)*rotationMatFrom(float)*Scale(Vec2(float))
setSeed(s); invm2 = InverseTransform(vec2, rotationMatFrom(float), float)
testTransforms2(m2, mc2, invm2)

// Transform(translation: AnyVec2, angle: Float)
setSeed(s); m2 = Transform(vec2, float)
setSeed(s); mc2 = Translation(vec2)*Rotation(float)
setSeed(s); invm2 = InverseTransform(vec2, float)
testTransforms2(m2, mc2, invm2)

// Transform(translation: AnyVec2, rotation: AnyMat2)
setSeed(s); m2 = Transform(vec2, rotationMatFrom(float))
setSeed(s); mc2 = Translation(vec2)*rotationMatFrom(float)
setSeed(s); invm2 = InverseTransform(vec2, rotationMatFrom(float))
testTransforms2(m2, mc2, invm2)

// Transform(translation: AnyVec2, scale: AnyVec2)
setSeed(s); m2 = Transform(vec2, vec2)
setSeed(s); mc2 = Translation(vec2)*Scale(vec2)
setSeed(s); invm2 = InverseTransform(vec2, vec2)
testTransforms2(m2, mc2, invm2)

// Transform(angle: Float, scale: AnyVec2)
setSeed(s); m2 = Transform(float, vec2)
setSeed(s); mc2 = Rotation(float)*Scale(vec2)
setSeed(s); invm2 = InverseTransform(float, vec2)
testTransforms2(m2, mc2, invm2)

// Transform(angle: Float, scale: Float)
setSeed(s); m2 = Transform(float, float)
setSeed(s); mc2 = Rotation(float)*Scale(Vec2(float))
setSeed(s); invm2 = InverseTransform(float, float)
testTransforms2(m2, mc2, invm2)

// Transform(rotation: AnyMat2, scale: AnyVec2)
setSeed(s); m2 = Transform(rotationMatFrom(float), vec2)
setSeed(s); mc2 = rotationMatFrom(float)*Scale(vec2)
setSeed(s); invm2 = InverseTransform(rotationMatFrom(float), vec2)
testTransforms2(m2, mc2, invm2)

// Transform(rotation: AnyMat2, scale: Float)
setSeed(s); m2 = Transform(rotationMatFrom(float), float)
setSeed(s); mc2 = rotationMatFrom(float)*Scale(Vec2(float))
setSeed(s); invm2 = InverseTransform(rotationMatFrom(float), float)
testTransforms2(m2, mc2, invm2)


// 3D
// Transform(translation: AnyVec3, rotation: AnyQuat4, scale: AnyVec3)
setSeed(s); m3 = Transform(vec3, quat4, vec3)
setSeed(s); mc3 = Translation(vec3)*Rotation(quat4)*Scale(vec3)
setSeed(s); invm3 = InverseTransform(vec3, quat4, vec3)
testTransforms3(m3, mc3, invm3)

// Transform(translation: AnyVec3, angle: Float, axis: AnyVec3, scale: AnyVec3)
setSeed(s); m3 = Transform(vec3, float, axis, vec3)
setSeed(s); mc3 = Translation(vec3)*Rotation(float, axis)*Scale(vec3)
setSeed(s); invm3 = InverseTransform(vec3, float, axis, vec3)
testTransforms3(m3, mc3, invm3)

// Transform(translation: AnyVec3, rotation: AnyMat3, scale: AnyVec3)
setSeed(s); m3 = Transform(vec3, rotationMatFrom(quat4), vec3)
setSeed(s); mc3 = Translation(vec3)*rotationMatFrom(quat4)*Scale(vec3)
setSeed(s); invm3 = InverseTransform(vec3, rotationMatFrom(quat4), vec3)
testTransforms3(m3, mc3, invm3)

// Transform(translation: AnyVec3, rotation: AnyQuat4, scale: Float)
setSeed(s); m3 = Transform(vec3, quat4, float)
setSeed(s); mc3 = Translation(vec3)*Rotation(quat4)*Scale(Vec3(float))
setSeed(s); invm3 = InverseTransform(vec3, quat4, float)
testTransforms3(m3, mc3, invm3)

// Transform(translation: AnyVec3, angle: Float, axis: AnyVec3, scale: Float)
setSeed(s); m3 = Transform(vec3, float, axis, float)
setSeed(s); mc3 = Translation(vec3)*Rotation(float, axis)*Scale(Vec3(float))
setSeed(s); invm3 = InverseTransform(vec3, float, axis, float)
testTransforms3(m3, mc3, invm3)

// Transform(translation: AnyVec3, rotation: AnyMat3, scale: Float)
setSeed(s); m3 = Transform(vec3, rotationMatFrom(quat4), float)
setSeed(s); mc3 = Translation(vec3)*rotationMatFrom(quat4)*Scale(Vec3(float))
setSeed(s); invm3 = InverseTransform(vec3, rotationMatFrom(quat4), float)
testTransforms3(m3, mc3, invm3)

// Transform(translation: AnyVec3, rotation: AnyQuat4)
setSeed(s); m3 = Transform(vec3, quat4)
setSeed(s); mc3 = Translation(vec3)*Rotation(quat4)
setSeed(s); invm3 = InverseTransform(vec3, quat4)
testTransforms3(m3, mc3, invm3)

// Transform(translation: AnyVec3, angle: Float, axis: AnyVec3)
setSeed(s); m3 = Transform(vec3, float, axis)
setSeed(s); mc3 = Translation(vec3)*Rotation(float, axis)
setSeed(s); invm3 = InverseTransform(vec3, float, axis)
testTransforms3(m3, mc3, invm3)

// Transform(translation: AnyVec3, rotation: AnyMat3)
setSeed(s); m3 = Transform(vec3, rotationMatFrom(quat4))
setSeed(s); mc3 = Translation(vec3)*rotationMatFrom(quat4)
setSeed(s); invm3 = InverseTransform(vec3, rotationMatFrom(quat4))
testTransforms3(m3, mc3, invm3)

// Transform(translation: AnyVec3, scale: AnyVec3)
setSeed(s); m3 = Transform(vec3, vec3)
setSeed(s); mc3 = Translation(vec3)*Scale(vec3)
setSeed(s); invm3 = InverseTransform(vec3, vec3)
testTransforms3(m3, mc3, invm3)

// Transform(translation: AnyVec3, scale: Float)
setSeed(s); m3 = Transform(vec3, float)
setSeed(s); mc3 = Translation(vec3)*Scale(Vec3(float))
setSeed(s); invm3 = InverseTransform(vec3, float)
testTransforms3(m3, mc3, invm3)

// Transform(rotation: AnyQuat4, scale: AnyVec3)
setSeed(s); m3 = Transform(quat4, vec3)
setSeed(s); mc3 = Rotation(quat4)*Scale(vec3)
setSeed(s); invm3 = InverseTransform(quat4, vec3)
testTransforms3(m3, mc3, invm3)

// Transform(angle: Float, axis: AnyVec3, scale: AnyVec3)
setSeed(s); m3 = Transform(float, axis, vec3)
setSeed(s); mc3 = Rotation(float, axis)*Scale(vec3)
setSeed(s); invm3 = InverseTransform(float, axis, vec3)
testTransforms3(m3, mc3, invm3)

// Transform(rotation: AnyMat3, scale: AnyVec3)
setSeed(s); m3 = Transform(rotationMatFrom(quat4), vec3)
setSeed(s); mc3 = rotationMatFrom(quat4)*Scale(vec3)
setSeed(s); invm3 = InverseTransform(rotationMatFrom(quat4), vec3)
testTransforms3(m3, mc3, invm3)

// Transform(rotation: AnyQuat4, scale: Float)
setSeed(s); m3 = Transform(quat4, float)
setSeed(s); mc3 = Rotation(quat4)*Scale(Vec3(float))
setSeed(s); invm3 = InverseTransform(quat4, float)
testTransforms3(m3, mc3, invm3)

// Transform(angle: Float, axis: AnyVec3, scale: Float)
setSeed(s); m3 = Transform(float, axis, float)
setSeed(s); mc3 = Rotation(float, axis)*Scale(Vec3(float))
setSeed(s); invm3 = InverseTransform(float, axis, float)
testTransforms3(m3, mc3, invm3)

// Transform(rotation: AnyMat3, scale: Float)
setSeed(s); m3 = Transform(rotationMatFrom(quat4), float)
setSeed(s); mc3 = rotationMatFrom(quat4)*Scale(Vec3(float))
setSeed(s); invm3 = InverseTransform(rotationMatFrom(quat4), float)
testTransforms3(m3, mc3, invm3)

        }

        info(
           "Large error after 2xTransform: 2d = " +
           badCount2 + " / " + total2 + ", 3d = " + badCount3 + " / " + total3
        )
    }

}
