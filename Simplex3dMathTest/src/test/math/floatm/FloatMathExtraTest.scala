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
import java.util.Random


/**
 * @author Aleksey Nikiforov (lex)
 */
class FloatMathExtraTest extends FunSuite {

    test("Vec lerp") {
        for (i <- 0 until 1000) {
            val random1 = new Random(i)
            def a = random1.nextFloat

            val random2 = new Random(i)
            def b = random2.nextFloat

            assert(lerp(a, a, a) == mix(b, b, b))

            assert(lerp(Vec2(a, a), Vec2(a, a), a) ==
                   mix(Vec2(b, b), Vec2(b, b), b))

            assert(lerp(Vec3(a, a, a), Vec3(a, a, a), a) ==
                   mix(Vec3(b, b, b), Vec3(b, b, b), b))

            assert(lerp(Vec4(a, a, a, a), Vec4(a, a, a, a), a) ==
                   mix(Vec4(b, b, b, b), Vec4(b, b, b, b), b))
        }
    }

    test("Mat lerp") {
        for (i <- 0 until 1000) {
            val random = new Random(i)
            def r = random.nextFloat

            val amount = r

            val a2x2 = Mat2x2(r, r, r, r)
            val b2x2 = Mat2x2(r, r, r, r)
            assert(lerp(a2x2, b2x2, amount) ==
                   Mat2x2(mix(a2x2(0), b2x2(0), amount),
                          mix(a2x2(1), b2x2(1), amount)))

            val a2x3 = Mat2x3(r, r, r, r, r, r)
            val b2x3 = Mat2x3(r, r, r, r, r, r)
            assert(lerp(a2x3, b2x3, amount) ==
                   Mat2x3(mix(a2x3(0), b2x3(0), amount),
                          mix(a2x3(1), b2x3(1), amount),
                          mix(a2x3(2), b2x3(2), amount)))

            val a2x4 = Mat2x4(r, r, r, r, r, r, r, r)
            val b2x4 = Mat2x4(r, r, r, r, r, r, r, r)
            assert(lerp(a2x4, b2x4, amount) ==
                   Mat2x4(mix(a2x4(0), b2x4(0), amount),
                          mix(a2x4(1), b2x4(1), amount),
                          mix(a2x4(2), b2x4(2), amount),
                          mix(a2x4(3), b2x4(3), amount)))

            val a3x2 = Mat3x2(r, r, r, r, r, r)
            val b3x2 = Mat3x2(r, r, r, r, r, r)
            assert(lerp(a3x2, b3x2, amount) ==
                   Mat3x2(mix(a3x2(0), b3x2(0), amount),
                          mix(a3x2(1), b3x2(1), amount)))

            val a3x3 = Mat3x3(r, r, r, r, r, r, r, r, r)
            val b3x3 = Mat3x3(r, r, r, r, r, r, r, r, r)
            assert(lerp(a3x3, b3x3, amount) ==
                   Mat3x3(mix(a3x3(0), b3x3(0), amount),
                          mix(a3x3(1), b3x3(1), amount),
                          mix(a3x3(2), b3x3(2), amount)))

            val a3x4 = Mat3x4(r, r, r, r, r, r, r, r, r, r, r, r)
            val b3x4 = Mat3x4(r, r, r, r, r, r, r, r, r, r, r, r)
            assert(lerp(a3x4, b3x4, amount) ==
                   Mat3x4(mix(a3x4(0), b3x4(0), amount),
                          mix(a3x4(1), b3x4(1), amount),
                          mix(a3x4(2), b3x4(2), amount),
                          mix(a3x4(3), b3x4(3), amount)))

            val a4x2 = Mat4x2(r, r, r, r, r, r, r, r)
            val b4x2 = Mat4x2(r, r, r, r, r, r, r, r)
            assert(lerp(a4x2, b4x2, amount) ==
                   Mat4x2(mix(a4x2(0), b4x2(0), amount),
                          mix(a4x2(1), b4x2(1), amount)))

            val a4x3 = Mat4x3(r, r, r, r, r, r, r, r, r, r, r, r)
            val b4x3 = Mat4x3(r, r, r, r, r, r, r, r, r, r, r, r)
            assert(lerp(a4x3, b4x3, amount) ==
                   Mat4x3(mix(a4x3(0), b4x3(0), amount),
                          mix(a4x3(1), b4x3(1), amount),
                          mix(a4x3(2), b4x3(2), amount)))

            val a4x4 = Mat4x4(r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r)
            val b4x4 = Mat4x4(r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r)
            assert(lerp(a4x4, b4x4, amount) ==
                   Mat4x4(mix(a4x4(0), b4x4(0), amount),
                          mix(a4x4(1), b4x4(1), amount),
                          mix(a4x4(2), b4x4(2), amount),
                          mix(a4x4(3), b4x4(3), amount)))
        }
    }

    test("Extra Math") {
        

//    def lengthSquare(x: Float) :Float = x*x
//    def lengthSquare(u: AnyVec2f) :Float = u.x*u.x + u.y*u.y
//    def lengthSquare(u: AnyVec3f) :Float = u.x*u.x + u.y*u.y + u.z*u.z
//    def lengthSquare(u: AnyVec4f) :Float = u.x*u.x + u.y*u.y + u.z*u.z + u.w*u.w
//
//    def hasErrors(x: Float) :Boolean = isinf(x) || isnan(x)
//    def hasErrors(u: AnyVec2f) :Boolean = u.hasErrors
//    def hasErrors(u: AnyVec3f) :Boolean = u.hasErrors
//    def hasErrors(u: AnyVec4f) :Boolean = u.hasErrors
//    def hasErrors(q: AnyQuat4f) :Boolean = q.hasErrors
//    def hasErrors(m: AnyMat2f) :Boolean = m.hasErrors
//    def hasErrors(m: AnyMat2x3f) :Boolean = m.hasErrors
//    def hasErrors(m: AnyMat2x4f) :Boolean = m.hasErrors
//    def hasErrors(m: AnyMat3x2f) :Boolean = m.hasErrors
//    def hasErrors(m: AnyMat3f) :Boolean = m.hasErrors
//    def hasErrors(m: AnyMat3x4f) :Boolean = m.hasErrors
//    def hasErrors(m: AnyMat4x2f) :Boolean = m.hasErrors
//    def hasErrors(m: AnyMat4x3f) :Boolean = m.hasErrors
//    def hasErrors(m: AnyMat4f) :Boolean = m.hasErrors
//
//    def approxEqual(x: Float, y: Float, absDelta: Float) :Boolean = {
//        abs(x - y) < absDelta
//    }
//    def approxEqual(u: AnyVec2f, v: AnyVec2f, absDelta: Float) :Boolean = {
//        abs(v.x - u.x) < absDelta &&
//        abs(v.y - u.y) < absDelta
//    }
//    def approxEqual(u: AnyVec3f, v: AnyVec3f, absDelta: Float) :Boolean = {
//        abs(v.x - u.x) < absDelta &&
//        abs(v.y - u.y) < absDelta &&
//        abs(v.z - u.z) < absDelta
//    }
//    def approxEqual(u: AnyVec4f, v: AnyVec4f, absDelta: Float) :Boolean = {
//        abs(v.x - u.x) < absDelta &&
//        abs(v.y - u.y) < absDelta &&
//        abs(v.z - u.z) < absDelta &&
//        abs(v.w - u.w) < absDelta
//    }
//    def approxEqual(p: AnyQuat4f, q: AnyQuat4f, absDelta: Float) :Boolean = {
//        abs(p.a - q.a) < absDelta &&
//        abs(p.b - q.b) < absDelta &&
//        abs(p.c - q.c) < absDelta &&
//        abs(p.d - q.d) < absDelta
//    }
//    def approxEqual(m: AnyMat2f, n: AnyMat2f, absDelta: Float) :Boolean = {
//        (
//            abs(n.m00 - m.m00) < absDelta &&
//            abs(n.m10 - m.m10) < absDelta &&
//
//            abs(n.m01 - m.m01) < absDelta &&
//            abs(n.m11 - m.m11) < absDelta
//        )
//    }
//    def approxEqual(m: AnyMat2x3f, n: AnyMat2x3f, absDelta: Float) :Boolean = {
//        (
//            abs(n.m00 - m.m00) < absDelta &&
//            abs(n.m10 - m.m10) < absDelta &&
//
//            abs(n.m01 - m.m01) < absDelta &&
//            abs(n.m11 - m.m11) < absDelta &&
//
//            abs(n.m02 - m.m02) < absDelta &&
//            abs(n.m12 - m.m12) < absDelta
//        )
//    }
//    def approxEqual(m: AnyMat2x4f, n: AnyMat2x4f, absDelta: Float) :Boolean = {
//        (
//            abs(n.m00 - m.m00) < absDelta &&
//            abs(n.m10 - m.m10) < absDelta &&
//
//            abs(n.m01 - m.m01) < absDelta &&
//            abs(n.m11 - m.m11) < absDelta &&
//
//            abs(n.m02 - m.m02) < absDelta &&
//            abs(n.m12 - m.m12) < absDelta &&
//
//            abs(n.m03 - m.m03) < absDelta &&
//            abs(n.m13 - m.m13) < absDelta
//        )
//    }
//
//    def approxEqual(m: AnyMat3x2f, n: AnyMat3x2f, absDelta: Float) :Boolean = {
//        (
//            abs(n.m00 - m.m00) < absDelta &&
//            abs(n.m10 - m.m10) < absDelta &&
//            abs(n.m20 - m.m20) < absDelta &&
//
//            abs(n.m01 - m.m01) < absDelta &&
//            abs(n.m11 - m.m11) < absDelta &&
//            abs(n.m21 - m.m21) < absDelta
//        )
//    }
//    def approxEqual(m: AnyMat3f, n: AnyMat3f, absDelta: Float) :Boolean = {
//        (
//            abs(n.m00 - m.m00) < absDelta &&
//            abs(n.m10 - m.m10) < absDelta &&
//            abs(n.m20 - m.m20) < absDelta &&
//
//            abs(n.m01 - m.m01) < absDelta &&
//            abs(n.m11 - m.m11) < absDelta &&
//            abs(n.m21 - m.m21) < absDelta &&
//
//            abs(n.m02 - m.m02) < absDelta &&
//            abs(n.m12 - m.m12) < absDelta &&
//            abs(n.m22 - m.m22) < absDelta
//        )
//    }
//    def approxEqual(m: AnyMat3x4f, n: AnyMat3x4f, absDelta: Float) :Boolean = {
//        (
//            abs(n.m00 - m.m00) < absDelta &&
//            abs(n.m10 - m.m10) < absDelta &&
//            abs(n.m20 - m.m20) < absDelta &&
//
//            abs(n.m01 - m.m01) < absDelta &&
//            abs(n.m11 - m.m11) < absDelta &&
//            abs(n.m21 - m.m21) < absDelta &&
//
//            abs(n.m02 - m.m02) < absDelta &&
//            abs(n.m12 - m.m12) < absDelta &&
//            abs(n.m22 - m.m22) < absDelta &&
//
//            abs(n.m03 - m.m03) < absDelta &&
//            abs(n.m13 - m.m13) < absDelta &&
//            abs(n.m23 - m.m23) < absDelta
//        )
//    }
//    def approxEqual(m: AnyMat4x2f, n: AnyMat4x2f, absDelta: Float) :Boolean = {
//        (
//            abs(n.m00 - m.m00) < absDelta &&
//            abs(n.m10 - m.m10) < absDelta &&
//            abs(n.m20 - m.m20) < absDelta &&
//            abs(n.m30 - m.m30) < absDelta &&
//
//            abs(n.m01 - m.m01) < absDelta &&
//            abs(n.m11 - m.m11) < absDelta &&
//            abs(n.m21 - m.m21) < absDelta &&
//            abs(n.m31 - m.m31) < absDelta
//        )
//    }
//    def approxEqual(m: AnyMat4x3f, n: AnyMat4x3f, absDelta: Float) :Boolean = {
//        (
//            abs(n.m00 - m.m00) < absDelta &&
//            abs(n.m10 - m.m10) < absDelta &&
//            abs(n.m20 - m.m20) < absDelta &&
//            abs(n.m30 - m.m30) < absDelta &&
//
//            abs(n.m01 - m.m01) < absDelta &&
//            abs(n.m11 - m.m11) < absDelta &&
//            abs(n.m21 - m.m21) < absDelta &&
//            abs(n.m31 - m.m31) < absDelta &&
//
//            abs(n.m02 - m.m02) < absDelta &&
//            abs(n.m12 - m.m12) < absDelta &&
//            abs(n.m22 - m.m22) < absDelta &&
//            abs(n.m32 - m.m32) < absDelta
//        )
//    }
//    def approxEqual(m: AnyMat4f, n: AnyMat4f, absDelta: Float) :Boolean = {
//        (
//            abs(n.m00 - m.m00) < absDelta &&
//            abs(n.m10 - m.m10) < absDelta &&
//            abs(n.m20 - m.m20) < absDelta &&
//            abs(n.m30 - m.m30) < absDelta &&
//
//            abs(n.m01 - m.m01) < absDelta &&
//            abs(n.m11 - m.m11) < absDelta &&
//            abs(n.m21 - m.m21) < absDelta &&
//            abs(n.m31 - m.m31) < absDelta &&
//
//            abs(n.m02 - m.m02) < absDelta &&
//            abs(n.m12 - m.m12) < absDelta &&
//            abs(n.m22 - m.m22) < absDelta &&
//            abs(n.m32 - m.m32) < absDelta &&
//
//            abs(n.m03 - m.m03) < absDelta &&
//            abs(n.m13 - m.m13) < absDelta &&
//            abs(n.m23 - m.m23) < absDelta &&
//            abs(n.m33 - m.m33) < absDelta
//        )
//    }
//
//    // Quaternion
//    def normSquare(q: AnyQuat4f) :Float = q.a*q.a + q.b*q.b + q.c*q.c + q.d*q.d
//    def norm(q: AnyQuat4f) :Float = {
//        sqrt(q.a*q.a + q.b*q.b + q.c*q.c + q.d*q.d)
//    }
//    def conjugate(q: AnyQuat4f) :Quat4f = new Quat4f(q.a, -q.b, -q.c, -q.d)
//
//    def normalize(q: AnyQuat4f) :Quat4f = {
//        q*inversesqrt(q.a*q.a + q.b*q.b + q.c*q.c + q.d*q.d)
//    }
//
//    /**
//     * This method is here for completness. Normally you should work with
//     * unit quaternions (<code>norm(q) == 1</code>), and in this case
//     * <code>inverse(q) == conjugate(q)</code>.
//     */
//    def inverse(q: AnyQuat4f) :Quat4f = conjugate(q)/normSquare(q)
//
//    def slerp(p: AnyQuat4f, q: AnyQuat4f, a: Float) :Quat4f = {
//        if (approxEqual(p, q, 1e-5f)) return Quat4f(q)
//
//        var cosTheta = p.a*q.a + p.b*q.b + p.c*q.c+ p.d*q.d
//        var negate = false
//        if (cosTheta < 0) {
//            cosTheta = -cosTheta
//            negate = true
//        }
//
//        var t = a
//        var s = 1 - t
//
//        if (cosTheta < 0.95f) {
//            val theta = acos(cosTheta)
//            val invSinTheta = 1/sin(theta)
//
//            t = sin(t*theta)*invSinTheta
//            s = sin(s*theta)*invSinTheta
//            if (negate) t = -t
//        }
//
//        new Quat4f(
//            s*p.a + t*q.a,
//            s*p.b + t*q.b,
//            s*p.c + t*q.c,
//            s*p.d + t*q.d
//        )
//    }
//
//    /**
//     * The result is undefined for quaternions with non-unit norm.
//     */
//    def rotate(u: AnyVec3f, q: Quat4f) = {
//        import q._
//
//        val t1 = a*b
//        val t2 = a*c
//        val t3 = a*d
//        val t4 = -b*b
//        val t5 = b*c
//        val t6 = b*d
//        val t7 = -c*c
//        val t8 = c*d
//        val t9 = -d*d
//
//        new Vec3f(
//            2*((t7 + t9)*u.x + (t5 - t3)*u.y + (t2 + t6)*u.z) + u.x,
//            2*((t3 + t5)*u.x + (t4 + t9)*u.y + (t8 - t1)*u.z) + u.y,
//            2*((t6 - t2)*u.x + (t1 + t8)*u.y + (t4 + t7)*u.z) + u.z
//        )
//    }
//
//    // Rotation
//    /**
//     * This method creates a 2d transformation matrix that rotates a vector
//     * counterclockwise by the specified angle.
//     */
//    def rotationMat(angle: Float) :Mat2f = {
//        val cosA = cos(angle)
//        val sinA = sin(angle)
//
//        new Mat2f(
//             cosA, sinA,
//            -sinA, cosA
//        )
//    }
//
//    /**
//     * The result is undefined if the matrix does not represent
//     * non-scaling rotation.
//     */
//    def rotationAngle(m: Mat2f) :Float = {
//        acos((m.m00 + m.m11)*0.5f)
//    }
//
//    def lookAt(direction: AnyVec3f, up: AnyVec3f) :Mat3f = {
//        val zaxis = normalize(direction)
//        val xaxis = normalize(cross(up, zaxis))
//        val yaxis = cross(zaxis, xaxis)
//        new Mat3f(
//            xaxis.x, xaxis.y, xaxis.z,
//            yaxis.x, yaxis.y, yaxis.z,
//            zaxis.x, zaxis.y, zaxis.z
//        )
//    }
    }

    test("Extra Inverse") {
        val m23 = Mat2x3(2, 4, 5, 3, 5, 3)
        val m23i = inverse(inverse(m23))
        assert(!hasErrors(m23i))
        assert(approxEqual(m23, m23i, 1e-6f))

        val m34 = Mat3x4(2, 4, 5, 3, 3, 6, 4, 3, 2, 6, 2, 4)
        val m34i = inverse(inverse(m34))
        assert(!hasErrors(m34i))
        assert(approxEqual(m34, m34i, 1e-6f))
    }

    test("Convert to quat") {
        def testMatrix(a: Float) {
            val m0: ConstMat3 = rotationMat(radians(a), normalize(Vec3(1, 2, 3)))
            val q: ConstQuat4 = quaternion(m0)
            val m1: ConstMat3 = rotationMat(q)

            assert(approxEqual(m0, m1, 1e-6f))
        }

        def testAngleAxis(angle: Float) {
            val angle0 = radians(angle)
            val axis0 = ConstVec3(0, 1, 0)

            val q = quaternion(angle0, axis0)

            val axis1 = Vec3(0)
            val angle1 = angleAxis(q, axis1)

            if (approxEqual(abs(angle0), 2*Pi, 1e-6f)) {
                assert(approxEqual(0, angle1, 1e-6f))
                assert(approxEqual(Vec3.UnitX, axis1, 1e-6f))
            }
            else if (approxEqual(angle0, 0, 1e-6f)) {
                assert(approxEqual(angle0, angle1, 1e-6f))
                assert(approxEqual(Vec3.UnitX, axis1, 1e-6f))
            }
            else {
                assert(
                    (
                        approxEqual(angle0, angle1, 1e-6f) &&
                        approxEqual(axis0, axis1, 1e-6f)
                    )||
                    (
                        approxEqual(angle0, -angle1, 1e-6f) &&
                        approxEqual(axis0, -axis1, 1e-6f)
                    )||
                    (
                        approxEqual(angle0, -(radians(360) - angle1), 1e-6f) &&
                        approxEqual(axis0, axis1, 1e-6f)
                    )||
                    (
                        approxEqual(angle0, (radians(360) - angle1), 1e-6f) &&
                        approxEqual(axis0, -axis1, 1e-6f)
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
        def testQuaternion(a: Float) {
            val q0: ConstQuat4 = quaternion(radians(a), normalize(Vec3(4, 5, 6)))
            val m: ConstMat3 = rotationMat(q0)
            val q1: ConstQuat4 = quaternion(m)

            assert(approxEqual(q0, q1, 1e-6f) || approxEqual(q0, -q1, 1e-6f))
        }

        def testAngleAxis(angle: Float) {
            val angle0 = radians(angle)
            val axis0 = ConstVec3(0, 0, 1)

            val m: ConstMat3 = rotationMat(angle0, axis0)

            val axis1 = Vec3(0)
            val angle1 = angleAxis(m, axis1)

            if (approxEqual(abs(angle0), 2*Pi, 1e-6f)) {
                assert(approxEqual(0, angle1, 1e-6f))
                assert(approxEqual(Vec3.UnitX, axis1, 1e-6f))
            }
            else if (approxEqual(angle0, 0, 1e-6f)) {
                assert(approxEqual(angle0, angle1, 1e-6f))
                assert(approxEqual(Vec3.UnitX, axis1, 1e-6f))
            }
            else {
                assert(
                    (
                        approxEqual(angle0, angle1, 1e-6f) &&
                        approxEqual(axis0, axis1, 1e-6f)
                    )||
                    (
                        approxEqual(angle0, -angle1, 1e-6f) &&
                        approxEqual(axis0, -axis1, 1e-6f)
                    )||
                    (
                        approxEqual(angle0, -(radians(360) - angle1), 1e-6f) &&
                        approxEqual(axis0, axis1, 1e-6f)
                    )||
                    (
                        approxEqual(angle0, (radians(360) - angle1), 1e-6f) &&
                        approxEqual(axis0, -axis1, 1e-6f)
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
        def testQuaternion(a: Float) {
            val q0: ConstQuat4 = quaternion(radians(a), normalize(Vec3(4, 5, 6)))

            val axis = Vec3(0)
            val angle = angleAxis(q0, axis)

            val q1: ConstQuat4 = quaternion(angle, axis)

            assert(approxEqual(q0, q1, 1e-6f) || approxEqual(q0, -q1, 1e-6f))
        }

        def testMatrix(a: Float) {
            val m0: ConstMat3 = rotationMat(radians(a), normalize(Vec3(1, 2, 3)))

            val axis = Vec3(0)
            val angle = angleAxis(m0, axis)

            val m1: ConstMat3 = rotationMat(angle, axis)

            assert(approxEqual(m0, m1, 1e-6f))
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
        def testMatrix(angle: Float, axis: AnyVec3) {
            val m0: ConstMat3 = rotationMat(
                radians(angle), normalize(axis)
            )
            val q: ConstQuat4 = quaternion(m0)
            val m1: ConstMat3 = rotationMat(q)

            assert(approxEqual(m0, m1, 1e-6f))
        }

        // branch 1
        testMatrix(336.7842f, Vec3(0.42147923f, -0.98776567f, -0.6945276f))

        // branch 2
        testMatrix(-210.44534f, Vec3(-0.92752934f, -0.334566f, 0.31773436f))

        // branch 3
        testMatrix(175.66881f, Vec3(0.41001987f, -0.71595466f, -0.4499402f))
        
        // branch 4
        testMatrix(-231.80054f, Vec3(-0.024970055f, 0.080794096f, -0.74685013f))
    }

    test("All if branches, angle axis from mat") {
        def testMatrix(ax: AnyVec3) {
            val m0: ConstMat3 = rotationMat(radians(180), normalize(ax))

            val axis = Vec3(0)
            val angle = angleAxis(m0, axis)

            val m1: ConstMat3 = rotationMat(angle, axis)

            assert(approxEqual(m0, m1, 1e-6f))
        }

        // sub-branch 1
        testMatrix(Vec3(0.39596772f, -0.080019474f, 0.35837686f))

        // sub-branch 2
        testMatrix(Vec3(-0.11932039f, 0.7943535f, -0.09355426f))

        // sub-branch 3
        testMatrix(Vec3(-0.19381118f, 0.30482996f, -0.4189638f))
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

        // gluPerspective(90, 640/480f, 10, 1000)
        val p1 = Mat4(
                0.75f, 0, 0, 0,
                0, 1.0f, 0, 0,
                0, 0, -1.020202f, -1.0f,
                0, 0, -20.20202f, 0.0f
        )
        val m1 = perspective(radians(90), 640/480f, 10, 1000)
        assert(approxEqual(m1, p1, 1e-7f))
        
        // gluPerspective(120, 800/600f, 10, 10000)
        val p2 = Mat4(
                0.4330127f, 0, 0, 0,
                0, 0.57735026f, 0, 0,
                0, 0, -1.002002f, -1.0f,
                0, 0, -20.02002f, 0
        )
        val m2 = perspective(radians(120), 800/600f, 10, 10000)
        assert(approxEqual(m2, p2, 1e-7f))

        // gluPerspective(100, 1680/1050f, 1, 800)
        val p3 = Mat4(
                0.52443725f, 0, 0,
                0, 0, 0.83909965f, 0,
                0, 0, 0, -1.0025032f, -1.0f,
                0, 0, -2.0025032f, 0
        )
        val m3 = perspective(radians(100), 1680/1050f, 1, 800)
        assert(approxEqual(m3, p3, 1e-7f))

        // glOrtho(-100, 100, -100, 100, -100, 100)
        val o1 = Mat4(
                0.01f, 0, 0, 0,
                0, 0.01f, 0, 0,
                0, 0, -0.01f, 0,
                0, 0, 0, 1.0f
        )
        val n1 = ortho(-100, 100, -100, 100, -100, 100)
        assert(approxEqual(n1, o1, 1e-7f))

        // gl.glOrtho(0, 300, -200, 400, -20, 500)
        val o2 = Mat4(
                0.006666667f, 0, 0, 0,
                0, 0.0033333334f, 0, 0,
                0, 0, -0.0038461538f, 0,
                -1.0f, -0.33333334f, -0.9230769f, 1.0f
        )
        val n2 = ortho(0, 300, -200, 400, -20, 500)
        assert(approxEqual(n2, o2, 1e-7f))

        // gl.glOrtho(-500, 22, -800, -222, 100, 1000)
        val o3 = Mat4(
                0.0038314175f, 0, 0, 0,
                0, 0.0034602077f, 0, 0,
                0, 0, -0.0022222223f, 0,
                0.91570884f, 1.7681661f, -1.2222222f, 1.0f
        )
        val n3 = ortho(-500, 22, -800, -222, 100, 1000)
        assert(approxEqual(n3, o3, 1e-6f))
    }
}
