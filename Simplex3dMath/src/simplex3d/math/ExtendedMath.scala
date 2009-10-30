/*
 * Simplex3D, Math package
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
 *
 *
 * CLASSPATH EXCEPTION FOR UNMODIFIED WORK:
 * Linking this library statically or dynamically with other modules is making
 * a combined work based on this library. Thus, the terms and conditions of
 * the GNU General Public License cover the whole combination.
 *
 * As a special exception, the copyright holders of this library give you
 * permission to link this library with independent modules to produce
 * an executable, regardless of the license terms of these independent modules,
 * and to copy and distribute the resulting executable under terms of your
 * choice, provided that you also meet, for each linked independent module,
 * the terms and conditions of the license of that module. An independent module
 * is a module which is not derived from or based on this library. If you modify
 * this library in any way, then this exception is null and void and no longer
 * applies, in this case delete this exception statement from your version.
 */

package simplex3d.math

import VecMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
object ExtendedMath {

    private val ColorToFloat = 1/255f;

    // Cast
    def long(x: Float) :Long = x.asInstanceOf[Long]
    def long(x: Double) :Long = x.asInstanceOf[Long]

    // Lerp
    def lerp(x: Float, y: Float, a: Float) = mix(x, y, a)
    def lerp(u: AnyVec2, v: AnyVec2, a: Float) = mix(u, v, a)
    def lerp(u: AnyVec3, v: AnyVec3, a: Float) = mix(u, v, a)
    def lerp(u: AnyVec4, v: AnyVec4, a: Float) = mix(u, v, a)

    def lerp(m: Mat2, n: Mat2, a: Float) :Mat2 = {
        val b = 1 - a

        Mat2(
            b*m.m00 + a*n.m00, b*m.m10 + a*n.m10,
            b*m.m01 + a*n.m01, b*m.m11 + a*n.m11
        )
    }
    def lerp(m: Mat2x3, n: Mat2x3, a: Float) :Mat2x3 = {
        val b = 1 - a

        Mat2x3(
            b*m.m00 + a*n.m00, b*m.m10 + a*n.m10,
            b*m.m01 + a*n.m01, b*m.m11 + a*n.m11,
            b*m.m02 + a*n.m02, b*m.m12 + a*n.m12
        )
    }
    def lerp(m: Mat2x4, n: Mat2x4, a: Float) :Mat2x4 = {
        val b = 1 - a

        Mat2x4(
            b*m.m00 + a*n.m00, b*m.m10 + a*n.m10,
            b*m.m01 + a*n.m01, b*m.m11 + a*n.m11,
            b*m.m02 + a*n.m02, b*m.m12 + a*n.m12,
            b*m.m03 + a*n.m03, b*m.m13 + a*n.m13
        )
    }
    def lerp(m: Mat3x2, n: Mat3x2, a: Float) :Mat3x2 = {
        val b = 1 - a

        Mat3x2(
            b*m.m00 + a*n.m00, b*m.m10 + a*n.m10, b*m.m20 + a*n.m20,
            b*m.m01 + a*n.m01, b*m.m11 + a*n.m11, b*m.m21 + a*n.m21
        )
    }
    def lerp(m: Mat3, n: Mat3, a: Float) :Mat3 = {
        val b = 1 - a

        Mat3(
            b*m.m00 + a*n.m00, b*m.m10 + a*n.m10, b*m.m20 + a*n.m20,
            b*m.m01 + a*n.m01, b*m.m11 + a*n.m11, b*m.m21 + a*n.m21,
            b*m.m02 + a*n.m02, b*m.m12 + a*n.m12, b*m.m22 + a*n.m22
        )
    }
    def lerp(m: Mat3x4, n: Mat3x4, a: Float) :Mat3x4 = {
        val b = 1 - a

        Mat3x4(
            b*m.m00 + a*n.m00, b*m.m10 + a*n.m10, b*m.m20 + a*n.m20,
            b*m.m01 + a*n.m01, b*m.m11 + a*n.m11, b*m.m21 + a*n.m21,
            b*m.m02 + a*n.m02, b*m.m12 + a*n.m12, b*m.m22 + a*n.m22,
            b*m.m03 + a*n.m03, b*m.m13 + a*n.m13, b*m.m23 + a*n.m23
        )
    }
    def lerp(m: Mat4x2, n: Mat4x2, a: Float) :Mat4x2 = {
      val b = 1 - a

      Mat4x2(
        b*m.m00 +a*n.m00, b*m.m10 +a*n.m10, b*m.m20 +a*n.m20, b*m.m30 +a*n.m30,
        b*m.m01 +a*n.m01, b*m.m11 +a*n.m11, b*m.m21 +a*n.m21, b*m.m31 +a*n.m31
      )
    }
    def lerp(m: Mat4x3, n: Mat4x3, a: Float) :Mat4x3 = {
      val b = 1 - a

      Mat4x3(
        b*m.m00 +a*n.m00, b*m.m10 +a*n.m10, b*m.m20 +a*n.m20, b*m.m30 +a*n.m30,
        b*m.m01 +a*n.m01, b*m.m11 +a*n.m11, b*m.m21 +a*n.m21, b*m.m31 +a*n.m31,
        b*m.m02 +a*n.m02, b*m.m12 +a*n.m12, b*m.m22 +a*n.m22, b*m.m32 +a*n.m32
      )
    }
    def lerp(m: Mat4, n: Mat4, a: Float) :Mat4 = {
      val b = 1 - a

      Mat4(
        b*m.m00 +a*n.m00, b*m.m10 +a*n.m10, b*m.m20 +a*n.m20, b*m.m30 +a*n.m30,
        b*m.m01 +a*n.m01, b*m.m11 +a*n.m11, b*m.m21 +a*n.m21, b*m.m31 +a*n.m31,
        b*m.m02 +a*n.m02, b*m.m12 +a*n.m12, b*m.m22 +a*n.m22, b*m.m32 +a*n.m32,
        b*m.m03 +a*n.m03, b*m.m13 +a*n.m13, b*m.m23 +a*n.m23, b*m.m33 +a*n.m33
      )
    }

    // Extra functions
    def lengthSquare(x: Float) :Float = x*x
    def lengthSquare(u: AnyVec2) :Float = u.x*u.x + u.y*u.y
    def lengthSquare(u: AnyVec3) :Float = u.x*u.x + u.y*u.y + u.z*u.z
    def lengthSquare(u: AnyVec4) :Float = u.x*u.x + u.y*u.y + u.z*u.z + u.w*u.w

    def hasErrors(x: Float) :Boolean = isinf(x) || isnan(x)
    def hasErrors(u: AnyVec2) :Boolean = u.hasErrors
    def hasErrors(u: AnyVec3) :Boolean = u.hasErrors
    def hasErrors(u: AnyVec4) :Boolean = u.hasErrors
    def hasErrors(q: AnyQuat4) :Boolean = q.hasErrors
    def hasErrors(m: AnyMat2) :Boolean = m.hasErrors
    def hasErrors(m: AnyMat2x3) :Boolean = m.hasErrors
    def hasErrors(m: AnyMat2x4) :Boolean = m.hasErrors
    def hasErrors(m: AnyMat3x2) :Boolean = m.hasErrors
    def hasErrors(m: AnyMat3) :Boolean = m.hasErrors
    def hasErrors(m: AnyMat3x4) :Boolean = m.hasErrors
    def hasErrors(m: AnyMat4x2) :Boolean = m.hasErrors
    def hasErrors(m: AnyMat4x3) :Boolean = m.hasErrors
    def hasErrors(m: AnyMat4) :Boolean = m.hasErrors

    def approxEqual(x: Float, y: Float, absDelta: Float) :Boolean = {
        abs(x - y) < absDelta
    }
    def approxEqual(u: AnyVec2, v: AnyVec2, absDelta: Float) :Boolean = {
        abs(v.x - u.x) < absDelta &&
        abs(v.y - u.y) < absDelta
    }
    def approxEqual(u: AnyVec3, v: AnyVec3, absDelta: Float) :Boolean = {
        abs(v.x - u.x) < absDelta &&
        abs(v.y - u.y) < absDelta &&
        abs(v.z - u.z) < absDelta
    }
    def approxEqual(u: AnyVec4, v: AnyVec4, absDelta: Float) :Boolean = {
        abs(v.x - u.x) < absDelta &&
        abs(v.y - u.y) < absDelta &&
        abs(v.z - u.z) < absDelta &&
        abs(v.w - u.w) < absDelta
    }
    def approxEqual(p: AnyQuat4, q: AnyQuat4, absDelta: Float) :Boolean = {
        abs(p.a - q.a) < absDelta &&
        abs(p.b - q.b) < absDelta &&
        abs(p.c - q.c) < absDelta &&
        abs(p.d - q.d) < absDelta
    }
    def approxEqual(m: AnyMat2, n: AnyMat2, absDelta: Float) :Boolean = {
        (
            abs(n.m00 - m.m00) < absDelta &&
            abs(n.m10 - m.m10) < absDelta &&

            abs(n.m01 - m.m01) < absDelta &&
            abs(n.m11 - m.m11) < absDelta
        )
    }
    def approxEqual(m: AnyMat2x3, n: AnyMat2x3, absDelta: Float) :Boolean = {
        (
            abs(n.m00 - m.m00) < absDelta &&
            abs(n.m10 - m.m10) < absDelta &&

            abs(n.m01 - m.m01) < absDelta &&
            abs(n.m11 - m.m11) < absDelta &&

            abs(n.m02 - m.m02) < absDelta &&
            abs(n.m12 - m.m12) < absDelta
        )
    }
    def approxEqual(m: AnyMat2x4, n: AnyMat2x4, absDelta: Float) :Boolean = {
        (
            abs(n.m00 - m.m00) < absDelta &&
            abs(n.m10 - m.m10) < absDelta &&

            abs(n.m01 - m.m01) < absDelta &&
            abs(n.m11 - m.m11) < absDelta &&

            abs(n.m02 - m.m02) < absDelta &&
            abs(n.m12 - m.m12) < absDelta &&

            abs(n.m03 - m.m03) < absDelta &&
            abs(n.m13 - m.m13) < absDelta
        )
    }

    def approxEqual(m: AnyMat3x2, n: AnyMat3x2, absDelta: Float) :Boolean = {
        (
            abs(n.m00 - m.m00) < absDelta &&
            abs(n.m10 - m.m10) < absDelta &&
            abs(n.m20 - m.m20) < absDelta &&

            abs(n.m01 - m.m01) < absDelta &&
            abs(n.m11 - m.m11) < absDelta &&
            abs(n.m21 - m.m21) < absDelta
        )
    }
    def approxEqual(m: AnyMat3, n: AnyMat3, absDelta: Float) :Boolean = {
        (
            abs(n.m00 - m.m00) < absDelta &&
            abs(n.m10 - m.m10) < absDelta &&
            abs(n.m20 - m.m20) < absDelta &&

            abs(n.m01 - m.m01) < absDelta &&
            abs(n.m11 - m.m11) < absDelta &&
            abs(n.m21 - m.m21) < absDelta &&

            abs(n.m02 - m.m02) < absDelta &&
            abs(n.m12 - m.m12) < absDelta &&
            abs(n.m22 - m.m22) < absDelta
        )
    }
    def approxEqual(m: AnyMat3x4, n: AnyMat3x4, absDelta: Float) :Boolean = {
        (
            abs(n.m00 - m.m00) < absDelta &&
            abs(n.m10 - m.m10) < absDelta &&
            abs(n.m20 - m.m20) < absDelta &&

            abs(n.m01 - m.m01) < absDelta &&
            abs(n.m11 - m.m11) < absDelta &&
            abs(n.m21 - m.m21) < absDelta &&

            abs(n.m02 - m.m02) < absDelta &&
            abs(n.m12 - m.m12) < absDelta &&
            abs(n.m22 - m.m22) < absDelta &&

            abs(n.m03 - m.m03) < absDelta &&
            abs(n.m13 - m.m13) < absDelta &&
            abs(n.m23 - m.m23) < absDelta
        )
    }
    def approxEqual(m: AnyMat4x2, n: AnyMat4x2, absDelta: Float) :Boolean = {
        (
            abs(n.m00 - m.m00) < absDelta &&
            abs(n.m10 - m.m10) < absDelta &&
            abs(n.m20 - m.m20) < absDelta &&
            abs(n.m30 - m.m30) < absDelta &&

            abs(n.m01 - m.m01) < absDelta &&
            abs(n.m11 - m.m11) < absDelta &&
            abs(n.m21 - m.m21) < absDelta &&
            abs(n.m31 - m.m31) < absDelta
        )
    }
    def approxEqual(m: AnyMat4x3, n: AnyMat4x3, absDelta: Float) :Boolean = {
        (
            abs(n.m00 - m.m00) < absDelta &&
            abs(n.m10 - m.m10) < absDelta &&
            abs(n.m20 - m.m20) < absDelta &&
            abs(n.m30 - m.m30) < absDelta &&

            abs(n.m01 - m.m01) < absDelta &&
            abs(n.m11 - m.m11) < absDelta &&
            abs(n.m21 - m.m21) < absDelta &&
            abs(n.m31 - m.m31) < absDelta &&

            abs(n.m02 - m.m02) < absDelta &&
            abs(n.m12 - m.m12) < absDelta &&
            abs(n.m22 - m.m22) < absDelta &&
            abs(n.m32 - m.m32) < absDelta
        )
    }
    def approxEqual(m: AnyMat4, n: AnyMat4, absDelta: Float) :Boolean = {
        (
            abs(n.m00 - m.m00) < absDelta &&
            abs(n.m10 - m.m10) < absDelta &&
            abs(n.m20 - m.m20) < absDelta &&
            abs(n.m30 - m.m30) < absDelta &&

            abs(n.m01 - m.m01) < absDelta &&
            abs(n.m11 - m.m11) < absDelta &&
            abs(n.m21 - m.m21) < absDelta &&
            abs(n.m31 - m.m31) < absDelta &&

            abs(n.m02 - m.m02) < absDelta &&
            abs(n.m12 - m.m12) < absDelta &&
            abs(n.m22 - m.m22) < absDelta &&
            abs(n.m32 - m.m32) < absDelta &&

            abs(n.m03 - m.m03) < absDelta &&
            abs(n.m13 - m.m13) < absDelta &&
            abs(n.m23 - m.m23) < absDelta &&
            abs(n.m33 - m.m33) < absDelta
        )
    }

    // Color conversions
    // Prototype: all color conversion methods need to be optimized.
    def rgb(c: AnyVec3i) :Int = {
        val u = clamp(c, 0, 255)
        (u.r << 16) | (u.g << 8) | u.b
    }
    def rgb(c: AnyVec3) :Int = {
        rgb(Vec3i(c*255))
    }

    def argb(c: AnyVec4i) :Int = {
        val u = clamp(c, 0, 255)
        (u.a << 24) | (u.r << 16) | (u.g << 8) | u.b
    }
    def argb(c: AnyVec3i) :Int = {
        val u = clamp(c, 0, 255)
        0xFF000000 | (u.r << 16) | (u.g << 8) | u.b
    }
    def argb(c: AnyVec4) :Int = {
        argb(Vec4i(c*255))
    }
    def argb(c: AnyVec3) :Int = {
        argb(Vec3i(c*255))
    }
    
    def rgba(c: AnyVec4i) :Int = {
        val u = clamp(c, 0, 255)
        (u.r << 24) | (u.g << 16) | (u.b << 8) | u.a
    }
    def rgba(c: AnyVec3i) :Int = {
        val u = clamp(c, 0, 255)
        (u.r << 24) | (u.g << 16) | (u.b << 8) | 0x000000FF
    }
    def rgba(c: AnyVec4) :Int = {
        rgba(Vec4i(c*255))
    }
    def rgba(c: AnyVec3) :Int = {
        rgba(Vec3i(c*255))
    }

    def bgr(c: AnyVec3i) :Int = {
        val u = clamp(c, 0, 255)
        (u.b << 16) | (u.g << 8) | u.r
    }
    def bgr(c: AnyVec3) :Int = {
        bgr(Vec3i(c*255))
    }

    def abgr(c: AnyVec4i) :Int = {
        val u = clamp(c, 0, 255)
        (u.a << 24) | (u.b << 16) | (u.g << 8) | u.r
    }
    def abgr(c: AnyVec3i) :Int = {
        val u = clamp(c, 0, 255)
        0xFF000000 | (u.b << 16) | (u.g << 8) | u.r
    }
    def abgr(c: AnyVec4) :Int = {
        abgr(Vec4i(c*255))
    }
    def abgr(c: AnyVec3) :Int = {
        abgr(Vec3i(c*255))
    }

    def bgra(c: AnyVec4i) :Int = {
        val u = clamp(c, 0, 255)
        (u.b << 24) | (u.g << 16) | (u.r << 8) | u.a
    }
    def bgra(c: AnyVec3i) :Int = {
        val u = clamp(c, 0, 255)
        (u.b << 24) | (u.g << 16) | (u.r << 8) | 0x000000FF
    }
    def bgra(c: AnyVec4) :Int = {
        bgra(Vec4i(c*255))
    }
    def bgra(c: AnyVec3) :Int = {
        bgra(Vec3i(c*255))
    }

    def rgb(c: Int) :Vec3 = {
        Vec3(
            ((c & 0x00FF0000) >> 16)*ColorToFloat,
            ((c & 0x0000FF00) >> 8)*ColorToFloat,
            (c & 0x000000FF)*ColorToFloat
        )
    }

    def argb(c: Int) :Vec4 = {
        Vec4(
            ((c & 0x00FF0000) >> 16)*ColorToFloat,
            ((c & 0x0000FF00) >> 8)*ColorToFloat,
            (c & 0x000000FF)*ColorToFloat,
            ((c & 0xFF000000) >> 24)*ColorToFloat
        )
    }

    def rgba(c: Int) :Vec4 = {
        Vec4(
            ((c & 0xFF000000) >> 24)*ColorToFloat,
            ((c & 0x00FF0000) >> 16)*ColorToFloat,
            ((c & 0x0000FF00) >> 8)*ColorToFloat,
            (c & 0x000000FF)*ColorToFloat
        )
    }

    def bgr(c: Int) :Vec3 = {
        Vec3(
            (c & 0x000000FF)*ColorToFloat,
            ((c & 0x0000FF00) >> 8)*ColorToFloat,
            ((c & 0x00FF0000) >> 16)*ColorToFloat
        )
    }

    def abgr(c: Int) :Vec4 = {
        Vec4(
            (c & 0x000000FF)*ColorToFloat,
            ((c & 0x0000FF00) >> 8)*ColorToFloat,
            ((c & 0x00FF0000) >> 16)*ColorToFloat,
            ((c & 0xFF000000) >> 24)*ColorToFloat
        )
    }

    def bgra(c: Int) :Vec4 = {
        Vec4(
            ((c & 0x0000FF00) >> 8)*ColorToFloat,
            ((c & 0x00FF0000) >> 16)*ColorToFloat,
            ((c & 0xFF000000) >> 24)*ColorToFloat,
            (c & 0x000000FF)*ColorToFloat
        )
    }

    // Determinant and inverse
    def det(m: AnyMat2) :Float = m.m00*m.m11 - m.m01*m.m10

    def det(m: AnyMat3) :Float = {
        import m._

        val c0 = m11*m22 - m12*m21
        val c1 = m12*m20 - m10*m22
        val c2 = m10*m21 - m11*m20

        m00*c0 + m01*c1 + m02*c2
    }

    def det(m: AnyMat4) :Float = {
        import m._

        val fA0 = m00*m11 - m01*m10
        val fA1 = m00*m12 - m02*m10
        val fA2 = m00*m13 - m03*m10
        val fA3 = m01*m12 - m02*m11
        val fA4 = m01*m13 - m03*m11
        val fA5 = m02*m13 - m03*m12
        val fB0 = m20*m31 - m21*m30
        val fB1 = m20*m32 - m22*m30
        val fB2 = m20*m33 - m23*m30
        val fB3 = m21*m32 - m22*m31
        val fB4 = m21*m33 - m23*m31
        val fB5 = m22*m33 - m23*m32

        fA0*fB5 - fA1*fB4 + fA2*fB3 + fA3*fB2 - fA4*fB1 + fA5*fB0
    }

    /**
     * If matrix determinant is zero the result is undefined.
     */
    def inverse(m: AnyMat2) :Mat2 = {
        val detInv = 1/det(m)
        Mat2(
            m.m11*detInv, -m.m10*detInv,
            -m.m01*detInv, m.m00*detInv
          )
    }

    /**
     * This method is equivalent to casting the matrix as 3x3 then inverting it
     * and the casting the result back to 2x3.<br/>
     *
     * This is a general matrix inverse. You can invert transofrmations
     * quicker by using InvTransform(translation, rotation, scale).
     * A rotation matrix that does not scale can be inverted even faster by
     * using transpose. In the latter case you can avoid inverse alltogether
     * by using transpose multiplication:
     * instead of multiplying a matrix by a vectors (M*v),
     * you can multiply the vector by the matrix (v*M).
     *
     * <br/>If matrix determinant is zero the result is undefined.
     */
    def inverse(m: AnyMat2x3) :Mat2x3 = {
        import m._

        val det = m00*m11 - m01*m10

        val mat = Mat2x3(
            m11, -m10,
            -m01, m00,
            m01*m12 - m02*m11, m02*m10 - m00*m12
        )

        mat /= det
        mat
    }

    /**
     * This is a general matrix inverse. You can invert transofrmations
     * quicker by using InvTransform(translation, rotation, scale).
     * A rotation matrix that does not scale can be inverted even faster by
     * using transpose. In the latter case you can avoid inverse alltogether
     * by using transpose multiplication:
     * instead of multiplying a matrix by a vectors (M*v),
     * you can multiply the vector by the matrix (v*M).
     *
     * <br/>If matrix determinant is zero the result is undefined.
     */
    def inverse(m: AnyMat3) :Mat3 = {
        import m._

        val c0 = m11*m22 - m12*m21
        val c1 = m12*m20 - m10*m22
        val c2 = m10*m21 - m11*m20

        val det = m00*c0 + m01*c1 + m02*c2

        val mat = Mat3(
            c0,
            c1,
            c2,

            m02*m21 - m01*m22,
            m00*m22 - m02*m20,
            m01*m20 - m00*m21,

            m01*m12 - m02*m11,
            m02*m10 - m00*m12,
            m00*m11 - m01*m10
        )

        mat /= det
        mat
    }

    /**
     * This method is equivalent to casting the matrix as 4x4 then inverting it
     * and the casting the result back to 3x4.<br/>
     *
     * This is a general matrix inverse. You can invert transofrmations
     * quicker by using InvTransform(translation, rotation, scale).
     * A rotation matrix that does not scale can be inverted even faster by
     * using transpose. In the latter case you can avoid inverse alltogether
     * by using transpose multiplication:
     * instead of multiplying a matrix by a vectors (M*v),
     * you can multiply the vector by the matrix (v*M).
     *
     * <br/>If matrix determinant is zero the result is undefined.
     */
    def inverse(m: AnyMat3x4) :Mat3x4 = {
        import m._

        val fA0 = m00*m11 - m01*m10
        val fA1 = m00*m12 - m02*m10
        val fA2 = m00*m13 - m03*m10
        val fA3 = m01*m12 - m02*m11
        val fA4 = m01*m13 - m03*m11
        val fA5 = m02*m13 - m03*m12

        val det = fA0*m22 - fA1*m21 + fA3*m20

        val mat = Mat3x4(
             m11*m22 - m12*m21,
            -m10*m22 + m12*m20,
             m10*m21 - m11*m20,

            -m01*m22 + m02*m21,
             m00*m22 - m02*m20,
            -m00*m21 + m01*m20,

             fA3,
            -fA1,
             fA0,

            -m21*fA5 + m22*fA4 - m23*fA3,
             m20*fA5 - m22*fA2 + m23*fA1,
            -m20*fA4 + m21*fA2 - m23*fA0
        )

        mat /= det
        mat
    }

    /**
     * This is a general matrix inverse. You can invert transofrmations
     * quicker by using InvTransform(translation, rotation, scale).
     * A rotation matrix that does not scale can be inverted even faster by
     * using transpose. In the latter case you can avoid inverse alltogether
     * by using transpose multiplication:
     * instead of multiplying a matrix by a vectors (M*v),
     * you can multiply the vector by the matrix (v*M).
     *
     * <br/>If matrix determinant is zero the result is undefined.
     */
    def inverse(m: AnyMat4) :Mat4 = {
        import m._

        val fA0 = m00*m11 - m01*m10
        val fA1 = m00*m12 - m02*m10
        val fA2 = m00*m13 - m03*m10
        val fA3 = m01*m12 - m02*m11
        val fA4 = m01*m13 - m03*m11
        val fA5 = m02*m13 - m03*m12
        val fB0 = m20*m31 - m21*m30
        val fB1 = m20*m32 - m22*m30
        val fB2 = m20*m33 - m23*m30
        val fB3 = m21*m32 - m22*m31
        val fB4 = m21*m33 - m23*m31
        val fB5 = m22*m33 - m23*m32

        val det = fA0*fB5 - fA1*fB4 + fA2*fB3 + fA3*fB2 - fA4*fB1 + fA5*fB0

        val mat = Mat4(
             m11*fB5 - m12*fB4 + m13*fB3,
            -m10*fB5 + m12*fB2 - m13*fB1,
             m10*fB4 - m11*fB2 + m13*fB0,
            -m10*fB3 + m11*fB1 - m12*fB0,
            
            -m01*fB5 + m02*fB4 - m03*fB3,
             m00*fB5 - m02*fB2 + m03*fB1,
            -m00*fB4 + m01*fB2 - m03*fB0,
             m00*fB3 - m01*fB1 + m02*fB0,
            
             m31*fA5 - m32*fA4 + m33*fA3,
            -m30*fA5 + m32*fA2 - m33*fA1,
             m30*fA4 - m31*fA2 + m33*fA0,
            -m30*fA3 + m31*fA1 - m32*fA0,
            
            -m21*fA5 + m22*fA4 - m23*fA3,
             m20*fA5 - m22*fA2 + m23*fA1,
            -m20*fA4 + m21*fA2 - m23*fA0,
             m20*fA3 - m21*fA1 + m22*fA0
        )

        mat /= det
        mat
    }

    def transposeSubMat2x2(m: RotationSubMat2x2) {
        import m._

        val t10 = m10
        m10 = m01
        m01 = t10
    }

    def transposeSubMat3x3(m: RotationSubMat3x3) {
        import m._

        val t10 = m10
        val t20 = m20
        val t21 = m21
        m10 = m01
        m20 = m02
        m01 = t10
        m21 = m12
        m02 = t20
        m12 = t21
    }

    // Quaternion
    def normSquare(q: AnyQuat4) :Float = q.a*q.a + q.b*q.b + q.c*q.c + q.d*q.d
    def norm(q: AnyQuat4) :Float = {
        sqrt(q.a*q.a + q.b*q.b + q.c*q.c + q.d*q.d)
    }
    def conjugate(q: AnyQuat4) :Quat4 = Quat4(q.a, -q.b, -q.c, -q.d)

    /**
     * This method is here for completness. Normally you should work with
     * unit quaternions (<code>norm(q) == 1</code>), and in this case
     * <code>inverse(q) == conjugate(q)</code>.
     */
    def inverse(q: AnyQuat4) :Quat4 = conjugate(q)/normSquare(q)

    def slerp(p: AnyQuat4, q: AnyQuat4, a: Float) :Quat4 = {
        if (approxEqual(p, q, 1e-5f)) return Quat4(q)

        var cosTheta = p.a*q.a + p.b*q.b + p.c*q.c+ p.d*q.d
        var negate = false
        if (cosTheta < 0) {
            cosTheta = -cosTheta
            negate = true
        }

        var t = a
        var s = 1 - t

        if (cosTheta < 0.95f) {
            val theta = acos(cosTheta)
            val invSinTheta = 1/sin(theta)

            t = sin(t*theta)*invSinTheta
            s = sin(s*theta)*invSinTheta
            if (negate) t = -t
        }

        Quat4(
            s*p.a + t*q.a,
            s*p.b + t*q.b,
            s*p.c + t*q.c,
            s*p.d + t*q.d
        )
    }
    
    // Rotation
    /**
     * This method creates a 2d transformation matrix that rotates a vector
     * counterclockwise by the specified angle.
     */
    def rotationMatFrom(angle: Float) :Mat2 = {
        val m = Mat2(1)
        rotationMatFrom(angle, m)
        m
    }
    /**
     * This method creates a 2d transformation matrix that rotates a vector
     * counterclockwise by the specified angle.
     */
    def rotationMatFrom(angle: Float, result: RotationSubMat2x2) {
        val cosA = cos(angle)
        val sinA = sin(angle)

        result.set(
             cosA, sinA,
            -sinA, cosA
        )
    }

    /**
     * The result is undefined if the matrix does not represent
     * non-scaling rotation.
     */
    def rotationAngleFrom(m: ConstRotationSubMat2x2) :Float = {
        acos((m.m00 + m.m11)*0.5f)
    }

    /**
     * The result is undefined if the matrix does not represent
     * non-scaling rotation.
     */
    def quatFrom(m: ConstRotationSubMat3x3) :Quat4 = {
        val q = Quat4()
        quatFrom(m, q)
        q
    }
    /**
     * The result is undefined if the matrix does not represent
     * non-scaling rotation.
     */
    def quatFrom(m: ConstRotationSubMat3x3, result: Quat4) {
        import m._

        val trace = m00 + m11 + m22

        if (trace > 0) {
            val t = trace + 1
            val s = inversesqrt(t)*0.5f
            result.a = s*t
            result.d = (m10 - m01)*s
            result.c = (m02 - m20)*s
            result.b = (m21 - m12)*s
        }
        else if (m00 > m11 && m00 > m22) {
            val t = m00 - m11 - m22 + 1
            val s = inversesqrt(t)*0.5f
            result.b = s*t
            result.c = (m10 + m01)*s
            result.d = (m02 + m20)*s
            result.a = (m21 - m12)*s
        }
        else if (m11 > m22) {
            val t = -m00 + m11 - m22 + 1
            val s = inversesqrt(t)*0.5f
            result.c = s*t
            result.b = (m10 + m01)*s
            result.a = (m02 - m20)*s
            result.d = (m21 + m12)*s
        }
        else {
            val t = -m00 - m11 + m22 + 1
            val s = inversesqrt(t)*0.5f
            result.d = s*t
            result.a = (m10 - m01)*s
            result.b = (m02 + m20)*s
            result.c = (m21 + m12)*s
        }
    }
    
    /**
     * The result is undefined for axis with non-unit length.
     */
    def quatFrom(angle: Float, axis: AnyVec3) :Quat4 = {
        val s = sin(angle/2)
        Quat4(cos(angle/2), s*axis.x, s*axis.y, s*axis.z)
    }
    /**
     * The result is undefined for axis with non-unit length.
     */
    def quatFrom(angle: Float, axis: AnyVec3, result: Quat4) {
        val s = sin(angle/2)
        result.a = cos(angle/2)
        result.b = s*axis.x
        result.c = s*axis.y
        result.d = s*axis.z
    }

    /**
     * The result is undefined for quaternions with non-unit norm.
     */
    def rotationMatFrom(q: AnyQuat4) :Mat3 = {
        val m = Mat3(1)
        rotationMatFrom(q, m)
        m
    }
    /**
     * The result is undefined for quaternions with non-unit norm.
     */
    def rotationMatFrom(q: AnyQuat4, result: RotationSubMat3x3) {
        import q._

        val tb = 2*b*b
        val tc = 1 - 2*c*c
        val td = 2*d*d
        val bc = 2*b*c
        val da = 2*d*a
        val bd = 2*b*d
        val ca = 2*c*a
        val cd = 2*c*d
        val ba = 2*b*a

        result.set(
            tc - td, bc + da, bd - ca,
            bc - da, 1 - tb - td, cd + ba,
            bd + ca, cd - ba, tc - tb
        )
    }
    /**
     * The result is undefined for axis with non-unit length.
     */
    def rotationMatFrom(angle: Float, axis: AnyVec3) :Mat3 = {
        val m = Mat3(1)
        rotationMatFrom(angle, axis, m)
        m
    }
    /**
     * The result is undefined for axis with non-unit length.
     */
    def rotationMatFrom(angle: Float, axis: AnyVec3, result: RotationSubMat3x3)
    {
        import axis._

        val sinA = sin(angle)
        val cosA = cos(angle)
        val c = 1 - cosA
        val sx = sinA*x
        val sy = sinA*y
        val sz = sinA*z
        val temp = c*x
        val cxy = temp*y
        val cxz = temp*z
        val cyz = c*y*z
        
        result.set(
            cosA + c*x*x, cxy + sz, cxz - sy,
            cxy - sz, cosA + c*y*y, cyz + sx,
            cxz + sy, cyz - sx, cosA + c*z*z
        )
    }

    /**
     * The result is undefined for quaternions with non-unit norm.
     * If quaternion represents 0 degree rotation, then rotation
     * axis is not defined, in this case the UnitX axis is chosen.
     */
    def angleAxisFrom(q: AnyQuat4, axisResult: Vec3) :Float = {
        import q._

        if (approxEqual(abs(a), 1, 1e-6f)) {
            axisResult.set(1, 0, 0)
            return 0
        }

        val t = inversesqrt(1 - a*a)
        axisResult.x = b*t
        axisResult.y = c*t
        axisResult.z = d*t

        2*acos(a)
    }
    /**
     * The result is undefined if the matrix does not represent
     * non-scaling rotation. If matrix represents 0 degree rotation,
     * then rotation axis is not defined, in this case the UnitX axis is chosen.
     */
    def angleAxisFrom(m: ConstRotationSubMat3x3, axisResult: Vec3) :Float = {
        import m._

        val cosAngle = (m00 + m11 + m22 - 1)*0.5f

        if (approxEqual(cosAngle, 1, 1e-5f)) {
            axisResult.set(1, 0, 0)
            return 0
        }
        else if (approxEqual(cosAngle, -1, 1e-5f)) {
            if (m00 > m11 && m00 > m22) {
                val r = sqrt((m00 + 1)*0.5f)
                val t = 1/(4*r)
                axisResult.x = r
                axisResult.y = (m01 + m10)*t
                axisResult.z = (m02 + m20)*t
            }
            else if (m11 > m22) {
                val r = sqrt((m11 + 1)*0.5f)
                val t = 1/(4*r)
                axisResult.y = r
                axisResult.x = (m01 + m10)*t
                axisResult.z = (m12 + m21)*t
            }
            else {
                val r = sqrt((m22 + 1)*0.5f)
                val t = 1/(4*r)
                axisResult.z = r
                axisResult.x = (m02 + m20)*t
                axisResult.y = (m12 + m21)*t
            }
            return Pi
        }

        val t0 = (m21 - m12)
        val t1 = (m02 - m20)
        val t2 = (m10 - m01)
        val t = inversesqrt(t0*t0 + t1*t1 + t2*t2)
        axisResult.x = (m21 - m12)*t
        axisResult.y = (m02 - m20)*t
        axisResult.z = (m10 - m01)*t

        acos(cosAngle)
    }

    // Projection
    def perspective(fieldOfView: Float, aspectRatio: Float,
                    near: Float, far: Float)
    :Mat4 =
    {
        val focus = 1/tan(fieldOfView * 0.5f)
        val n_f = 1/(near - far)

        Mat4(
            focus/aspectRatio, 0, 0, 0,
            0, focus, 0, 0,
            0, 0, (near + far)*n_f, -1,
            0, 0, 2*near*far*n_f, 0
        )
    }

    def ortho(left: Float, right: Float,
              bottom: Float, top: Float,
              near: Float, far: Float)
    :Mat4 =
    {
        val r_l = 1/(right - left);
        val t_b = 1/(top - bottom);
        val f_n = 1/(far - near);

        Mat4(
            2*r_l, 0, 0, 0,
            0, 2*t_b, 0, 0,
            0, 0, -2*f_n, 0,
            -(right + left)*r_l, -(top + bottom)*t_b, -(far + near)*f_n, 1
        )
    }
}
