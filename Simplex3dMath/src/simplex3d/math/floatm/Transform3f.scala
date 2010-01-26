/*
 * Simplex3d, FloatMath module
 * Copyright (C) 2009-2010 Simplex3d Team
 *
 * This file is part of Simplex3dMath.
 *
 * Simplex3dMath is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dMath is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.math.floatm

import FloatMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Transform3f private[math] (val matrix: Mat3x4f) {
    import matrix._

    def scale(s: Float) :Transform3f = {
        matrix *= s
        this
    }
    def scale(s: AnyVec3f) :Transform3f = {
        m00 *= s.x; m10 *= s.y; m20 *= s.z
        m01 *= s.x; m11 *= s.y; m21 *= s.z
        m02 *= s.x; m12 *= s.y; m22 *= s.z
        m03 *= s.x; m13 *= s.y; m23 *= s.z

        this
    }

    def rotate(q: AnyQuat4f) :Transform3f = {
        transform(rotationMat(q))
    }
    def rotate(angle: Float, axis: AnyVec3f) :Transform3f = {
        transform(rotationMat(angle, axis))
    }

    def rotateX(angle: Float) :Transform3f = {
        transform(rotationMat(angle, Vec3f.UnitX))
    }
    def rotateY(angle: Float) :Transform3f = {
        transform(rotationMat(angle, Vec3f.UnitY))
    }
    def rotateZ(angle: Float) :Transform3f = {
        transform(rotationMat(angle, Vec3f.UnitZ))
    }

    def translate(u: AnyVec3f) :Transform3f = {
        m03 += u.x
        m13 += u.y
        m23 += u.z

        this
    }

    def transform(t: Transform3f) :Transform3f = {
        transform(t.matrix)
    }
    def transform(m: AnyMat3x4f) :Transform3f = {
        val a00 = m.m00*m00 + m.m01*m10 + m.m02*m20
        val a10 = m.m10*m00 + m.m11*m10 + m.m12*m20
        val a20 = m.m20*m00 + m.m21*m10 + m.m22*m20

        val a01 = m.m00*m01 + m.m01*m11 + m.m02*m21
        val a11 = m.m10*m01 + m.m11*m11 + m.m12*m21
        val a21 = m.m20*m01 + m.m21*m11 + m.m22*m21

        val a02 = m.m00*m02 + m.m01*m12 + m.m02*m22
        val a12 = m.m10*m02 + m.m11*m12 + m.m12*m22
        val a22 = m.m20*m02 + m.m21*m12 + m.m22*m22

        val a03 = m.m00*m03 + m.m01*m13 + m.m02*m23 + m.m03
        val a13 = m.m10*m03 + m.m11*m13 + m.m12*m23 + m.m13
        val a23 = m.m20*m03 + m.m21*m13 + m.m22*m23 + m.m23

        m00 = a00; m10 = a10; m20 = a20
        m01 = a01; m11 = a11; m21 = a21
        m02 = a02; m12 = a12; m22 = a22
        m03 = a03; m13 = a13; m23 = a23

        this
    }
    def transform(m: AnyMat3f) :Transform3f = {
        matrix := m*matrix
        this
    }

    def transformPoint(p: AnyVec3f) = new Vec3f(
        m00*p.x + m01*p.y + m02*p.z + m03,
        m10*p.x + m11*p.y + m12*p.z + m13,
        m20*p.x + m21*p.y + m22*p.z + m23
    )

    def transformVector(v: AnyVec3f) = new Vec3f(
        m00*v.x + m01*v.y + m02*v.z,
        m10*v.x + m11*v.y + m12*v.z,
        m20*v.x + m21*v.y + m22*v.z
    )

    def inverse() :Transform3f = {
        new Transform3f(FloatMath.inverse(matrix))
    }

    def ==(t: Transform3f) :Boolean = {
        if (t eq null) false
        else matrix == t.matrix
    }

    def !=(t: Transform3f) :Boolean = !(this == t)

    override def toString = {
        this.getClass.getSimpleName + "(" + matrix.toString + ")"
    }
}

object Transform3f {

    def apply() :Transform3f = new Transform3f(Mat3x4f(1))
    def apply(m: AnyMat3x4f) :Transform3f = new Transform3f(Mat3x4f(m))
    def apply(t: Transform3f) :Transform3f = new Transform3f(Mat3x4f(t.matrix))

    def apply(scale: AnyVec3f = Vec3f.One,
              rotation: AnyMat3f = Mat3f.Identity,
              translation: AnyVec3f = Vec3f.Zero)
    :Transform3f =
    {
        import scale.{x => sx, y => sy, z => sz}
        import rotation._
        import translation.{x => tx, y => ty, z => tz}

        new Transform3f(new Mat3x4f(
            m00*sx, m10*sx, m20*sx,
            m01*sy, m11*sy, m21*sy,
            m02*sz, m12*sz, m22*sz,
            tx, ty, tz
        ))
    }

    /**
     * @param rotation Must be an orthogonal matrix (matrix that represents
     * an unscaled rotation) to achieve the desired result.
     */
    def inverse(scale: AnyVec3f = Vec3f.One,
                rotation: AnyMat3f = Mat3f.Identity,
                translation: AnyVec3f = Vec3f.Zero)
    :Transform3f =
    {
        import translation.{x => tx, y => ty, z => tz}

        val sx = 1/scale.x
        val sy = 1/scale.y
        val sz = 1/scale.z

        val m00 = rotation.m00*sx
        val m10 = rotation.m01*sy
        val m20 = rotation.m02*sz
        val m01 = rotation.m10*sx
        val m11 = rotation.m11*sy
        val m21 = rotation.m12*sz
        val m02 = rotation.m20*sx
        val m12 = rotation.m21*sy
        val m22 = rotation.m22*sz

        new Transform3f(new Mat3x4f(
            m00, m10, m20,
            m01, m11, m21,
            m02, m12, m22,
            -m00*tx - m01*ty - m02*tz,
            -m10*tx - m11*ty - m12*tz,
            -m20*tx - m21*ty - m22*tz
        ))
    }
}
