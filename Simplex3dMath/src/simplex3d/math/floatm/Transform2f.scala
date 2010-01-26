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
class Transform2f private[math] (val matrix: Mat2x3f) {
    import matrix._

    def scale(s: Float) :Transform2f = {
        matrix *= s
        this
    }
    def scale(s: AnyVec2f) :Transform2f = {
        m00 *= s.x; m10 *= s.y;
        m01 *= s.x; m11 *= s.y;
        m02 *= s.x; m12 *= s.y;

        this
    }

    def rotate(angle: Float) :Transform2f = {
        transform(rotationMat(angle))
    }

    def translate(u: AnyVec2f) :Transform2f = {
        m02 += u.x
        m12 += u.y

        this
    }

    def transform(t: Transform2f) :Transform2f = {
        transform(t.matrix)
    }
    def transform(m: AnyMat2x3f) :Transform2f = {
        val a00 = m.m00*m00 + m.m01*m10
        val a10 = m.m10*m00 + m.m11*m10

        val a01 = m.m00*m01 + m.m01*m11
        val a11 = m.m10*m01 + m.m11*m11

        val a02 = m.m00*m02 + m.m01*m12 + m.m02
        val a12 = m.m10*m02 + m.m11*m12 + m.m12

        m00 = a00; m10 = a10
        m01 = a01; m11 = a11
        m02 = a02; m12 = a12

        this
    }
    def transform(m: AnyMat2f) :Transform2f = {
        matrix := m*matrix
        this
    }

    def transformPoint(p: AnyVec2f) = new Vec2f(
        m00*p.x + m01*p.y + m02,
        m10*p.x + m11*p.y + m12
    )

    def transformVector(v: AnyVec2f) = new Vec2f(
        m00*v.x + m01*v.y,
        m10*v.x + m11*v.y
    )

    def inverse() :Transform2f = {
        new Transform2f(FloatMath.inverse(matrix))
    }

    def ==(t: Transform2f) :Boolean = {
        if (t eq null) false
        else matrix == t.matrix
    }

    def !=(t: Transform2f) :Boolean = !(this == t)

    override def toString = {
        this.getClass.getSimpleName + "(" + matrix.toString + ")"
    }
}

object Transform2f {

    def apply() :Transform2f = new Transform2f(Mat2x3f(1))
    def apply(m: AnyMat2x3f) :Transform2f = new Transform2f(Mat2x3f(m))
    def apply(t: Transform2f) :Transform2f = new Transform2f(Mat2x3f(t.matrix))

    def apply(scale: AnyVec2f = Vec2f.One,
              rotation: AnyMat2f = Mat2f.Identity,
              translation: AnyVec2f = Vec2f.Zero)
    :Transform2f =
    {
        import rotation._
        import translation.{x => tx, y => ty}
        import scale.{x => sx, y => sy}

        new Transform2f(new Mat2x3f(
            m00*sx, m10*sx,
            m01*sy, m11*sy,
            tx, ty
        ))
    }

    /**
     * @param rotation Must be an orthogonal matrix (matrix that represents
     * an unscaled rotation) to achieve the desired result.
     */
    def inverse(scale: AnyVec2f = Vec2f.One,
                rotation: AnyMat2f = Mat2f.Identity,
                translation: AnyVec2f = Vec2f.Zero)
    :Transform2f =
    {
        import translation.{x => tx, y => ty}

        val sx = 1/scale.x
        val sy = 1/scale.y

        val m00 = rotation.m00*sx
        val m10 = rotation.m01*sy
        val m01 = rotation.m10*sx
        val m11 = rotation.m11*sy

        new Transform2f(new Mat2x3f(
            m00, m10,
            m01, m11,
            -m00*tx - m01*ty,
            -m10*tx - m11*ty
        ))
    }
}
