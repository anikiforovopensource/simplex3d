/*
 * Simplex3d, DoubleMath module
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

package simplex3d.math.doublem

import DoubleMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyTransform2d (val matrix: AnyMat2x3d) {
    import matrix._

    def scale(s: Float) :Transform2d = {
        new Transform2d(matrix*s)
    }
    def scale(s: AnyVec2d) :Transform2d = {
        new Transform2d(new Mat2x3d(
            m00*s.x, m10*s.y,
            m01*s.x, m11*s.y,
            m02*s.x, m12*s.y
        ))
    }

    def rotate(angle: Float) :Transform2d = {
        concatenate(rotationMat(angle))
    }

    def translate(u: AnyVec2d) :Transform2d = {
        new Transform2d(new Mat2x3d(
            m00, m10,
            m01, m11,
            m02 + u.x, m12 + u.y
        ))
    }

    def concatenate(t: AnyTransform2d) :Transform2d = {
        concatenate(t.matrix)
    }
    def concatenate(m: AnyMat2x3d) :Transform2d = {
        new Transform2d(new Mat2x3d(
            m.m00*m00 + m.m01*m10,
            m.m10*m00 + m.m11*m10,

            m.m00*m01 + m.m01*m11,
            m.m10*m01 + m.m11*m11,

            m.m00*m02 + m.m01*m12 + m.m02,
            m.m10*m02 + m.m11*m12 + m.m12
        ))
    }
    def concatenate(m: AnyMat2d) :Transform2d = {
        new Transform2d(m*matrix)
    }

    def transformPoint(p: AnyVec2d) = new Vec2d(
        m00*p.x + m01*p.y + m02,
        m10*p.x + m11*p.y + m12
    )

    def transformVector(v: AnyVec2d) = new Vec2d(
        m00*v.x + m01*v.y,
        m10*v.x + m11*v.y
    )

    def invert() :Transform2d = {
        new Transform2d(inverse(matrix))
    }

    def ==(t: AnyTransform2d) :Boolean = {
        if (t eq null) false
        else matrix == t.matrix
    }

    def !=(t: AnyTransform2d) :Boolean = !(this == t)

    override def equals(other: Any) :Boolean = {
        other match {
            case u: Transform2d => this == u
            case _ => false
        }
    }

    override def hashCode :Int = {
        matrix.hashCode
    }

    override def toString = {
        this.getClass.getSimpleName + "(" + matrix.toString + ")"
    }
}

final class ConstTransform2d private[math] (override val matrix: ConstMat2x3d)
extends AnyTransform2d(matrix)

object ConstTransform2d {
    def apply(m: AnyMat2x3d) :ConstTransform2d =
        new ConstTransform2d(ConstMat2x3d(m))

    def apply(t: AnyTransform2d) :ConstTransform2d =
        new ConstTransform2d(ConstMat2x3d(t.matrix))

    implicit def toConst(t: Transform2d) = ConstTransform2d(t)
}

final class Transform2d private[math] (override val matrix: Mat2x3d)
extends AnyTransform2d(matrix)

object Transform2d {

    val Identity: ConstTransform2d = Transform2d()

    def apply() :Transform2d = new Transform2d(Mat2x3d(1))
    def apply(m: AnyMat2x3d) :Transform2d = new Transform2d(Mat2x3d(m))

    def apply(t: AnyTransform2d) :Transform2d =
        new Transform2d(Mat2x3d(t.matrix))

    def apply(scale: AnyVec2d = Vec2d.One,
              rotation: AnyMat2d = Mat2d.Identity,
              translation: AnyVec2d = Vec2d.Zero)
    :Transform2d =
    {
        import rotation._
        import translation.{x => tx, y => ty}
        import scale.{x => sx, y => sy}

        new Transform2d(new Mat2x3d(
            m00*sx, m10*sx,
            m01*sy, m11*sy,
            tx, ty
        ))
    }

    /**
     * @param rotation Must be an orthogonal matrix (matrix that represents
     * an unscaled rotation) to achieve the desired result.
     */
    def inverse(scale: AnyVec2d = Vec2d.One,
                rotation: AnyMat2d = Mat2d.Identity,
                translation: AnyVec2d = Vec2d.Zero)
    :Transform2d =
    {
        import translation.{x => tx, y => ty}

        val sx = 1/scale.x
        val sy = 1/scale.y

        val m00 = rotation.m00*sx
        val m10 = rotation.m01*sy
        val m01 = rotation.m10*sx
        val m11 = rotation.m11*sy

        new Transform2d(new Mat2x3d(
            m00, m10,
            m01, m11,
            -m00*tx - m01*ty,
            -m10*tx - m11*ty
        ))
    }

    implicit def toMutable(t: ConstTransform2d) = Transform2d(t)
}
