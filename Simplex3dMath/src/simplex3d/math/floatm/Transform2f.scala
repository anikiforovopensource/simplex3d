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

import simplex3d.math._
import FloatMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class Transform2f {
    def toMatrix(): Mat2x3f

    def scale(s: Float) :Transform2f
    def scale(s: AnyVec2f) :Transform2f

    def rotate(angle: Float) :Transform2f = {
        concatenate(rotationMat(angle))
    }

    def translate(u: AnyVec2f) :Transform2f

    def concatenate(t: Transform2f) :Transform2f = {
        concatenate(t.toMatrix)
    }
    def concatenate(m: AnyMat2x3f) :Transform2f
    def concatenate(m: AnyMat2f) :Transform2f

    def transformPoint(p: AnyVec2f) :Vec2f
    def transformVector(v: AnyVec2f) :Vec2f

    def invert() :Transform2f

    def ==(t: Transform2f) :Boolean = {
        if (t eq null) false
        else toMatrix == t.toMatrix
    }

    def !=(t: Transform2f) :Boolean = !(this == t)

    override def equals(other: Any) :Boolean = {
        other match {
            case u: Transform2f => this == u
            case _ => false
        }
    }

    override def hashCode :Int = {
        toMatrix.hashCode
    }

    override def toString = {
        "Transform2f(" + toMatrix.toString + ")"
    }
}

private[math] final class TransformMat2x3f(val mat3: Mat2x3f)
extends Transform2f
{
    def toMatrix() = Mat2x3f(mat3)

    import mat3._

    def scale(s: Float) :TransformMat2x3f = {
        new TransformMat2x3f(mat3*s)
    }
    def scale(s: AnyVec2f) :TransformMat2x3f = {
        new TransformMat2x3f(new Mat2x3f(
            m00*s.x, m10*s.y,
            m01*s.x, m11*s.y,
            m02*s.x, m12*s.y
        ))
    }

    def translate(u: AnyVec2f) :TransformMat2x3f = {
        new TransformMat2x3f(new Mat2x3f(
            m00, m10,
            m01, m11,
            m02 + u.x, m12 + u.y
        ))
    }

    def concatenate(m: AnyMat2x3f) :TransformMat2x3f = {
        new TransformMat2x3f(new Mat2x3f(
            m.m00*m00 + m.m01*m10,
            m.m10*m00 + m.m11*m10,

            m.m00*m01 + m.m01*m11,
            m.m10*m01 + m.m11*m11,

            m.m00*m02 + m.m01*m12 + m.m02,
            m.m10*m02 + m.m11*m12 + m.m12
        ))
    }
    def concatenate(m: AnyMat2f) :TransformMat2x3f = {
        new TransformMat2x3f(m*mat3)
    }

    def transformPoint(p: AnyVec2f) = new Vec2f(
        m00*p.x + m01*p.y + m02,
        m10*p.x + m11*p.y + m12
    )
    def transformVector(v: AnyVec2f) = new Vec2f(
        m00*v.x + m01*v.y,
        m10*v.x + m11*v.y
    )

    def invert() :TransformMat2x3f = {
        new TransformMat2x3f(inverse(mat3))
    }
}

private[math] final class TransformMat2f(val mat2: Mat2f)
extends Transform2f
{
    import mat2._

    def toMatrix() = new Mat2x3f(
        m00, m10,
        m01, m11,
        0, 0
    )

    def scale(s: Float) :TransformMat2f = {
        new TransformMat2f(mat2*s)
    }
    def scale(s: AnyVec2f) :TransformMat2f = {
        new TransformMat2f(new Mat2f(
            m00*s.x, m10*s.y,
            m01*s.x, m11*s.y
        ))
    }

    def translate(u: AnyVec2f) :TransformMat2x3f = {
        new TransformMat2x3f(new Mat2x3f(
            m00, m10,
            m01, m11,
            u.x, u.y
        ))
    }

    def concatenate(m: AnyMat2x3f) :TransformMat2x3f = {
        new TransformMat2x3f(new Mat2x3f(
            m.m00*m00 + m.m01*m10,
            m.m10*m00 + m.m11*m10,

            m.m00*m01 + m.m01*m11,
            m.m10*m01 + m.m11*m11,

            m.m02,
            m.m12
        ))
    }
    def concatenate(m: AnyMat2f) :TransformMat2f = {
        new TransformMat2f(m*mat2)
    }

    def transformPoint(p: AnyVec2f) = transformVector(p)
    def transformVector(v: AnyVec2f) = new Vec2f(
        m00*v.x + m01*v.y,
        m10*v.x + m11*v.y
    )

    def invert() :TransformMat2f = {
        new TransformMat2f(inverse(mat2))
    }
}

private[math] final class Scale2f(val scale: Vec2f)
extends Transform2f
{
    def toMatrix() = new Mat2x3f(
        scale.x, 0,
        0, scale.y,
        0, 0
    )

    def scale(s: Float) :Scale2f = {
        new Scale2f(scale*s)
    }
    def scale(s: AnyVec2f) :Scale2f = {
        new Scale2f(scale*s)
    }

    def translate(u: AnyVec2f) :TransformMat2x3f = {
        new TransformMat2x3f(new Mat2x3f(
            scale.x, 0,
            0, scale.y,
            u.x, u.y
        ))
    }

    def concatenate(m: AnyMat2x3f) :TransformMat2x3f = {
        new TransformMat2x3f(new Mat2x3f(
            m.m00*scale.x, m.m10*scale.x,
            m.m01*scale.y, m.m11*scale.y,
            m.m02, m.m12
        ))
    }
    def concatenate(m: AnyMat2f) :TransformMat2f = {
        new TransformMat2f(new Mat2f(
            m.m00*scale.x, m.m10*scale.x,
            m.m01*scale.y, m.m11*scale.y
        ))
    }

    def transformPoint(p: AnyVec2f) = p*scale
    def transformVector(v: AnyVec2f) = v*scale

    def invert() :Scale2f = new Scale2f(1/scale)
}

private[math] final class Translation2f(val translation: Vec2f)
extends Transform2f
{
    def toMatrix() = new Mat2x3f(
        1, 0,
        0, 1,
        translation.x, translation.y
    )

    def scale(s: Float) :TransformMat2x3f = {
        new TransformMat2x3f(new Mat2x3f(
            s, 0,
            0, s,
            translation.x*s, translation.y*s
        ))
    }
    def scale(s: AnyVec2f) :TransformMat2x3f = {
        new TransformMat2x3f(new Mat2x3f(
            s.x, 0,
            0, s.y,
            translation.x*s.x, translation.y*s.y
        ))
    }

    def translate(u: AnyVec2f) :Translation2f = {
        new Translation2f(translation + u)
    }

    def concatenate(m: AnyMat2x3f) :TransformMat2x3f = {
        import translation._

        new TransformMat2x3f(new Mat2x3f(
            m.m00, m.m10,
            m.m01, m.m11,

            m.m00*x + m.m01*y + m.m02,
            m.m10*x + m.m11*y + m.m12
        ))
    }
    def concatenate(m: AnyMat2f) :TransformMat2x3f = {
        import translation._

        new TransformMat2x3f(new Mat2x3f(
            m.m00, m.m10,
            m.m01, m.m11,

            m.m00*x + m.m01*y,
            m.m10*x + m.m11*y
        ))
    }

    def transformPoint(p: AnyVec2f) = translation + p
    def transformVector(v: AnyVec2f) = Vec2f(v)

    def invert() :Translation2f = {
        new Translation2f(-translation)
    }
}

private[math] object Transform2fIdentity extends Transform2f {
    def toMatrix() = Mat2x3f(1)

    def scale(s: Float) :Transform2f = new Scale2f(Vec2f(s))
    def scale(s: AnyVec2f) :Transform2f = new Scale2f(Vec2f(s))

    def translate(u: AnyVec2f) :Transform2f = new Translation2f(Vec2f(u))

    def concatenate(m: AnyMat2x3f) :Transform2f = {
        new TransformMat2x3f(Mat2x3f(m))
    }
    def concatenate(m: AnyMat2f) :Transform2f = {
        new TransformMat2f(Mat2f(m))
    }

    def transformPoint(p: AnyVec2f) :Vec2f = Vec2f(p)
    def transformVector(v: AnyVec2f) :Vec2f = Vec2f(v)

    def invert() :Transform2f = this
}

object Transform2f {
    val Identity: Transform2f = Transform2fIdentity

    def apply(m: Read2x2) :Transform2f =
        new TransformMat2f(Mat2f(m))

    def apply(m: Read2x3) :Transform2f =
        new TransformMat2x3f(Mat2x3f(m))

    def scale(s: Float) :Transform2f = Identity.scale(s)
    def scale(s: AnyVec2f) :Transform2f = Identity.scale(s)

    def rotate(angle: Float) :Transform2f = Identity.rotate(angle)

    def translate(u: AnyVec2f) :Transform2f = Identity.translate(u)
}
