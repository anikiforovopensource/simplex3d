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

import simplex3d.math._
import DoubleMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class Transform2d {
    def toMatrix(): Mat2x3d

    def scale(s: Double) :Transform2d
    def scale(s: AnyVec2d) :Transform2d

    def rotate(angle: Double) :Transform2d = {
        concatenate(rotationMat(angle))
    }

    def translate(u: AnyVec2d) :Transform2d

    def concatenate(t: Transform2d) :Transform2d = {
        concatenate(t.toMatrix)
    }
    def concatenate(m: AnyMat2x3d) :Transform2d
    def concatenate(m: AnyMat2d) :Transform2d

    def transformPoint(p: AnyVec2d) :Vec2d
    def transformVector(v: AnyVec2d) :Vec2d

    def invert() :Transform2d

    def ==(t: Transform2d) :Boolean = {
        if (t eq null) false
        else toMatrix == t.toMatrix
    }

    def !=(t: Transform2d) :Boolean = !(this == t)

    override def equals(other: Any) :Boolean = {
        other match {
            case u: Transform2d => this == u
            case _ => false
        }
    }

    override def hashCode :Int = {
        toMatrix.hashCode
    }

    override def toString = {
        "Transform2d(" + toMatrix.toString + ")"
    }
}

private[math] final class TransformMat2x3d(val mat3: Mat2x3d)
extends Transform2d
{
    def toMatrix() = Mat2x3d(mat3)

    import mat3._

    def scale(s: Double) :TransformMat2x3d = {
        new TransformMat2x3d(mat3*s)
    }
    def scale(s: AnyVec2d) :TransformMat2x3d = {
        new TransformMat2x3d(new Mat2x3d(
            m00*s.x, m10*s.y,
            m01*s.x, m11*s.y,
            m02*s.x, m12*s.y
        ))
    }

    def translate(u: AnyVec2d) :TransformMat2x3d = {
        new TransformMat2x3d(new Mat2x3d(
            m00, m10,
            m01, m11,
            m02 + u.x, m12 + u.y
        ))
    }

    def concatenate(m: AnyMat2x3d) :TransformMat2x3d = {
        new TransformMat2x3d(new Mat2x3d(
            m.m00*m00 + m.m01*m10,
            m.m10*m00 + m.m11*m10,

            m.m00*m01 + m.m01*m11,
            m.m10*m01 + m.m11*m11,

            m.m00*m02 + m.m01*m12 + m.m02,
            m.m10*m02 + m.m11*m12 + m.m12
        ))
    }
    def concatenate(m: AnyMat2d) :TransformMat2x3d = {
        new TransformMat2x3d(m*mat3)
    }

    def transformPoint(p: AnyVec2d) = new Vec2d(
        m00*p.x + m01*p.y + m02,
        m10*p.x + m11*p.y + m12
    )
    def transformVector(v: AnyVec2d) = new Vec2d(
        m00*v.x + m01*v.y,
        m10*v.x + m11*v.y
    )

    def invert() :TransformMat2x3d = {
        new TransformMat2x3d(inverse(mat3))
    }
}

private[math] final class TransformMat2d(val mat2: Mat2d)
extends Transform2d
{
    import mat2._

    def toMatrix() = new Mat2x3d(
        m00, m10,
        m01, m11,
        0, 0
    )

    def scale(s: Double) :TransformMat2d = {
        new TransformMat2d(mat2*s)
    }
    def scale(s: AnyVec2d) :TransformMat2d = {
        new TransformMat2d(new Mat2d(
            m00*s.x, m10*s.y,
            m01*s.x, m11*s.y
        ))
    }

    def translate(u: AnyVec2d) :TransformMat2x3d = {
        new TransformMat2x3d(new Mat2x3d(
            m00, m10,
            m01, m11,
            u.x, u.y
        ))
    }

    def concatenate(m: AnyMat2x3d) :TransformMat2x3d = {
        new TransformMat2x3d(new Mat2x3d(
            m.m00*m00 + m.m01*m10,
            m.m10*m00 + m.m11*m10,

            m.m00*m01 + m.m01*m11,
            m.m10*m01 + m.m11*m11,

            m.m02,
            m.m12
        ))
    }
    def concatenate(m: AnyMat2d) :TransformMat2d = {
        new TransformMat2d(m*mat2)
    }

    def transformPoint(p: AnyVec2d) = transformVector(p)
    def transformVector(v: AnyVec2d) = new Vec2d(
        m00*v.x + m01*v.y,
        m10*v.x + m11*v.y
    )

    def invert() :TransformMat2d = {
        new TransformMat2d(inverse(mat2))
    }
}

private[math] final class Scale2d(val scale: Vec2d)
extends Transform2d
{
    def toMatrix() = new Mat2x3d(
        scale.x, 0,
        0, scale.y,
        0, 0
    )

    def scale(s: Double) :Scale2d = {
        new Scale2d(scale*s)
    }
    def scale(s: AnyVec2d) :Scale2d = {
        new Scale2d(scale*s)
    }

    def translate(u: AnyVec2d) :TransformMat2x3d = {
        new TransformMat2x3d(new Mat2x3d(
            scale.x, 0,
            0, scale.y,
            u.x, u.y
        ))
    }

    def concatenate(m: AnyMat2x3d) :TransformMat2x3d = {
        new TransformMat2x3d(new Mat2x3d(
            m.m00*scale.x, m.m10*scale.x,
            m.m01*scale.y, m.m11*scale.y,
            m.m02, m.m12
        ))
    }
    def concatenate(m: AnyMat2d) :TransformMat2d = {
        new TransformMat2d(new Mat2d(
            m.m00*scale.x, m.m10*scale.x,
            m.m01*scale.y, m.m11*scale.y
        ))
    }

    def transformPoint(p: AnyVec2d) = p*scale
    def transformVector(v: AnyVec2d) = v*scale

    def invert() :Scale2d = new Scale2d(1/scale)
}

private[math] final class Translation2d(val translation: Vec2d)
extends Transform2d
{
    def toMatrix() = new Mat2x3d(
        1, 0,
        0, 1,
        translation.x, translation.y
    )

    def scale(s: Double) :TransformMat2x3d = {
        new TransformMat2x3d(new Mat2x3d(
            s, 0,
            0, s,
            translation.x*s, translation.y*s
        ))
    }
    def scale(s: AnyVec2d) :TransformMat2x3d = {
        new TransformMat2x3d(new Mat2x3d(
            s.x, 0,
            0, s.y,
            translation.x*s.x, translation.y*s.y
        ))
    }

    def translate(u: AnyVec2d) :Translation2d = {
        new Translation2d(translation + u)
    }

    def concatenate(m: AnyMat2x3d) :TransformMat2x3d = {
        import translation._

        new TransformMat2x3d(new Mat2x3d(
            m.m00, m.m10,
            m.m01, m.m11,

            m.m00*x + m.m01*y + m.m02,
            m.m10*x + m.m11*y + m.m12
        ))
    }
    def concatenate(m: AnyMat2d) :TransformMat2x3d = {
        import translation._

        new TransformMat2x3d(new Mat2x3d(
            m.m00, m.m10,
            m.m01, m.m11,

            m.m00*x + m.m01*y,
            m.m10*x + m.m11*y
        ))
    }

    def transformPoint(p: AnyVec2d) = translation + p
    def transformVector(v: AnyVec2d) = Vec2d(v)

    def invert() :Translation2d = {
        new Translation2d(-translation)
    }
}

private[math] object Transform2dIdentity extends Transform2d {
    def toMatrix() = Mat2x3d(1)

    def scale(s: Double) :Transform2d = new Scale2d(Vec2d(s))
    def scale(s: AnyVec2d) :Transform2d = new Scale2d(Vec2d(s))

    def translate(u: AnyVec2d) :Transform2d = new Translation2d(Vec2d(u))

    def concatenate(m: AnyMat2x3d) :Transform2d = {
        new TransformMat2x3d(Mat2x3d(m))
    }
    def concatenate(m: AnyMat2d) :Transform2d = {
        new TransformMat2d(Mat2d(m))
    }

    def transformPoint(p: AnyVec2d) :Vec2d = Vec2d(p)
    def transformVector(v: AnyVec2d) :Vec2d = Vec2d(v)

    def invert() :Transform2d = this
}

object Transform2d {
    val Identity: Transform2d = Transform2dIdentity

    def apply(m: Read2x2) :Transform2d =
        new TransformMat2d(Mat2d(m))

    def apply(m: Read2x3) :Transform2d =
        new TransformMat2x3d(Mat2x3d(m))

    def scale(s: Double) :Transform2d = Identity.scale(s)
    def scale(s: AnyVec2d) :Transform2d = Identity.scale(s)

    def rotate(angle: Double) :Transform2d = Identity.rotate(angle)

    def translate(u: AnyVec2d) :Transform2d = Identity.translate(u)
}
