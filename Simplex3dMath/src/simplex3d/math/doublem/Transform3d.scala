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
sealed abstract class Transform3d {
    def toMatrix(): Mat3x4d
    
    def scale(s: Double) :Transform3d
    def scale(s: AnyVec3d) :Transform3d

    def rotate(q: AnyQuat4d) :Transform3d = {
        concatenate(rotationMat(q))
    }
    def rotate(angle: Double, axis: AnyVec3d) :Transform3d = {
        concatenate(rotationMat(angle, axis))
    }

    def rotateX(angle: Double) :Transform3d = {
        concatenate(rotationMat(angle, Vec3d.UnitX))
    }
    def rotateY(angle: Double) :Transform3d = {
        concatenate(rotationMat(angle, Vec3d.UnitY))
    }
    def rotateZ(angle: Double) :Transform3d = {
        concatenate(rotationMat(angle, Vec3d.UnitZ))
    }

    def translate(u: AnyVec3d) :Transform3d

    def concatenate(t: Transform3d) :Transform3d = {
        concatenate(t.toMatrix)
    }
    def concatenate(m: AnyMat3x4d) :Transform3d
    def concatenate(m: AnyMat3d) :Transform3d

    def transformPoint(p: AnyVec3d) :Vec3d
    def transformVector(v: AnyVec3d) :Vec3d

    def invert() :Transform3d

    def ==(t: Transform3d) :Boolean = {
        if (t eq null) false
        else toMatrix == t.toMatrix
    }

    def !=(t: Transform3d) :Boolean = !(this == t)

    override def equals(other: Any) :Boolean = {
        other match {
            case u: Transform3d => this == u
            case _ => false
        }
    }

    override def hashCode :Int = {
        toMatrix.hashCode
    }

    override def toString = {
        "Transform3d(" + toMatrix.toString + ")"
    }
}

private[math] final class TransformMat3x4d(val mat4: Mat3x4d)
extends Transform3d
{
    def toMatrix() = Mat3x4d(mat4)

    import mat4._

    def scale(s: Double) :TransformMat3x4d = {
        new TransformMat3x4d(mat4*s)
    }
    def scale(s: AnyVec3d) :TransformMat3x4d = {
        new TransformMat3x4d(new Mat3x4d(
            m00*s.x, m10*s.y, m20*s.z,
            m01*s.x, m11*s.y, m21*s.z,
            m02*s.x, m12*s.y, m22*s.z,
            m03*s.x, m13*s.y, m23*s.z
        ))
    }

    def translate(u: AnyVec3d) :TransformMat3x4d = {
        new TransformMat3x4d(new Mat3x4d(
            m00, m10, m20,
            m01, m11, m21,
            m02, m12, m22,
            m03 + u.x, m13 + u.y, m23 + u.z
        ))
    }

    def concatenate(m: AnyMat3x4d) :TransformMat3x4d = {
        new TransformMat3x4d(new Mat3x4d(
            m.m00*m00 + m.m01*m10 + m.m02*m20,
            m.m10*m00 + m.m11*m10 + m.m12*m20,
            m.m20*m00 + m.m21*m10 + m.m22*m20,

            m.m00*m01 + m.m01*m11 + m.m02*m21,
            m.m10*m01 + m.m11*m11 + m.m12*m21,
            m.m20*m01 + m.m21*m11 + m.m22*m21,

            m.m00*m02 + m.m01*m12 + m.m02*m22,
            m.m10*m02 + m.m11*m12 + m.m12*m22,
            m.m20*m02 + m.m21*m12 + m.m22*m22,

            m.m00*m03 + m.m01*m13 + m.m02*m23 + m.m03,
            m.m10*m03 + m.m11*m13 + m.m12*m23 + m.m13,
            m.m20*m03 + m.m21*m13 + m.m22*m23 + m.m23
        ))
    }
    def concatenate(m: AnyMat3d) :TransformMat3x4d = {
        new TransformMat3x4d(m*mat4)
    }

    def transformPoint(p: AnyVec3d) = new Vec3d(
        m00*p.x + m01*p.y + m02*p.z + m03,
        m10*p.x + m11*p.y + m12*p.z + m13,
        m20*p.x + m21*p.y + m22*p.z + m23
    )
    def transformVector(v: AnyVec3d) = new Vec3d(
        m00*v.x + m01*v.y + m02*v.z,
        m10*v.x + m11*v.y + m12*v.z,
        m20*v.x + m21*v.y + m22*v.z
    )

    def invert() :TransformMat3x4d = {
        new TransformMat3x4d(inverse(mat4))
    }
}

private[math] final class TransformMat3d(val mat3: Mat3d)
extends Transform3d
{
    import mat3._

    def toMatrix() = new Mat3x4d(
        m00, m10, m20,
        m01, m11, m21,
        m02, m12, m22,
        0, 0, 0
    )

    def scale(s: Double) :TransformMat3d = {
        new TransformMat3d(mat3*s)
    }
    def scale(s: AnyVec3d) :TransformMat3d = {
        new TransformMat3d(new Mat3d(
            m00*s.x, m10*s.y, m20*s.z,
            m01*s.x, m11*s.y, m21*s.z,
            m02*s.x, m12*s.y, m22*s.z
        ))
    }

    def translate(u: AnyVec3d) :TransformMat3x4d = {
        new TransformMat3x4d(new Mat3x4d(
            m00, m10, m20,
            m01, m11, m21,
            m02, m12, m22,
            u.x, u.y, u.z
        ))
    }

    def concatenate(m: AnyMat3x4d) :TransformMat3x4d = {
        new TransformMat3x4d(new Mat3x4d(
            m.m00*m00 + m.m01*m10 + m.m02*m20,
            m.m10*m00 + m.m11*m10 + m.m12*m20,
            m.m20*m00 + m.m21*m10 + m.m22*m20,

            m.m00*m01 + m.m01*m11 + m.m02*m21,
            m.m10*m01 + m.m11*m11 + m.m12*m21,
            m.m20*m01 + m.m21*m11 + m.m22*m21,

            m.m00*m02 + m.m01*m12 + m.m02*m22,
            m.m10*m02 + m.m11*m12 + m.m12*m22,
            m.m20*m02 + m.m21*m12 + m.m22*m22,

            m.m03,
            m.m13,
            m.m23
        ))
    }
    def concatenate(m: AnyMat3d) :TransformMat3d = {
        new TransformMat3d(m*mat3)
    }

    def transformPoint(p: AnyVec3d) = transformVector(p)
    def transformVector(v: AnyVec3d) = new Vec3d(
        m00*v.x + m01*v.y + m02*v.z,
        m10*v.x + m11*v.y + m12*v.z,
        m20*v.x + m21*v.y + m22*v.z
    )

    def invert() :TransformMat3d = {
        new TransformMat3d(inverse(mat3))
    }
}

private[math] final class Scale3d(val scale: Vec3d)
extends Transform3d
{
    def toMatrix() = new Mat3x4d(
        scale.x, 0, 0,
        0, scale.y, 0,
        0, 0, scale.z,
        0, 0, 0
    )

    def scale(s: Double) :Scale3d = {
        new Scale3d(scale*s)
    }
    def scale(s: AnyVec3d) :Scale3d = {
        new Scale3d(scale*s)
    }

    def translate(u: AnyVec3d) :TransformMat3x4d = {
        new TransformMat3x4d(new Mat3x4d(
            scale.x, 0, 0,
            0, scale.y, 0,
            0, 0, scale.z,
            u.x, u.y, u.z
        ))
    }

    def concatenate(m: AnyMat3x4d) :TransformMat3x4d = {
        new TransformMat3x4d(new Mat3x4d(
            m.m00*scale.x, m.m10*scale.x, m.m20*scale.x,
            m.m01*scale.y, m.m11*scale.y, m.m21*scale.y,
            m.m02*scale.z, m.m12*scale.z, m.m22*scale.z,
            m.m03, m.m13, m.m23
        ))
    }
    def concatenate(m: AnyMat3d) :TransformMat3d = {
        new TransformMat3d(new Mat3d(
            m.m00*scale.x, m.m10*scale.x, m.m20*scale.x,
            m.m01*scale.y, m.m11*scale.y, m.m21*scale.y,
            m.m02*scale.z, m.m12*scale.z, m.m22*scale.z
        ))
    }

    def transformPoint(p: AnyVec3d) = p*scale
    def transformVector(v: AnyVec3d) = v*scale

    def invert() :Scale3d = new Scale3d(1/scale)
}

private[math] final class Translation3d(val translation: Vec3d)
extends Transform3d
{
    def toMatrix() = new Mat3x4d(
        1, 0, 0,
        0, 1, 0,
        0, 0, 1,
        translation.x, translation.y, translation.z
    )

    def scale(s: Double) :TransformMat3x4d = {
        new TransformMat3x4d(new Mat3x4d(
            s, 0, 0,
            0, s, 0,
            0, 0, s,
            translation.x*s, translation.y*s, translation.z*s
        ))
    }
    def scale(s: AnyVec3d) :TransformMat3x4d = {
        new TransformMat3x4d(new Mat3x4d(
            s.x, 0, 0,
            0, s.y, 0,
            0, 0, s.z,
            translation.x*s.x, translation.y*s.y, translation.z*s.z
        ))
    }

    def translate(u: AnyVec3d) :Translation3d = {
        new Translation3d(translation + u)
    }

    def concatenate(m: AnyMat3x4d) :TransformMat3x4d = {
        import translation._

        new TransformMat3x4d(new Mat3x4d(
            m.m00, m.m10, m.m20,
            m.m01, m.m11, m.m21,
            m.m02, m.m12, m.m22,

            m.m00*x + m.m01*y + m.m02*z + m.m03,
            m.m10*x + m.m11*y + m.m12*z + m.m13,
            m.m20*x + m.m21*y + m.m22*z + m.m23
        ))
    }
    def concatenate(m: AnyMat3d) :TransformMat3x4d = {
        import translation._

        new TransformMat3x4d(new Mat3x4d(
            m.m00, m.m10, m.m20,
            m.m01, m.m11, m.m21,
            m.m02, m.m12, m.m22,

            m.m00*x + m.m01*y + m.m02*z,
            m.m10*x + m.m11*y + m.m12*z,
            m.m20*x + m.m21*y + m.m22*z
        ))
    }

    def transformPoint(p: AnyVec3d) = translation + p
    def transformVector(v: AnyVec3d) = Vec3d(v)

    def invert() :Translation3d = {
        new Translation3d(-translation)
    }
}

private[math] object Transform3dIdentity extends Transform3d {
    def toMatrix() = Mat3x4d(1)

    def scale(s: Double) :Transform3d = new Scale3d(Vec3d(s))
    def scale(s: AnyVec3d) :Transform3d = new Scale3d(Vec3d(s))

    def translate(u: AnyVec3d) :Transform3d = new Translation3d(Vec3d(u))

    def concatenate(m: AnyMat3x4d) :Transform3d = {
        new TransformMat3x4d(Mat3x4d(m))
    }
    def concatenate(m: AnyMat3d) :Transform3d = {
        new TransformMat3d(Mat3d(m))
    }

    def transformPoint(p: AnyVec3d) :Vec3d = Vec3d(p)
    def transformVector(v: AnyVec3d) :Vec3d = Vec3d(v)

    def invert() :Transform3d = this
}

object Transform3d {
    val Identity: Transform3d = Transform3dIdentity

    def apply(m: Read3x3) :Transform3d =
        new TransformMat3d(Mat3d(m))

    def apply(m: Read3x4) :Transform3d =
        new TransformMat3x4d(Mat3x4d(m))

    def scale(s: Double) :Transform3d = Identity.scale(s)
    def scale(s: AnyVec3d) :Transform3d = Identity.scale(s)

    def rotate(q: AnyQuat4d) :Transform3d = Identity.rotate(q)
    def rotate(angle: Double, axis: AnyVec3d) :Transform3d = {
        Identity.rotate(angle, axis)
    }

    def rotateX(angle: Double) :Transform3d = Identity.rotateX(angle)
    def rotateY(angle: Double) :Transform3d = Identity.rotateY(angle)
    def rotateZ(angle: Double) :Transform3d = Identity.rotateZ(angle)

    def translate(u: AnyVec3d) :Transform3d = Identity.translate(u)
}
