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
sealed abstract class Transform3f {
    def toMatrix: Mat3x4f
    
    def scale(s: Float) :Transform3f
    def scale(s: AnyVec3f) :Transform3f

    def rotate(q: AnyQuat4f) :Transform3f = {
        concatenate(rotationMat(q))
    }
    def rotate(angle: Float, axis: AnyVec3f) :Transform3f = {
        concatenate(rotationMat(angle, axis))
    }

    def rotateX(angle: Float) :Transform3f = {
        concatenate(rotationMat(angle, Vec3f.UnitX))
    }
    def rotateY(angle: Float) :Transform3f = {
        concatenate(rotationMat(angle, Vec3f.UnitY))
    }
    def rotateZ(angle: Float) :Transform3f = {
        concatenate(rotationMat(angle, Vec3f.UnitZ))
    }

    def translate(u: AnyVec3f) :Transform3f

    def concatenate(t: Transform3f) :Transform3f = {
        concatenate(t.toMatrix)
    }
    def concatenate(m: AnyMat3x4f) :Transform3f
    def concatenate(m: AnyMat3f) :Transform3f

    def transformPoint(p: AnyVec3f) :Vec3f
    def transformVector(v: AnyVec3f) :Vec3f

    def invert() :Transform3f

    def ==(t: Transform3f) :Boolean = {
        if (t eq null) false
        else toMatrix == t.toMatrix
    }

    def !=(t: Transform3f) :Boolean = !(this == t)

    override def equals(other: Any) :Boolean = {
        other match {
            case u: Transform3f => this == u
            case _ => false
        }
    }

    override def hashCode :Int = {
        toMatrix.hashCode
    }

    override def toString = {
        "Transform3f(" + toMatrix.toString + ")"
    }
}

private[math] final class TransformMat3x4f(val mat4: Mat3x4f)
extends Transform3f
{
    def toMatrix = Mat3x4f(mat4)

    import mat4._

    def scale(s: Float) :TransformMat3x4f = {
        new TransformMat3x4f(mat4*s)
    }
    def scale(s: AnyVec3f) :TransformMat3x4f = {
        new TransformMat3x4f(new Mat3x4f(
            m00*s.x, m10*s.y, m20*s.z,
            m01*s.x, m11*s.y, m21*s.z,
            m02*s.x, m12*s.y, m22*s.z,
            m03*s.x, m13*s.y, m23*s.z
        ))
    }

    def translate(u: AnyVec3f) :TransformMat3x4f = {
        new TransformMat3x4f(new Mat3x4f(
            m00, m10, m20,
            m01, m11, m21,
            m02, m12, m22,
            m03 + u.x, m13 + u.y, m23 + u.z
        ))
    }

    def concatenate(m: AnyMat3x4f) :TransformMat3x4f = {
        new TransformMat3x4f(new Mat3x4f(
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
    def concatenate(m: AnyMat3f) :TransformMat3x4f = {
        new TransformMat3x4f(m*mat4)
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

    def invert() :TransformMat3x4f = {
        new TransformMat3x4f(inverse(mat4))
    }
}

private[math] final class TransformMat3f(val mat3: Mat3f)
extends Transform3f
{
    import mat3._

    def toMatrix = new Mat3x4f(
        m00, m10, m20,
        m01, m11, m21,
        m02, m12, m22,
        0, 0, 0
    )

    def scale(s: Float) :TransformMat3f = {
        new TransformMat3f(mat3*s)
    }
    def scale(s: AnyVec3f) :TransformMat3f = {
        new TransformMat3f(new Mat3f(
            m00*s.x, m10*s.y, m20*s.z,
            m01*s.x, m11*s.y, m21*s.z,
            m02*s.x, m12*s.y, m22*s.z
        ))
    }

    def translate(u: AnyVec3f) :TransformMat3x4f = {
        new TransformMat3x4f(new Mat3x4f(
            m00, m10, m20,
            m01, m11, m21,
            m02, m12, m22,
            u.x, u.y, u.z
        ))
    }

    def concatenate(m: AnyMat3x4f) :TransformMat3x4f = {
        new TransformMat3x4f(new Mat3x4f(
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
    def concatenate(m: AnyMat3f) :TransformMat3f = {
        new TransformMat3f(m*mat3)
    }

    def transformPoint(p: AnyVec3f) = new Vec3f(
        m00*p.x + m01*p.y + m02*p.z,
        m10*p.x + m11*p.y + m12*p.z,
        m20*p.x + m21*p.y + m22*p.z
    )
    def transformVector(v: AnyVec3f) = new Vec3f(
        m00*v.x + m01*v.y + m02*v.z,
        m10*v.x + m11*v.y + m12*v.z,
        m20*v.x + m21*v.y + m22*v.z
    )

    def invert() :TransformMat3f = {
        new TransformMat3f(inverse(mat3))
    }
}

private[math] final class Scale3f(val scale: Vec3f)
extends Transform3f
{
    def toMatrix = new Mat3x4f(
        scale.x, 0, 0,
        0, scale.y, 0,
        0, 0, scale.z,
        0, 0, 0
    )

    def scale(s: Float) :Scale3f = {
        new Scale3f(scale*s)
    }
    def scale(s: AnyVec3f) :Scale3f = {
        new Scale3f(scale*s)
    }

    def translate(u: AnyVec3f) :TransformMat3x4f = {
        new TransformMat3x4f(new Mat3x4f(
            scale.x, 0, 0,
            0, scale.y, 0,
            0, 0, scale.z,
            u.x, u.y, u.z
        ))
    }

    def concatenate(m: AnyMat3x4f) :TransformMat3x4f = {
        new TransformMat3x4f(new Mat3x4f(
            m.m00*scale.x, m.m10*scale.x, m.m20*scale.x,
            m.m01*scale.y, m.m11*scale.y, m.m21*scale.y,
            m.m02*scale.z, m.m12*scale.z, m.m22*scale.z,
            m.m03, m.m13, m.m23
        ))
    }
    def concatenate(m: AnyMat3f) :TransformMat3f = {
        new TransformMat3f(new Mat3f(
            m.m00*scale.x, m.m10*scale.x, m.m20*scale.x,
            m.m01*scale.y, m.m11*scale.y, m.m21*scale.y,
            m.m02*scale.z, m.m12*scale.z, m.m22*scale.z
        ))
    }

    def transformPoint(p: AnyVec3f) = p*scale
    def transformVector(v: AnyVec3f) = v*scale

    def invert() :Scale3f = new Scale3f(1/scale)
}

private[math] final class Translation3f(val translation: Vec3f)
extends Transform3f
{
    def toMatrix = new Mat3x4f(
        1, 0, 0,
        0, 1, 0,
        0, 0, 1,
        translation.x, translation.y, translation.z
    )

    def scale(s: Float) :TransformMat3x4f = {
        new TransformMat3x4f(new Mat3x4f(
            s, 0, 0,
            0, s, 0,
            0, 0, s,
            translation.x*s, translation.y*s, translation.z*s
        ))
    }
    def scale(s: AnyVec3f) :TransformMat3x4f = {
        new TransformMat3x4f(new Mat3x4f(
            s.x, 0, 0,
            0, s.y, 0,
            0, 0, s.z,
            translation.x*s.x, translation.y*s.y, translation.z*s.z
        ))
    }

    def translate(u: AnyVec3f) :Translation3f = {
        new Translation3f(translation + u)
    }

    def concatenate(m: AnyMat3x4f) :TransformMat3x4f = {
        import translation._

        new TransformMat3x4f(new Mat3x4f(
            m.m00, m.m10, m.m20,
            m.m01, m.m11, m.m21,
            m.m02, m.m12, m.m22,

            m.m00*x + m.m01*y + m.m02*z + m.m03,
            m.m10*x + m.m11*y + m.m12*z + m.m13,
            m.m20*x + m.m21*y + m.m22*z + m.m23
        ))
    }
    def concatenate(m: AnyMat3f) :TransformMat3x4f = {
        import translation._

        new TransformMat3x4f(new Mat3x4f(
            m.m00, m.m10, m.m20,
            m.m01, m.m11, m.m21,
            m.m02, m.m12, m.m22,

            m.m00*x + m.m01*y + m.m02*z,
            m.m10*x + m.m11*y + m.m12*z,
            m.m20*x + m.m21*y + m.m22*z
        ))
    }

    def transformPoint(p: AnyVec3f) = translation + p
    def transformVector(v: AnyVec3f) = Vec3f(v)

    def invert() :Translation3f = {
        new Translation3f(-translation)
    }
}

private[math] object Transform3fIdentity extends Transform3f {
    def toMatrix = Mat3x4f(1)

    def scale(s: Float) :Transform3f = new Scale3f(Vec3f(s))
    def scale(s: AnyVec3f) :Transform3f = new Scale3f(Vec3f(s))

    def translate(u: AnyVec3f) :Transform3f = new Translation3f(Vec3f(u))

    def concatenate(m: AnyMat3x4f) :Transform3f = {
        new TransformMat3x4f(Mat3x4f(m))
    }
    def concatenate(m: AnyMat3f) :Transform3f = {
        new TransformMat3f(Mat3f(m))
    }

    def transformPoint(p: AnyVec3f) :Vec3f = Vec3f(p)
    def transformVector(v: AnyVec3f) :Vec3f = Vec3f(v)

    def invert() :Transform3f = this
}

object Transform3f {
    
    val Identity: Transform3f = Transform3fIdentity

    def apply(m: AnyMat3f) :Transform3f =
        new TransformMat3f(Mat3f(m))

    def apply(m: AnyMat3x4f) :Transform3f =
        new TransformMat3x4f(Mat3x4f(m))

    def scale(s: Float) :Transform3f = Identity.scale(s)
    def scale(s: AnyVec3f) :Transform3f = Identity.scale(s)

    def rotate(q: AnyQuat4f) :Transform3f = Identity.rotate(q)
    def rotate(angle: Float, axis: AnyVec3f) :Transform3f = {
        Identity.rotate(angle, axis)
    }

    def rotateX(angle: Float) :Transform3f = Identity.rotateX(angle)
    def rotateY(angle: Float) :Transform3f = Identity.rotateX(angle)
    def rotateZ(angle: Float) :Transform3f = Identity.rotateX(angle)

    def translate(u: AnyVec3f) :Transform3f = Identity.translate(u)

    def apply(scale: AnyVec3f,
              rotation: AnyMat3f,
              translation: AnyVec3f)
    :Transform3f =
    {
        import scale.{x => sx, y => sy, z => sz}
        import rotation._
        import translation.{x => tx, y => ty, z => tz}

        new TransformMat3x4f(new Mat3x4f(
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
    def inverse(scale: AnyVec3f,
                rotation: AnyMat3f,
                translation: AnyVec3f)
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

        new TransformMat3x4f(new Mat3x4f(
            m00, m10, m20,
            m01, m11, m21,
            m02, m12, m22,
            -m00*tx - m01*ty - m02*tz,
            -m10*tx - m11*ty - m12*tz,
            -m20*tx - m21*ty - m22*tz
        ))
    }
}
