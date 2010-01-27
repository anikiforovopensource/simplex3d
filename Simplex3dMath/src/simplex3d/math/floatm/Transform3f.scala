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
sealed abstract class AnyTransform3f(val matrix: AnyMat3x4f) {
    import matrix._

    def scale(s: Float) :Transform3f = {
        new Transform3f(matrix*s)
    }
    def scale(s: AnyVec3f) :Transform3f = {
        new Transform3f(new Mat3x4f(
            m00*s.x, m10*s.y, m20*s.z,
            m01*s.x, m11*s.y, m21*s.z,
            m02*s.x, m12*s.y, m22*s.z,
            m03*s.x, m13*s.y, m23*s.z
        ))
    }

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

    def translate(u: AnyVec3f) :Transform3f = {
        new Transform3f(new Mat3x4f(
            m00, m10, m20,
            m01, m11, m21,
            m02, m12, m22,
            m03 + u.x, m13 + u.y, m23 + u.z
        ))
    }

    def concatenate(t: AnyTransform3f) :Transform3f = {
        concatenate(t.matrix)
    }
    def concatenate(m: AnyMat3x4f) :Transform3f = {
        new Transform3f(new Mat3x4f(
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
    def concatenate(m: AnyMat3f) :Transform3f = {
        new Transform3f(m*matrix)
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

    def invert() :Transform3f = {
        new Transform3f(inverse(matrix))
    }

    def ==(t: AnyTransform3f) :Boolean = {
        if (t eq null) false
        else matrix == t.matrix
    }

    def !=(t: AnyTransform3f) :Boolean = !(this == t)

    override def equals(other: Any) :Boolean = {
        other match {
            case u: Transform3f => this == u
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


final class ConstTransform3f private[math] (override val matrix: ConstMat3x4f)
extends AnyTransform3f(matrix)

object ConstTransform3f {
    def apply(m: AnyMat3x4f) :ConstTransform3f =
        new ConstTransform3f(ConstMat3x4f(m))

    def apply(t: AnyTransform3f) :ConstTransform3f =
        new ConstTransform3f(ConstMat3x4f(t.matrix))

    implicit def toConst(t: Transform3f) = ConstTransform3f(t)
}


final class Transform3f private[math] (override val matrix: Mat3x4f)
extends AnyTransform3f(matrix)

object Transform3f {
    
    val Identity: ConstTransform3f = Transform3f()

    def apply() :Transform3f = new Transform3f(Mat3x4f(1))
    def apply(m: AnyMat3x4f) :Transform3f = new Transform3f(Mat3x4f(m))
    
    def apply(t: AnyTransform3f) :Transform3f =
        new Transform3f(Mat3x4f(t.matrix))

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

    implicit def toMutable(t: ConstTransform3f) = Transform3f(t)
}
