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
object Transform3d {

    def apply(translation: AnyVec3d,
              rotation: AnyMat3d,
              scale: AnyVec3d)
    :Mat3x4d =
    {
        import rotation._
        import translation.{x => tx, y => ty, z => tz}
        import scale.{x => sx, y => sy, z => sz}

        new Mat3x4d(
            m00*sx, m10*sx, m20*sx,
            m01*sy, m11*sy, m21*sy,
            m02*sz, m12*sz, m22*sz,
            tx, ty, tz
        )
    }

    def apply(rotation: AnyMat3d,
              scale: AnyVec3d)
    :Mat3x4d = apply(Vec3d.Zero, rotation, scale)

    def apply(translation: AnyVec3d,
              scale: AnyVec3d)
    :Mat3x4d = apply(translation, Mat3d.Identity, scale)

    def apply(translation: AnyVec3d,
              rotation: AnyMat3d)
    :Mat3x4d = apply(translation, rotation, Vec3d.One)

    def apply(translation: AnyVec3d,
              rotation: AnyQuat4d,
              scale: AnyVec3d)
    :Mat3x4d = apply(translation, rotationMat(rotation), scale)

    def apply(rotation: AnyQuat4d,
              scale: AnyVec3d)
    :Mat3x4d = apply(Vec3d.Zero, rotationMat(rotation), scale)

    def apply(translation: AnyVec3d,
              rotation: AnyQuat4d)
    :Mat3x4d = apply(translation, rotationMat(rotation), Vec3d.One)

    def apply(translation: AnyVec3d,
              angle: Double, axis: AnyVec3d,
              scale: AnyVec3d)
    :Mat3x4d = apply(translation, rotationMat(angle, axis), scale)

    def apply(angle: Double, axis: AnyVec3d,
              scale: AnyVec3d)
    :Mat3x4d = apply(Vec3d.Zero, rotationMat(angle, axis), scale)

    def apply(translation: AnyVec3d,
              angle: Double, axis: AnyVec3d)
    :Mat3x4d = apply(translation, rotationMat(angle, axis), Vec3d.One)
}

object InverseTransform3d {

    /**
     * @param rotation must be an orthogonal matrix (matrix that represents
     * an unscaled rotation) to achieve the desired result
     */
    def apply(translation: AnyVec3d,
              rotation: AnyMat3d,
              scale: AnyVec3d)
    :Mat3x4d =
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

        new Mat3x4d(
            m00, m10, m20,
            m01, m11, m21,
            m02, m12, m22,
            -m00*tx - m01*ty - m02*tz,
            -m10*tx - m11*ty - m12*tz,
            -m20*tx - m21*ty - m22*tz
        )
    }

    /**
     * @param rotation must be an orthogonal matrix (matrix that represents
     * an unscaled rotation) to achieve the desired result
     */
    def apply(rotation: AnyMat3d,
              scale: AnyVec3d)
    :Mat3x4d = apply(Vec3d.Zero, rotation, scale)

    def apply(translation: AnyVec3d,
              scale: AnyVec3d)
    :Mat3x4d = apply(translation, Mat3d.Identity, scale)

    def apply(translation: AnyVec3d,
              rotation: AnyMat3d)
    :Mat3x4d = apply(translation, rotation, Vec3d.One)

    def apply(translation: AnyVec3d,
              rotation: AnyQuat4d,
              scale: AnyVec3d)
    :Mat3x4d = apply(translation, rotationMat(rotation), scale)

    def apply(rotation: AnyQuat4d,
              scale: AnyVec3d)
    :Mat3x4d = apply(Vec3d.Zero, rotationMat(rotation), scale)

    def apply(translation: AnyVec3d,
              rotation: AnyQuat4d)
    :Mat3x4d = apply(translation, rotationMat(rotation), Vec3d.One)

    def apply(translation: AnyVec3d,
              angle: Double, axis: AnyVec3d,
              scale: AnyVec3d)
    :Mat3x4d = apply(translation, rotationMat(angle, axis), scale)

    def apply(angle: Double, axis: AnyVec3d,
              scale: AnyVec3d)
    :Mat3x4d = apply(Vec3d.Zero, rotationMat(angle, axis), scale)

    def apply(translation: AnyVec3d,
              angle: Double, axis: AnyVec3d)
    :Mat3x4d = apply(translation, rotationMat(angle, axis), Vec3d.One)
}

object Translation3d {
    def apply(t: AnyVec3d) :Mat3x4d = {
        val m = Mat3x4d(1)
        m(3) = t
        m
    }
}

object Rotation3d {
    def apply(q: AnyQuat4d) :Mat3x4d = Mat3x4d(rotationMat(q))
    def apply(angle: Double, axis: AnyVec3d) :Mat3x4d = {
        Mat3x4d(rotationMat(angle, axis))
    }
}

object Scale3d {
    def apply(s: Double) :Mat3x4d = {
        val m = Mat3x4d(s)
        m
    }
    def apply(s: AnyVec3d) :Mat3x4d = {
        val m = Mat3x4d(s.x)
        m.m11 = s.y
        m.m22 = s.z
        m
    }
}
