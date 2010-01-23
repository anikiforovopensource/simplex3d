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
object Transform2d {

    def apply(translation: AnyVec2d,
              rotation: AnyMat2d,
              scale: AnyVec2d)
    :Mat2x3d =
    {
        import rotation._
        import translation.{x => tx, y => ty}
        import scale.{x => sx, y => sy}

        new Mat2x3d(
            m00*sx, m10*sx,
            m01*sy, m11*sy,
            tx, ty
        )
    }

    def apply(rotation: AnyMat2d,
              scale: AnyVec2d)
    :Mat2x3d = apply(Vec2d.Zero, rotation, scale)

    def apply(translation: AnyVec2d,
              scale: AnyVec2d)
    :Mat2x3d = apply(translation, Mat2d.Identity, scale)

    def apply(translation: AnyVec2d,
              rotation: AnyMat2d)
    :Mat2x3d = apply(translation, rotation, Vec2d.One)

    def apply(translation: AnyVec2d,
              angle: Double,
              scale: AnyVec2d)
    :Mat2x3d = apply(translation, rotationMatFrom(angle), scale)

    def apply(angle: Double,
              scale: AnyVec2d)
    :Mat2x3d = apply(Vec2d.Zero, rotationMatFrom(angle), scale)

    def apply(translation: AnyVec2d,
              angle: Double)
    :Mat2x3d = apply(translation, rotationMatFrom(angle), Vec2d.One)
}

object InverseTransform2d {

    /**
     * @param rotation must be an orthogonal matrix (matrix that represents
     * an unscaled rotation) to achieve the desired result
     */
    def apply(translation: AnyVec2d,
              rotation: AnyMat2d,
              scale: AnyVec2d)
    :Mat2x3d =
    {
        import translation.{x => tx, y => ty}

        val sx = 1/scale.x
        val sy = 1/scale.y

        val m00 = rotation.m00*sx
        val m10 = rotation.m01*sy
        val m01 = rotation.m10*sx
        val m11 = rotation.m11*sy

        new Mat2x3d(
            m00, m10,
            m01, m11,
            -m00*tx - m01*ty,
            -m10*tx - m11*ty
        )
    }

    /**
     * @param rotation must be an orthogonal matrix (matrix that represents
     * an unscaled rotation) to achieve the desired result
     */
    def apply(rotation: AnyMat2d,
              scale: AnyVec2d)
    :Mat2x3d = apply(Vec2d.Zero, rotation, scale)

    def apply(translation: AnyVec2d,
              scale: AnyVec2d)
    :Mat2x3d = apply(translation, Mat2d.Identity, scale)

    def apply(translation: AnyVec2d,
              rotation: AnyMat2d)
    :Mat2x3d = apply(translation, rotation, Vec2d.One)

    def apply(translation: AnyVec2d,
              angle: Double,
              scale: AnyVec2d)
    :Mat2x3d = apply(translation, rotationMatFrom(angle), scale)

    def apply(angle: Double,
              scale: AnyVec2d)
    :Mat2x3d = apply(Vec2d.Zero, rotationMatFrom(angle), scale)

    def apply(translation: AnyVec2d,
              angle: Double)
    :Mat2x3d = apply(translation, rotationMatFrom(angle), Vec2d.One)
}

object Translation2d {
    def apply(t: AnyVec2d) :Mat2x3d = {
        val m = Mat2x3d(1)
        m(2) = t
        m
    }
}

object Rotation2d {
    def apply(angle: Double) :Mat2x3d = Mat2x3d(rotationMatFrom(angle))
}

object Scale2d {
    def apply(s: Double) :Mat2x3d = {
        val m = Mat2x3d(s)
        m
    }
    def apply(s: AnyVec2d) :Mat2x3d = {
        val m = Mat2x3d(s.x)
        m.m11 = s.y
        m
    }
}
