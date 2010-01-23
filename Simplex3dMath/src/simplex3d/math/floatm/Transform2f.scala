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
object Transform2f {

    def apply(translation: AnyVec2f,
              rotation: AnyMat2f,
              scale: AnyVec2f)
    :Mat2x3f =
    {
        import rotation._
        import translation.{x => tx, y => ty}
        import scale.{x => sx, y => sy}

        new Mat2x3f(
            m00*sx, m10*sx,
            m01*sy, m11*sy,
            tx, ty
        )
    }

    def apply(rotation: AnyMat2f,
              scale: AnyVec2f)
    :Mat2x3f = apply(Vec2f.Zero, rotation, scale)

    def apply(translation: AnyVec2f,
              scale: AnyVec2f)
    :Mat2x3f = apply(translation, Mat2f.Identity, scale)

    def apply(translation: AnyVec2f,
              rotation: AnyMat2f)
    :Mat2x3f = apply(translation, rotation, Vec2f.One)

    def apply(translation: AnyVec2f,
              angle: Float,
              scale: AnyVec2f)
    :Mat2x3f = apply(translation, rotationMatFrom(angle), scale)

    def apply(angle: Float,
              scale: AnyVec2f)
    :Mat2x3f = apply(Vec2f.Zero, rotationMatFrom(angle), scale)

    def apply(translation: AnyVec2f,
              angle: Float)
    :Mat2x3f = apply(translation, rotationMatFrom(angle), Vec2f.One)
}

object InverseTransform2f {

    /**
     * @param rotation must be an orthogonal matrix (matrix that represents
     * an unscaled rotation) to achieve the desired result
     */
    def apply(translation: AnyVec2f,
              rotation: AnyMat2f,
              scale: AnyVec2f)
    :Mat2x3f =
    {
        import translation.{x => tx, y => ty}

        val sx = 1/scale.x
        val sy = 1/scale.y

        val m00 = rotation.m00*sx
        val m10 = rotation.m01*sy
        val m01 = rotation.m10*sx
        val m11 = rotation.m11*sy

        new Mat2x3f(
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
    def apply(rotation: AnyMat2f,
              scale: AnyVec2f)
    :Mat2x3f = apply(Vec2f.Zero, rotation, scale)

    def apply(translation: AnyVec2f,
              scale: AnyVec2f)
    :Mat2x3f = apply(translation, Mat2f.Identity, scale)

    def apply(translation: AnyVec2f,
              rotation: AnyMat2f)
    :Mat2x3f = apply(translation, rotation, Vec2f.One)

    def apply(translation: AnyVec2f,
              angle: Float,
              scale: AnyVec2f)
    :Mat2x3f = apply(translation, rotationMatFrom(angle), scale)

    def apply(angle: Float,
              scale: AnyVec2f)
    :Mat2x3f = apply(Vec2f.Zero, rotationMatFrom(angle), scale)

    def apply(translation: AnyVec2f,
              angle: Float)
    :Mat2x3f = apply(translation, rotationMatFrom(angle), Vec2f.One)
}

object Translation2f {
    def apply(t: AnyVec2f) :Mat2x3f = {
        val m = Mat2x3f(1)
        m(2) = t
        m
    }
}

object Rotation2f {
    def apply(angle: Float) :Mat2x3f = Mat2x3f(rotationMatFrom(angle))
}

object Scale2f {
    def apply(s: Float) :Mat2x3f = {
        val m = Mat2x3f(s)
        m
    }
    def apply(s: AnyVec2f) :Mat2x3f = {
        val m = Mat2x3f(s.x)
        m.m11 = s.y
        m
    }
}
