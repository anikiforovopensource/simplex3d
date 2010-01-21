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

    def apply(translation: AnyVec2f, angle: Float, scale: AnyVec2f) :Mat2x3f = {
        val m = Mat2x3f(rotationMatFrom(angle))
        m(0) *= scale.x
        m(1) *= scale.y
        m(2) = translation
        m
    }
    def apply(translation: AnyVec2f, rotation: AnyMat2f, scale: AnyVec2f)
    :Mat2x3f =
    {
        val m = Mat2x3f(rotation)
        m(0) *= scale.x
        m(1) *= scale.y
        m(2) = translation
        m
    }
    def apply(translation: AnyVec2f, angle: Float, scale: Float) :Mat2x3f = {
        val m = Mat2x3f(rotationMatFrom(angle))
        m(0) *= scale
        m(1) *= scale
        m(2) = translation
        m
    }
    def apply(translation: AnyVec2f, rotation: AnyMat2f, scale: Float)
    :Mat2x3f =
    {
        val m = Mat2x3f(rotation)
        m(0) *= scale
        m(1) *= scale
        m(2) = translation
        m
    }
    def apply(translation: AnyVec2f, angle: Float) :Mat2x3f = {
        val m = Mat2x3f(rotationMatFrom(angle))
        m(2) = translation
        m
    }
    def apply(translation: AnyVec2f, rotation: AnyMat2f) :Mat2x3f = {
        val m = Mat2x3f(rotation)
        m(2) = translation
        m
    }
    def apply(translation: AnyVec2f, scale: AnyVec2f) :Mat2x3f = {
        val m = Mat2x3f(scale.x)
        m.m11 = scale.y
        m(2) = translation
        m
    }
    def apply(angle: Float, scale: AnyVec2f) :Mat2x3f = {
        val m = Mat2x3f(rotationMatFrom(angle))
        m(0) *= scale.x
        m(1) *= scale.y
        m
    }
    def apply(angle: Float, scale: Float) :Mat2x3f = {
        val m = Mat2x3f(rotationMatFrom(angle))
        m(0) *= scale
        m(1) *= scale
        m
    }
    def apply(rotation: AnyMat2f, scale: AnyVec2f) :Mat2x3f = {
        val m = Mat2x3f(rotation)
        m(0) *= scale.x
        m(1) *= scale.y
        m
    }
    def apply(rotation: AnyMat2f, scale: Float) :Mat2x3f = {
        val m = Mat2x3f(rotation)
        m(0) *= scale
        m(1) *= scale
        m
    }
}

object InverseTransform2f {
    
    def apply(translation: AnyVec2f, angle: Float, scale: AnyVec2f) :Mat2x3f = {
        val m = Mat2x3f(rotationMatFrom(angle))
        m(0) /= scale.x
        m(1) /= scale.y
        transposeSubMat2(m)
        val t = m.transformPoint(-translation)
        m(2) = t
        m
    }
    def apply(translation: AnyVec2f, rotation: AnyMat2f, scale: AnyVec2f)
    :Mat2x3f =
    {
        val m = Mat2x3f(rotation)
        m(0) /= scale.x
        m(1) /= scale.y
        transposeSubMat2(m)
        val t = m.transformPoint(-translation)
        m(2) = t
        m
    }
    def apply(translation: AnyVec2f, angle: Float, scale: Float) :Mat2x3f = {
        val m = Mat2x3f(rotationMatFrom(angle))
        m(0) /= scale
        m(1) /= scale
        transposeSubMat2(m)
        val t = m.transformPoint(-translation)
        m(2) = t
        m
    }
    def apply(translation: AnyVec2f, rotation: AnyMat2f, scale: Float)
    :Mat2x3f =
    {
        val m = Mat2x3f(rotation)
        m(0) /= scale
        m(1) /= scale
        transposeSubMat2(m)
        val t = m.transformPoint(-translation)
        m(2) = t
        m
    }
    def apply(translation: AnyVec2f, angle: Float) :Mat2x3f = {
        val m = Mat2x3f(rotationMatFrom(angle))
        transposeSubMat2(m)
        val t = m.transformPoint(-translation)
        m(2) = t
        m
    }
    def apply(translation: AnyVec2f, rotation: AnyMat2f) :Mat2x3f = {
        val m = Mat2x3f(rotation)
        transposeSubMat2(m)
        val t = m.transformPoint(-translation)
        m(2) = t
        m
    }
    def apply(translation: AnyVec2f, scale: AnyVec2f) :Mat2x3f = {
        val m = Mat2x3f(1/scale.x)
        m.m11 = 1/scale.y
        val t = m.transformPoint(-translation)
        m(2) = t
        m
    }
    def apply(angle: Float, scale: AnyVec2f) :Mat2x3f = {
        val m = Mat2x3f(rotationMatFrom(angle))
        m(0) /= scale.x
        m(1) /= scale.y
        transposeSubMat2(m)
        m
    }
    def apply(angle: Float, scale: Float) :Mat2x3f = {
        val m = Mat2x3f(rotationMatFrom(angle))
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        transposeSubMat2(m)
        m
    }
    def apply(rotation: AnyMat2f, scale: AnyVec2f) :Mat2x3f = {
        val m = Mat2x3f(rotation)
        m(0) /= scale.x
        m(1) /= scale.y
        transposeSubMat2(m)
        m
    }
    def apply(rotation: AnyMat2f, scale: Float) :Mat2x3f = {
        val m = Mat2x3f(rotation)
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        transposeSubMat2(m)
        m
    }

    private def transposeSubMat2(m: Mat2x3f) {
        import m._

        val t10 = m10
        m10 = m01
        m01 = t10
    }
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
