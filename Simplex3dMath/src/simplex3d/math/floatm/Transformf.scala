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
object Transformf {
    // 2-Dimensional
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

    // 3-Dimensional
    def apply(translation: AnyVec3f, rotation: AnyQuat4f, scale: AnyVec3f)
    :Mat3x4f =
    {
        val m = Mat3x4f(rotationMatFrom(rotation))
        m(0) *= scale.x
        m(1) *= scale.y
        m(2) *= scale.z
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3f, angle: Float, axis: AnyVec3f, scale: AnyVec3f)
    :Mat3x4f =
    {
        val m = Mat3x4f(rotationMatFrom(angle, axis))
        m(0) *= scale.x
        m(1) *= scale.y
        m(2) *= scale.z
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3f, rotation: AnyMat3f, scale: AnyVec3f)
    :Mat3x4f =
    {
        val m = Mat3x4f(rotation)
        m(0) *= scale.x
        m(1) *= scale.y
        m(2) *= scale.z
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3f, rotation: AnyQuat4f, scale: Float)
    :Mat3x4f =
    {
        val m = Mat3x4f(rotationMatFrom(rotation))
        m(0) *= scale
        m(1) *= scale
        m(2) *= scale
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3f, angle: Float, axis: AnyVec3f, scale: Float)
    :Mat3x4f =
    {
        val m = Mat3x4f(rotationMatFrom(angle, axis))
        m(0) *= scale
        m(1) *= scale
        m(2) *= scale
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3f, rotation: AnyMat3f, scale: Float) :Mat3x4f = {
        val m = Mat3x4f(rotation)
        m(0) *= scale
        m(1) *= scale
        m(2) *= scale
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3f, rotation: AnyQuat4f) :Mat3x4f = {
        val m = Mat3x4f(rotationMatFrom(rotation))
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3f, angle: Float, axis: AnyVec3f) :Mat3x4f = {
        val m = Mat3x4f(rotationMatFrom(angle, axis))
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3f, rotation: AnyMat3f) :Mat3x4f = {
        val m = Mat3x4f(rotation)
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3f, scale: AnyVec3f) :Mat3x4f = {
        val m = Mat3x4f(scale.x)
        m.m11 = scale.y
        m.m22 = scale.z
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3f, scale: Float) :Mat3x4f = {
        val m = Mat3x4f(scale)
        m(3) = translation
        m
    }
    def apply(rotation: AnyQuat4f, scale: AnyVec3f) :Mat3x4f = {
        val m = Mat3x4f(rotationMatFrom(rotation))
        m(0) *= scale.x
        m(1) *= scale.y
        m(2) *= scale.z
        m
    }
    def apply(angle: Float, axis: AnyVec3f, scale: AnyVec3f) :Mat3x4f = {
        val m = Mat3x4f(rotationMatFrom(angle, axis))
        m(0) *= scale.x
        m(1) *= scale.y
        m(2) *= scale.z
        m
    }
    def apply(rotation: AnyMat3f, scale: AnyVec3f) :Mat3x4f = {
        val m = Mat3x4f(rotation)
        m(0) *= scale.x
        m(1) *= scale.y
        m(2) *= scale.z
        m
    }
    def apply(rotation: AnyQuat4f, scale: Float) :Mat3x4f = {
        val m = Mat3x4f(rotationMatFrom(rotation))
        m(0) *= scale
        m(1) *= scale
        m(2) *= scale
        m
    }
    def apply(angle: Float, axis: AnyVec3f, scale: Float) :Mat3x4f = {
        val m = Mat3x4f(rotationMatFrom(angle, axis))
        m(0) *= scale
        m(1) *= scale
        m(2) *= scale
        m
    }
    def apply(rotation: AnyMat3f, scale: Float) :Mat3x4f = {
        val m = Mat3x4f(rotation)
        m(0) *= scale
        m(1) *= scale
        m(2) *= scale
        m
    }
}

object InverseTransformf {
    // 2-Dimensional
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

    // 3-Dimensional
    def apply(translation: AnyVec3f, rotation: AnyQuat4f, scale: AnyVec3f)
    :Mat3x4f =
    {
        val m = Mat3x4f(rotationMatFrom(rotation))
        m(0) /= scale.x
        m(1) /= scale.y
        m(2) /= scale.z
        transposeSubMat3(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3f, angle: Float, axis: AnyVec3f, scale: AnyVec3f)
    :Mat3x4f =
    {
        val m = Mat3x4f(rotationMatFrom(angle, axis))
        m(0) /= scale.x
        m(1) /= scale.y
        m(2) /= scale.z
        transposeSubMat3(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3f, rotation: AnyMat3f, scale: AnyVec3f)
    :Mat3x4f =
    {
        val m = Mat3x4f(rotation)
        m(0) /= scale.x
        m(1) /= scale.y
        m(2) /= scale.z
        transposeSubMat3(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3f, rotation: AnyQuat4f, scale: Float)
    :Mat3x4f =
    {
        val m = Mat3x4f(rotationMatFrom(rotation))
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        m(2) *= invs
        transposeSubMat3(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3f, angle: Float, axis: AnyVec3f, scale: Float)
    :Mat3x4f =
    {
        val m = Mat3x4f(rotationMatFrom(angle, axis))
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        m(2) *= invs
        transposeSubMat3(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3f, rotation: AnyMat3f, scale: Float) :Mat3x4f = {
        val m = Mat3x4f(rotation)
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        m(2) *= invs
        transposeSubMat3(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3f, rotation: AnyQuat4f) :Mat3x4f = {
        val m = Mat3x4f(rotationMatFrom(rotation))
        transposeSubMat3(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3f, angle: Float, axis: AnyVec3f) :Mat3x4f = {
        val m = Mat3x4f(rotationMatFrom(angle, axis))
        transposeSubMat3(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3f, rotation: AnyMat3f) :Mat3x4f = {
        val m = Mat3x4f(rotation)
        transposeSubMat3(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3f, scale: AnyVec3f) :Mat3x4f = {
        val m = Mat3x4f(1/scale.x)
        m.m11 = 1/scale.y
        m.m22 = 1/scale.z
        transposeSubMat3(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3f, scale: Float) :Mat3x4f = {
        val invs = 1/scale
        val m = Mat3x4f(invs)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(rotation: AnyQuat4f, scale: AnyVec3f) :Mat3x4f = {
        val m = Mat3x4f(rotationMatFrom(rotation))
        m(0) /= scale.x
        m(1) /= scale.y
        m(2) /= scale.z
        transposeSubMat3(m)
        m
    }
    def apply(angle: Float, axis: AnyVec3f, scale: AnyVec3f) :Mat3x4f = {
        val m = Mat3x4f(rotationMatFrom(angle, axis))
        m(0) /= scale.x
        m(1) /= scale.y
        m(2) /= scale.z
        transposeSubMat3(m)
        m
    }
    def apply(rotation: AnyMat3f, scale: AnyVec3f) :Mat3x4f = {
        val m = Mat3x4f(rotation)
        m(0) /= scale.x
        m(1) /= scale.y
        m(2) /= scale.z
        transposeSubMat3(m)
        m
    }
    def apply(rotation: AnyQuat4f, scale: Float) :Mat3x4f = {
        val m = Mat3x4f(rotationMatFrom(rotation))
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        m(2) *= invs
        transposeSubMat3(m)
        m
    }
    def apply(angle: Float, axis: AnyVec3f, scale: Float) :Mat3x4f = {
        val m = Mat3x4f(rotationMatFrom(angle, axis))
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        m(2) *= invs
        transposeSubMat3(m)
        m
    }
    def apply(rotation: AnyMat3f, scale: Float) :Mat3x4f = {
        val m = Mat3x4f(rotation)
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        m(2) *= invs
        transposeSubMat3(m)
        m
    }

    private def transposeSubMat2(m: Mat2x3f) {
        import m._

        val t10 = m10
        m10 = m01
        m01 = t10
    }

    private def transposeSubMat3(m: Mat3x4f) {
        import m._

        val t10 = m10
        val t20 = m20
        val t21 = m21
        m10 = m01
        m20 = m02
        m01 = t10
        m21 = m12
        m02 = t20
        m12 = t21
    }
}

object Translationf {
    def apply(t: AnyVec2f) :Mat2x3f = {
        val m = Mat2x3f(1)
        m(2) = t
        m
    }

    def apply(t: AnyVec3f) :Mat3x4f = {
        val m = Mat3x4f(1)
        m(3) = t
        m
    }
}

object Rotationf {
    def apply(angle: Float) :Mat2f = rotationMatFrom(angle)
    def apply(q: AnyQuat4f) :Mat3f = rotationMatFrom(q)
    def apply(angle: Float, axis: AnyVec3f) = rotationMatFrom(angle, axis)
}

object Scalef {
    def apply(s: AnyVec2f) :Mat2x3f = {
        val m = Mat2x3f(s.x)
        m.m11 = s.y
        m
    }

    def apply(s: AnyVec3f) :Mat3x4f = {
        val m = Mat3x4f(s.x)
        m.m11 = s.y
        m.m22 = s.z
        m
    }
}
