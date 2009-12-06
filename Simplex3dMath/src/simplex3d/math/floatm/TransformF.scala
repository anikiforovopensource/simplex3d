/*
 * Simplex3D, FloatMath module
 * Copyright (C) 2009 Simplex3D team
 *
 * This file is part of Simplex3d.
 *
 * Simplex3d is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3d is distributed in the hope that it will be useful,
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
 * Prototype: all the methods here need to be optimized.
 *
 * @author Aleksey Nikiforov (lex)
 */
object TransformF {
    // 2-Dimensional
    def apply(translation: AnyVec2f, angle: Float, scale: AnyVec2f) :Mat2x3f = {
        val m = Mat2x3f(1)
        rotationMatFrom(angle, m)
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
        val m = Mat2x3f(1)
        rotationMatFrom(angle, m)
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
        val m = Mat2x3f(1)
        rotationMatFrom(angle, m)
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
        val m = Mat2x3f(1)
        rotationMatFrom(angle, m)
        m(0) *= scale.x
        m(1) *= scale.y
        m
    }
    def apply(angle: Float, scale: Float) :Mat2x3f = {
        val m = Mat2x3f(1)
        rotationMatFrom(angle, m)
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
        val m = Mat3x4f(1)
        rotationMatFrom(rotation, m)
        m(0) *= scale.x
        m(1) *= scale.y
        m(2) *= scale.z
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3f, angle: Float, axis: AnyVec3f, scale: AnyVec3f)
    :Mat3x4f =
    {
        val m = Mat3x4f(1)
        rotationMatFrom(angle, axis, m)
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
        val m = Mat3x4f(1)
        rotationMatFrom(rotation, m)
        m(0) *= scale
        m(1) *= scale
        m(2) *= scale
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3f, angle: Float, axis: AnyVec3f, scale: Float)
    :Mat3x4f =
    {
        val m = Mat3x4f(1)
        rotationMatFrom(angle, axis, m)
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
        val m = Mat3x4f(1)
        rotationMatFrom(rotation, m)
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3f, angle: Float, axis: AnyVec3f) :Mat3x4f = {
        val m = Mat3x4f(1)
        rotationMatFrom(angle, axis, m)
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
        val m = Mat3x4f(1)
        rotationMatFrom(rotation, m)
        m(0) *= scale.x
        m(1) *= scale.y
        m(2) *= scale.z
        m
    }
    def apply(angle: Float, axis: AnyVec3f, scale: AnyVec3f) :Mat3x4f = {
        val m = Mat3x4f(1)
        rotationMatFrom(angle, axis, m)
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
        val m = Mat3x4f(1)
        rotationMatFrom(rotation, m)
        m(0) *= scale
        m(1) *= scale
        m(2) *= scale
        m
    }
    def apply(angle: Float, axis: AnyVec3f, scale: Float) :Mat3x4f = {
        val m = Mat3x4f(1)
        rotationMatFrom(angle, axis, m)
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

object InverseTransformF {
    // 2-Dimensional
    def apply(translation: AnyVec2f, angle: Float, scale: AnyVec2f) :Mat2x3f = {
        val m = Mat2x3f(1)
        rotationMatFrom(angle, m)
        m(0) /= scale.x
        m(1) /= scale.y
        transposeSubMat2f(m)
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
        transposeSubMat2f(m)
        val t = m.transformPoint(-translation)
        m(2) = t
        m
    }
    def apply(translation: AnyVec2f, angle: Float, scale: Float) :Mat2x3f = {
        val m = Mat2x3f(1)
        rotationMatFrom(angle, m)
        m(0) /= scale
        m(1) /= scale
        transposeSubMat2f(m)
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
        transposeSubMat2f(m)
        val t = m.transformPoint(-translation)
        m(2) = t
        m
    }
    def apply(translation: AnyVec2f, angle: Float) :Mat2x3f = {
        val m = Mat2x3f(1)
        rotationMatFrom(angle, m)
        transposeSubMat2f(m)
        val t = m.transformPoint(-translation)
        m(2) = t
        m
    }
    def apply(translation: AnyVec2f, rotation: AnyMat2f) :Mat2x3f = {
        val m = Mat2x3f(rotation)
        transposeSubMat2f(m)
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
        val m = Mat2x3f(1)
        rotationMatFrom(angle, m)
        m(0) /= scale.x
        m(1) /= scale.y
        transposeSubMat2f(m)
        m
    }
    def apply(angle: Float, scale: Float) :Mat2x3f = {
        val m = Mat2x3f(1)
        rotationMatFrom(angle, m)
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        transposeSubMat2f(m)
        m
    }
    def apply(rotation: AnyMat2f, scale: AnyVec2f) :Mat2x3f = {
        val m = Mat2x3f(rotation)
        m(0) /= scale.x
        m(1) /= scale.y
        transposeSubMat2f(m)
        m
    }
    def apply(rotation: AnyMat2f, scale: Float) :Mat2x3f = {
        val m = Mat2x3f(rotation)
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        transposeSubMat2f(m)
        m
    }

    // 3-Dimensional
    def apply(translation: AnyVec3f, rotation: AnyQuat4f, scale: AnyVec3f)
    :Mat3x4f =
    {
        val m = Mat3x4f(1)
        rotationMatFrom(rotation, m)
        m(0) /= scale.x
        m(1) /= scale.y
        m(2) /= scale.z
        transposeSubMat3f(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3f, angle: Float, axis: AnyVec3f, scale: AnyVec3f)
    :Mat3x4f =
    {
        val m = Mat3x4f(1)
        rotationMatFrom(angle, axis, m)
        m(0) /= scale.x
        m(1) /= scale.y
        m(2) /= scale.z
        transposeSubMat3f(m)
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
        transposeSubMat3f(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3f, rotation: AnyQuat4f, scale: Float)
    :Mat3x4f =
    {
        val m = Mat3x4f(1)
        rotationMatFrom(rotation, m)
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        m(2) *= invs
        transposeSubMat3f(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3f, angle: Float, axis: AnyVec3f, scale: Float)
    :Mat3x4f =
    {
        val m = Mat3x4f(1)
        rotationMatFrom(angle, axis, m)
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        m(2) *= invs
        transposeSubMat3f(m)
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
        transposeSubMat3f(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3f, rotation: AnyQuat4f) :Mat3x4f = {
        val m = Mat3x4f(1)
        rotationMatFrom(rotation, m)
        transposeSubMat3f(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3f, angle: Float, axis: AnyVec3f) :Mat3x4f = {
        val m = Mat3x4f(1)
        rotationMatFrom(angle, axis, m)
        transposeSubMat3f(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3f, rotation: AnyMat3f) :Mat3x4f = {
        val m = Mat3x4f(rotation)
        transposeSubMat3f(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3f, scale: AnyVec3f) :Mat3x4f = {
        val m = Mat3x4f(1/scale.x)
        m.m11 = 1/scale.y
        m.m22 = 1/scale.z
        transposeSubMat3f(m)
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
        val m = Mat3x4f(1)
        rotationMatFrom(rotation, m)
        m(0) /= scale.x
        m(1) /= scale.y
        m(2) /= scale.z
        transposeSubMat3f(m)
        m
    }
    def apply(angle: Float, axis: AnyVec3f, scale: AnyVec3f) :Mat3x4f = {
        val m = Mat3x4f(1)
        rotationMatFrom(angle, axis, m)
        m(0) /= scale.x
        m(1) /= scale.y
        m(2) /= scale.z
        transposeSubMat3f(m)
        m
    }
    def apply(rotation: AnyMat3f, scale: AnyVec3f) :Mat3x4f = {
        val m = Mat3x4f(rotation)
        m(0) /= scale.x
        m(1) /= scale.y
        m(2) /= scale.z
        transposeSubMat3f(m)
        m
    }
    def apply(rotation: AnyQuat4f, scale: Float) :Mat3x4f = {
        val m = Mat3x4f(1)
        rotationMatFrom(rotation, m)
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        m(2) *= invs
        transposeSubMat3f(m)
        m
    }
    def apply(angle: Float, axis: AnyVec3f, scale: Float) :Mat3x4f = {
        val m = Mat3x4f(1)
        rotationMatFrom(angle, axis, m)
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        m(2) *= invs
        transposeSubMat3f(m)
        m
    }
    def apply(rotation: AnyMat3f, scale: Float) :Mat3x4f = {
        val m = Mat3x4f(rotation)
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        m(2) *= invs
        transposeSubMat3f(m)
        m
    }
}

object TranslationF {
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

object RotationF {
    def apply(angle: Float) :Mat2x3f = {
        val m = Mat2x3f(1)
        rotationMatFrom(angle, m)
        m
    }
    def apply(q: AnyQuat4f) :Mat3x4f = {
        val m = Mat3x4f(1)
        rotationMatFrom(q, m)
        m
    }
    def apply(angle: Float, axis: AnyVec3f) :Mat3x4f = {
        val m = Mat3x4f(1)
        rotationMatFrom(angle, axis, m)
        m
    }
}

object ScaleF {
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
