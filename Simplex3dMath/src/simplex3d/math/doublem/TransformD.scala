/*
 * Simplex3D, Math module
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

package simplex3d.math.doublem

import DoubleMath._


/**
 * Prototype: all the methods here need to be optimized.
 *
 * @author Aleksey Nikiforov (lex)
 */
object TransformD {
    // 2-Dimensional
    def apply(translation: AnyVec2d, angle: Double, scale: AnyVec2d) :Mat2x3d = {
        val m = Mat2x3d(1)
        rotationMatFrom(angle, m)
        m(0) *= scale.x
        m(1) *= scale.y
        m(2) = translation
        m
    }
    def apply(translation: AnyVec2d, rotation: AnyMat2d, scale: AnyVec2d)
    :Mat2x3d =
    {
        val m = Mat2x3d(rotation)
        m(0) *= scale.x
        m(1) *= scale.y
        m(2) = translation
        m
    }
    def apply(translation: AnyVec2d, angle: Double, scale: Double) :Mat2x3d = {
        val m = Mat2x3d(1)
        rotationMatFrom(angle, m)
        m(0) *= scale
        m(1) *= scale
        m(2) = translation
        m
    }
    def apply(translation: AnyVec2d, rotation: AnyMat2d, scale: Double)
    :Mat2x3d =
    {
        val m = Mat2x3d(rotation)
        m(0) *= scale
        m(1) *= scale
        m(2) = translation
        m
    }
    def apply(translation: AnyVec2d, angle: Double) :Mat2x3d = {
        val m = Mat2x3d(1)
        rotationMatFrom(angle, m)
        m(2) = translation
        m
    }
    def apply(translation: AnyVec2d, rotation: AnyMat2d) :Mat2x3d = {
        val m = Mat2x3d(rotation)
        m(2) = translation
        m
    }
    def apply(translation: AnyVec2d, scale: AnyVec2d) :Mat2x3d = {
        val m = Mat2x3d(scale.x)
        m.m11 = scale.y
        m(2) = translation
        m
    }
    def apply(angle: Double, scale: AnyVec2d) :Mat2x3d = {
        val m = Mat2x3d(1)
        rotationMatFrom(angle, m)
        m(0) *= scale.x
        m(1) *= scale.y
        m
    }
    def apply(angle: Double, scale: Double) :Mat2x3d = {
        val m = Mat2x3d(1)
        rotationMatFrom(angle, m)
        m(0) *= scale
        m(1) *= scale
        m
    }
    def apply(rotation: AnyMat2d, scale: AnyVec2d) :Mat2x3d = {
        val m = Mat2x3d(rotation)
        m(0) *= scale.x
        m(1) *= scale.y
        m
    }
    def apply(rotation: AnyMat2d, scale: Double) :Mat2x3d = {
        val m = Mat2x3d(rotation)
        m(0) *= scale
        m(1) *= scale
        m
    }

    // 3-Dimensional
    def apply(translation: AnyVec3d, rotation: AnyQuat4d, scale: AnyVec3d)
    :Mat3x4d =
    {
        val m = Mat3x4d(1)
        rotationMatFrom(rotation, m)
        m(0) *= scale.x
        m(1) *= scale.y
        m(2) *= scale.z
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3d, angle: Double, axis: AnyVec3d, scale: AnyVec3d)
    :Mat3x4d =
    {
        val m = Mat3x4d(1)
        rotationMatFrom(angle, axis, m)
        m(0) *= scale.x
        m(1) *= scale.y
        m(2) *= scale.z
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3d, rotation: AnyMat3d, scale: AnyVec3d)
    :Mat3x4d =
    {
        val m = Mat3x4d(rotation)
        m(0) *= scale.x
        m(1) *= scale.y
        m(2) *= scale.z
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3d, rotation: AnyQuat4d, scale: Double)
    :Mat3x4d =
    {
        val m = Mat3x4d(1)
        rotationMatFrom(rotation, m)
        m(0) *= scale
        m(1) *= scale
        m(2) *= scale
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3d, angle: Double, axis: AnyVec3d, scale: Double)
    :Mat3x4d =
    {
        val m = Mat3x4d(1)
        rotationMatFrom(angle, axis, m)
        m(0) *= scale
        m(1) *= scale
        m(2) *= scale
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3d, rotation: AnyMat3d, scale: Double) :Mat3x4d = {
        val m = Mat3x4d(rotation)
        m(0) *= scale
        m(1) *= scale
        m(2) *= scale
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3d, rotation: AnyQuat4d) :Mat3x4d = {
        val m = Mat3x4d(1)
        rotationMatFrom(rotation, m)
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3d, angle: Double, axis: AnyVec3d) :Mat3x4d = {
        val m = Mat3x4d(1)
        rotationMatFrom(angle, axis, m)
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3d, rotation: AnyMat3d) :Mat3x4d = {
        val m = Mat3x4d(rotation)
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3d, scale: AnyVec3d) :Mat3x4d = {
        val m = Mat3x4d(scale.x)
        m.m11 = scale.y
        m.m22 = scale.z
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3d, scale: Double) :Mat3x4d = {
        val m = Mat3x4d(scale)
        m(3) = translation
        m
    }
    def apply(rotation: AnyQuat4d, scale: AnyVec3d) :Mat3x4d = {
        val m = Mat3x4d(1)
        rotationMatFrom(rotation, m)
        m(0) *= scale.x
        m(1) *= scale.y
        m(2) *= scale.z
        m
    }
    def apply(angle: Double, axis: AnyVec3d, scale: AnyVec3d) :Mat3x4d = {
        val m = Mat3x4d(1)
        rotationMatFrom(angle, axis, m)
        m(0) *= scale.x
        m(1) *= scale.y
        m(2) *= scale.z
        m
    }
    def apply(rotation: AnyMat3d, scale: AnyVec3d) :Mat3x4d = {
        val m = Mat3x4d(rotation)
        m(0) *= scale.x
        m(1) *= scale.y
        m(2) *= scale.z
        m
    }
    def apply(rotation: AnyQuat4d, scale: Double) :Mat3x4d = {
        val m = Mat3x4d(1)
        rotationMatFrom(rotation, m)
        m(0) *= scale
        m(1) *= scale
        m(2) *= scale
        m
    }
    def apply(angle: Double, axis: AnyVec3d, scale: Double) :Mat3x4d = {
        val m = Mat3x4d(1)
        rotationMatFrom(angle, axis, m)
        m(0) *= scale
        m(1) *= scale
        m(2) *= scale
        m
    }
    def apply(rotation: AnyMat3d, scale: Double) :Mat3x4d = {
        val m = Mat3x4d(rotation)
        m(0) *= scale
        m(1) *= scale
        m(2) *= scale
        m
    }
}

object InverseTransformD {
    // 2-Dimensional
    def apply(translation: AnyVec2d, angle: Double, scale: AnyVec2d) :Mat2x3d = {
        val m = Mat2x3d(1)
        rotationMatFrom(angle, m)
        m(0) /= scale.x
        m(1) /= scale.y
        transposeSubMat2d(m)
        val t = m.transformPoint(-translation)
        m(2) = t
        m
    }
    def apply(translation: AnyVec2d, rotation: AnyMat2d, scale: AnyVec2d)
    :Mat2x3d =
    {
        val m = Mat2x3d(rotation)
        m(0) /= scale.x
        m(1) /= scale.y
        transposeSubMat2d(m)
        val t = m.transformPoint(-translation)
        m(2) = t
        m
    }
    def apply(translation: AnyVec2d, angle: Double, scale: Double) :Mat2x3d = {
        val m = Mat2x3d(1)
        rotationMatFrom(angle, m)
        m(0) /= scale
        m(1) /= scale
        transposeSubMat2d(m)
        val t = m.transformPoint(-translation)
        m(2) = t
        m
    }
    def apply(translation: AnyVec2d, rotation: AnyMat2d, scale: Double)
    :Mat2x3d =
    {
        val m = Mat2x3d(rotation)
        m(0) /= scale
        m(1) /= scale
        transposeSubMat2d(m)
        val t = m.transformPoint(-translation)
        m(2) = t
        m
    }
    def apply(translation: AnyVec2d, angle: Double) :Mat2x3d = {
        val m = Mat2x3d(1)
        rotationMatFrom(angle, m)
        transposeSubMat2d(m)
        val t = m.transformPoint(-translation)
        m(2) = t
        m
    }
    def apply(translation: AnyVec2d, rotation: AnyMat2d) :Mat2x3d = {
        val m = Mat2x3d(rotation)
        transposeSubMat2d(m)
        val t = m.transformPoint(-translation)
        m(2) = t
        m
    }
    def apply(translation: AnyVec2d, scale: AnyVec2d) :Mat2x3d = {
        val m = Mat2x3d(1/scale.x)
        m.m11 = 1/scale.y
        val t = m.transformPoint(-translation)
        m(2) = t
        m
    }
    def apply(angle: Double, scale: AnyVec2d) :Mat2x3d = {
        val m = Mat2x3d(1)
        rotationMatFrom(angle, m)
        m(0) /= scale.x
        m(1) /= scale.y
        transposeSubMat2d(m)
        m
    }
    def apply(angle: Double, scale: Double) :Mat2x3d = {
        val m = Mat2x3d(1)
        rotationMatFrom(angle, m)
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        transposeSubMat2d(m)
        m
    }
    def apply(rotation: AnyMat2d, scale: AnyVec2d) :Mat2x3d = {
        val m = Mat2x3d(rotation)
        m(0) /= scale.x
        m(1) /= scale.y
        transposeSubMat2d(m)
        m
    }
    def apply(rotation: AnyMat2d, scale: Double) :Mat2x3d = {
        val m = Mat2x3d(rotation)
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        transposeSubMat2d(m)
        m
    }

    // 3-Dimensional
    def apply(translation: AnyVec3d, rotation: AnyQuat4d, scale: AnyVec3d)
    :Mat3x4d =
    {
        val m = Mat3x4d(1)
        rotationMatFrom(rotation, m)
        m(0) /= scale.x
        m(1) /= scale.y
        m(2) /= scale.z
        transposeSubMat3d(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3d, angle: Double, axis: AnyVec3d, scale: AnyVec3d)
    :Mat3x4d =
    {
        val m = Mat3x4d(1)
        rotationMatFrom(angle, axis, m)
        m(0) /= scale.x
        m(1) /= scale.y
        m(2) /= scale.z
        transposeSubMat3d(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3d, rotation: AnyMat3d, scale: AnyVec3d)
    :Mat3x4d =
    {
        val m = Mat3x4d(rotation)
        m(0) /= scale.x
        m(1) /= scale.y
        m(2) /= scale.z
        transposeSubMat3d(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3d, rotation: AnyQuat4d, scale: Double)
    :Mat3x4d =
    {
        val m = Mat3x4d(1)
        rotationMatFrom(rotation, m)
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        m(2) *= invs
        transposeSubMat3d(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3d, angle: Double, axis: AnyVec3d, scale: Double)
    :Mat3x4d =
    {
        val m = Mat3x4d(1)
        rotationMatFrom(angle, axis, m)
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        m(2) *= invs
        transposeSubMat3d(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3d, rotation: AnyMat3d, scale: Double) :Mat3x4d = {
        val m = Mat3x4d(rotation)
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        m(2) *= invs
        transposeSubMat3d(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3d, rotation: AnyQuat4d) :Mat3x4d = {
        val m = Mat3x4d(1)
        rotationMatFrom(rotation, m)
        transposeSubMat3d(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3d, angle: Double, axis: AnyVec3d) :Mat3x4d = {
        val m = Mat3x4d(1)
        rotationMatFrom(angle, axis, m)
        transposeSubMat3d(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3d, rotation: AnyMat3d) :Mat3x4d = {
        val m = Mat3x4d(rotation)
        transposeSubMat3d(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3d, scale: AnyVec3d) :Mat3x4d = {
        val m = Mat3x4d(1/scale.x)
        m.m11 = 1/scale.y
        m.m22 = 1/scale.z
        transposeSubMat3d(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3d, scale: Double) :Mat3x4d = {
        val invs = 1/scale
        val m = Mat3x4d(invs)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(rotation: AnyQuat4d, scale: AnyVec3d) :Mat3x4d = {
        val m = Mat3x4d(1)
        rotationMatFrom(rotation, m)
        m(0) /= scale.x
        m(1) /= scale.y
        m(2) /= scale.z
        transposeSubMat3d(m)
        m
    }
    def apply(angle: Double, axis: AnyVec3d, scale: AnyVec3d) :Mat3x4d = {
        val m = Mat3x4d(1)
        rotationMatFrom(angle, axis, m)
        m(0) /= scale.x
        m(1) /= scale.y
        m(2) /= scale.z
        transposeSubMat3d(m)
        m
    }
    def apply(rotation: AnyMat3d, scale: AnyVec3d) :Mat3x4d = {
        val m = Mat3x4d(rotation)
        m(0) /= scale.x
        m(1) /= scale.y
        m(2) /= scale.z
        transposeSubMat3d(m)
        m
    }
    def apply(rotation: AnyQuat4d, scale: Double) :Mat3x4d = {
        val m = Mat3x4d(1)
        rotationMatFrom(rotation, m)
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        m(2) *= invs
        transposeSubMat3d(m)
        m
    }
    def apply(angle: Double, axis: AnyVec3d, scale: Double) :Mat3x4d = {
        val m = Mat3x4d(1)
        rotationMatFrom(angle, axis, m)
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        m(2) *= invs
        transposeSubMat3d(m)
        m
    }
    def apply(rotation: AnyMat3d, scale: Double) :Mat3x4d = {
        val m = Mat3x4d(rotation)
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        m(2) *= invs
        transposeSubMat3d(m)
        m
    }
}

object TranslationD {
    def apply(t: AnyVec2d) :Mat2x3d = {
        val m = Mat2x3d(1)
        m(2) = t
        m
    }

    def apply(t: AnyVec3d) :Mat3x4d = {
        val m = Mat3x4d(1)
        m(3) = t
        m
    }
}

object RotationD {
    def apply(angle: Double) :Mat2x3d = {
        val m = Mat2x3d(1)
        rotationMatFrom(angle, m)
        m
    }
    def apply(q: AnyQuat4d) :Mat3x4d = {
        val m = Mat3x4d(1)
        rotationMatFrom(q, m)
        m
    }
    def apply(angle: Double, axis: AnyVec3d) :Mat3x4d = {
        val m = Mat3x4d(1)
        rotationMatFrom(angle, axis, m)
        m
    }
}

object ScaleD {
    def apply(s: AnyVec2d) :Mat2x3d = {
        val m = Mat2x3d(s.x)
        m.m11 = s.y
        m
    }

    def apply(s: AnyVec3d) :Mat3x4d = {
        val m = Mat3x4d(s.x)
        m.m11 = s.y
        m.m22 = s.z
        m
    }
}
