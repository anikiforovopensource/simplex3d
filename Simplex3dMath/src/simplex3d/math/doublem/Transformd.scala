/*
 * Simplex3d, DoubleMath module
 * Copyright (C) 2009 Simplex3d Team
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
object Transformd {
    // 2-Dimensional
    def apply(translation: AnyVec2d, angle: Float, scale: AnyVec2d) :Mat2x3d = {
        val m = Mat2x3d(rotationMatFrom(angle))
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
    def apply(translation: AnyVec2d, angle: Float, scale: Float) :Mat2x3d = {
        val m = Mat2x3d(rotationMatFrom(angle))
        m(0) *= scale
        m(1) *= scale
        m(2) = translation
        m
    }
    def apply(translation: AnyVec2d, rotation: AnyMat2d, scale: Float)
    :Mat2x3d =
    {
        val m = Mat2x3d(rotation)
        m(0) *= scale
        m(1) *= scale
        m(2) = translation
        m
    }
    def apply(translation: AnyVec2d, angle: Float) :Mat2x3d = {
        val m = Mat2x3d(rotationMatFrom(angle))
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
    def apply(angle: Float, scale: AnyVec2d) :Mat2x3d = {
        val m = Mat2x3d(rotationMatFrom(angle))
        m(0) *= scale.x
        m(1) *= scale.y
        m
    }
    def apply(angle: Float, scale: Float) :Mat2x3d = {
        val m = Mat2x3d(rotationMatFrom(angle))
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
    def apply(rotation: AnyMat2d, scale: Float) :Mat2x3d = {
        val m = Mat2x3d(rotation)
        m(0) *= scale
        m(1) *= scale
        m
    }

    // 3-Dimensional
    def apply(translation: AnyVec3d, rotation: AnyQuat4d, scale: AnyVec3d)
    :Mat3x4d =
    {
        val m = Mat3x4d(rotationMatFrom(rotation))
        m(0) *= scale.x
        m(1) *= scale.y
        m(2) *= scale.z
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3d, angle: Float, axis: AnyVec3d, scale: AnyVec3d)
    :Mat3x4d =
    {
        val m = Mat3x4d(rotationMatFrom(angle, axis))
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
    def apply(translation: AnyVec3d, rotation: AnyQuat4d, scale: Float)
    :Mat3x4d =
    {
        val m = Mat3x4d(rotationMatFrom(rotation))
        m(0) *= scale
        m(1) *= scale
        m(2) *= scale
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3d, angle: Float, axis: AnyVec3d, scale: Float)
    :Mat3x4d =
    {
        val m = Mat3x4d(rotationMatFrom(angle, axis))
        m(0) *= scale
        m(1) *= scale
        m(2) *= scale
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3d, rotation: AnyMat3d, scale: Float) :Mat3x4d = {
        val m = Mat3x4d(rotation)
        m(0) *= scale
        m(1) *= scale
        m(2) *= scale
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3d, rotation: AnyQuat4d) :Mat3x4d = {
        val m = Mat3x4d(rotationMatFrom(rotation))
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3d, angle: Float, axis: AnyVec3d) :Mat3x4d = {
        val m = Mat3x4d(rotationMatFrom(angle, axis))
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
    def apply(translation: AnyVec3d, scale: Float) :Mat3x4d = {
        val m = Mat3x4d(scale)
        m(3) = translation
        m
    }
    def apply(rotation: AnyQuat4d, scale: AnyVec3d) :Mat3x4d = {
        val m = Mat3x4d(rotationMatFrom(rotation))
        m(0) *= scale.x
        m(1) *= scale.y
        m(2) *= scale.z
        m
    }
    def apply(angle: Float, axis: AnyVec3d, scale: AnyVec3d) :Mat3x4d = {
        val m = Mat3x4d(rotationMatFrom(angle, axis))
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
    def apply(rotation: AnyQuat4d, scale: Float) :Mat3x4d = {
        val m = Mat3x4d(rotationMatFrom(rotation))
        m(0) *= scale
        m(1) *= scale
        m(2) *= scale
        m
    }
    def apply(angle: Float, axis: AnyVec3d, scale: Float) :Mat3x4d = {
        val m = Mat3x4d(rotationMatFrom(angle, axis))
        m(0) *= scale
        m(1) *= scale
        m(2) *= scale
        m
    }
    def apply(rotation: AnyMat3d, scale: Float) :Mat3x4d = {
        val m = Mat3x4d(rotation)
        m(0) *= scale
        m(1) *= scale
        m(2) *= scale
        m
    }
}

object InverseTransformd {
    // 2-Dimensional
    def apply(translation: AnyVec2d, angle: Float, scale: AnyVec2d) :Mat2x3d = {
        val m = Mat2x3d(rotationMatFrom(angle))
        m(0) /= scale.x
        m(1) /= scale.y
        transposeSubMat2(m)
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
        transposeSubMat2(m)
        val t = m.transformPoint(-translation)
        m(2) = t
        m
    }
    def apply(translation: AnyVec2d, angle: Float, scale: Float) :Mat2x3d = {
        val m = Mat2x3d(rotationMatFrom(angle))
        m(0) /= scale
        m(1) /= scale
        transposeSubMat2(m)
        val t = m.transformPoint(-translation)
        m(2) = t
        m
    }
    def apply(translation: AnyVec2d, rotation: AnyMat2d, scale: Float)
    :Mat2x3d =
    {
        val m = Mat2x3d(rotation)
        m(0) /= scale
        m(1) /= scale
        transposeSubMat2(m)
        val t = m.transformPoint(-translation)
        m(2) = t
        m
    }
    def apply(translation: AnyVec2d, angle: Float) :Mat2x3d = {
        val m = Mat2x3d(rotationMatFrom(angle))
        transposeSubMat2(m)
        val t = m.transformPoint(-translation)
        m(2) = t
        m
    }
    def apply(translation: AnyVec2d, rotation: AnyMat2d) :Mat2x3d = {
        val m = Mat2x3d(rotation)
        transposeSubMat2(m)
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
    def apply(angle: Float, scale: AnyVec2d) :Mat2x3d = {
        val m = Mat2x3d(rotationMatFrom(angle))
        m(0) /= scale.x
        m(1) /= scale.y
        transposeSubMat2(m)
        m
    }
    def apply(angle: Float, scale: Float) :Mat2x3d = {
        val m = Mat2x3d(rotationMatFrom(angle))
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        transposeSubMat2(m)
        m
    }
    def apply(rotation: AnyMat2d, scale: AnyVec2d) :Mat2x3d = {
        val m = Mat2x3d(rotation)
        m(0) /= scale.x
        m(1) /= scale.y
        transposeSubMat2(m)
        m
    }
    def apply(rotation: AnyMat2d, scale: Float) :Mat2x3d = {
        val m = Mat2x3d(rotation)
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        transposeSubMat2(m)
        m
    }

    // 3-Dimensional
    def apply(translation: AnyVec3d, rotation: AnyQuat4d, scale: AnyVec3d)
    :Mat3x4d =
    {
        val m = Mat3x4d(rotationMatFrom(rotation))
        m(0) /= scale.x
        m(1) /= scale.y
        m(2) /= scale.z
        transposeSubMat3(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3d, angle: Float, axis: AnyVec3d, scale: AnyVec3d)
    :Mat3x4d =
    {
        val m = Mat3x4d(rotationMatFrom(angle, axis))
        m(0) /= scale.x
        m(1) /= scale.y
        m(2) /= scale.z
        transposeSubMat3(m)
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
        transposeSubMat3(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3d, rotation: AnyQuat4d, scale: Float)
    :Mat3x4d =
    {
        val m = Mat3x4d(rotationMatFrom(rotation))
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        m(2) *= invs
        transposeSubMat3(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3d, angle: Float, axis: AnyVec3d, scale: Float)
    :Mat3x4d =
    {
        val m = Mat3x4d(rotationMatFrom(angle, axis))
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        m(2) *= invs
        transposeSubMat3(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3d, rotation: AnyMat3d, scale: Float) :Mat3x4d = {
        val m = Mat3x4d(rotation)
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        m(2) *= invs
        transposeSubMat3(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3d, rotation: AnyQuat4d) :Mat3x4d = {
        val m = Mat3x4d(rotationMatFrom(rotation))
        transposeSubMat3(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3d, angle: Float, axis: AnyVec3d) :Mat3x4d = {
        val m = Mat3x4d(rotationMatFrom(angle, axis))
        transposeSubMat3(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3d, rotation: AnyMat3d) :Mat3x4d = {
        val m = Mat3x4d(rotation)
        transposeSubMat3(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3d, scale: AnyVec3d) :Mat3x4d = {
        val m = Mat3x4d(1/scale.x)
        m.m11 = 1/scale.y
        m.m22 = 1/scale.z
        transposeSubMat3(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3d, scale: Float) :Mat3x4d = {
        val invs = 1/scale
        val m = Mat3x4d(invs)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(rotation: AnyQuat4d, scale: AnyVec3d) :Mat3x4d = {
        val m = Mat3x4d(rotationMatFrom(rotation))
        m(0) /= scale.x
        m(1) /= scale.y
        m(2) /= scale.z
        transposeSubMat3(m)
        m
    }
    def apply(angle: Float, axis: AnyVec3d, scale: AnyVec3d) :Mat3x4d = {
        val m = Mat3x4d(rotationMatFrom(angle, axis))
        m(0) /= scale.x
        m(1) /= scale.y
        m(2) /= scale.z
        transposeSubMat3(m)
        m
    }
    def apply(rotation: AnyMat3d, scale: AnyVec3d) :Mat3x4d = {
        val m = Mat3x4d(rotation)
        m(0) /= scale.x
        m(1) /= scale.y
        m(2) /= scale.z
        transposeSubMat3(m)
        m
    }
    def apply(rotation: AnyQuat4d, scale: Float) :Mat3x4d = {
        val m = Mat3x4d(rotationMatFrom(rotation))
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        m(2) *= invs
        transposeSubMat3(m)
        m
    }
    def apply(angle: Float, axis: AnyVec3d, scale: Float) :Mat3x4d = {
        val m = Mat3x4d(rotationMatFrom(angle, axis))
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        m(2) *= invs
        transposeSubMat3(m)
        m
    }
    def apply(rotation: AnyMat3d, scale: Float) :Mat3x4d = {
        val m = Mat3x4d(rotation)
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        m(2) *= invs
        transposeSubMat3(m)
        m
    }

    private def transposeSubMat2(m: Mat2x3d) {
        import m._

        val t10 = m10
        m10 = m01
        m01 = t10
    }

    private def transposeSubMat3(m: Mat3x4d) {
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

object Translationd {
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

object Rotationd {
    def apply(angle: Float) :Mat2x3d = {
        val m = Mat2x3d(rotationMatFrom(angle))
        m
    }
    def apply(q: AnyQuat4d) :Mat3x4d = {
        val m = Mat3x4d(rotationMatFrom(q))
        m
    }
    def apply(angle: Float, axis: AnyVec3d) :Mat3x4d = {
        val m = Mat3x4d(rotationMatFrom(angle, axis))
        m
    }
}

object Scaled {
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
