/*
 * Simplex3D, Math package
 * Copyright (C) 2009 Simplex3D team
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *
 * CLASSPATH EXCEPTION FOR UNMODIFIED WORK:
 * Linking this library statically or dynamically with other modules is making
 * a combined work based on this library. Thus, the terms and conditions of
 * the GNU General Public License cover the whole combination.
 *
 * As a special exception, the copyright holders of this library give you
 * permission to link this library with independent modules to produce
 * an executable, regardless of the license terms of these independent modules,
 * and to copy and distribute the resulting executable under terms of your
 * choice, provided that you also meet, for each linked independent module,
 * the terms and conditions of the license of that module. An independent module
 * is a module which is not derived from or based on this library. If you modify
 * this library in any way, then this exception is null and void and no longer
 * applies, in this case delete this exception statement from your version.
 */

package simplex3d.math

import ExtendedMath._


/**
 * Prototype: all the methods here need to be optimized.
 *
 * @author Aleksey Nikiforov (lex)
 */
object Transform {
    // 2-Dimensional
    def apply(translation: AnyVec2, angle: Float, scale: AnyVec2) :Mat2x3 = {
        val m = Mat2x3(1)
        rotationMatFrom(angle, m)
        m(0) *= scale.x
        m(1) *= scale.y
        m(2) = translation
        m
    }
    def apply(translation: AnyVec2, rotation: AnyMat2, scale: AnyVec2)
    :Mat2x3 =
    {
        val m = Mat2x3(rotation)
        m(0) *= scale.x
        m(1) *= scale.y
        m(2) = translation
        m
    }
    def apply(translation: AnyVec2, angle: Float, scale: Float) :Mat2x3 = {
        val m = Mat2x3(1)
        rotationMatFrom(angle, m)
        m(0) *= scale
        m(1) *= scale
        m(2) = translation
        m
    }
    def apply(translation: AnyVec2, rotation: AnyMat2, scale: Float)
    :Mat2x3 =
    {
        val m = Mat2x3(rotation)
        m(0) *= scale
        m(1) *= scale
        m(2) = translation
        m
    }
    def apply(translation: AnyVec2, angle: Float) :Mat2x3 = {
        val m = Mat2x3(1)
        rotationMatFrom(angle, m)
        m(2) = translation
        m
    }
    def apply(translation: AnyVec2, rotation: AnyMat2) :Mat2x3 = {
        val m = Mat2x3(rotation)
        m(2) = translation
        m
    }
    def apply(translation: AnyVec2, scale: AnyVec2) :Mat2x3 = {
        val m = Mat2x3(scale.x)
        m.m11 = scale.y
        m(2) = translation
        m
    }
    def apply(angle: Float, scale: AnyVec2) :Mat2x3 = {
        val m = Mat2x3(1)
        rotationMatFrom(angle, m)
        m(0) *= scale.x
        m(1) *= scale.y
        m
    }
    def apply(angle: Float, scale: Float) :Mat2x3 = {
        val m = Mat2x3(1)
        rotationMatFrom(angle, m)
        m(0) *= scale
        m(1) *= scale
        m
    }
    def apply(rotation: AnyMat2, scale: AnyVec2) :Mat2x3 = {
        val m = Mat2x3(rotation)
        m(0) *= scale.x
        m(1) *= scale.y
        m
    }
    def apply(rotation: AnyMat2, scale: Float) :Mat2x3 = {
        val m = Mat2x3(rotation)
        m(0) *= scale
        m(1) *= scale
        m
    }

    // 3-Dimensional
    def apply(translation: AnyVec3, rotation: AnyQuat4, scale: AnyVec3)
    :Mat3x4 =
    {
        val m = Mat3x4(1)
        rotationMatFrom(rotation, m)
        m(0) *= scale.x
        m(1) *= scale.y
        m(2) *= scale.z
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3, angle: Float, axis: AnyVec3, scale: AnyVec3)
    :Mat3x4 =
    {
        val m = Mat3x4(1)
        rotationMatFrom(angle, axis, m)
        m(0) *= scale.x
        m(1) *= scale.y
        m(2) *= scale.z
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3, rotation: AnyMat3, scale: AnyVec3)
    :Mat3x4 =
    {
        val m = Mat3x4(rotation)
        m(0) *= scale.x
        m(1) *= scale.y
        m(2) *= scale.z
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3, rotation: AnyQuat4, scale: Float)
    :Mat3x4 =
    {
        val m = Mat3x4(1)
        rotationMatFrom(rotation, m)
        m(0) *= scale
        m(1) *= scale
        m(2) *= scale
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3, angle: Float, axis: AnyVec3, scale: Float)
    :Mat3x4 =
    {
        val m = Mat3x4(1)
        rotationMatFrom(angle, axis, m)
        m(0) *= scale
        m(1) *= scale
        m(2) *= scale
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3, rotation: AnyMat3, scale: Float) :Mat3x4 = {
        val m = Mat3x4(rotation)
        m(0) *= scale
        m(1) *= scale
        m(2) *= scale
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3, rotation: AnyQuat4) :Mat3x4 = {
        val m = Mat3x4(1)
        rotationMatFrom(rotation, m)
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3, angle: Float, axis: AnyVec3) :Mat3x4 = {
        val m = Mat3x4(1)
        rotationMatFrom(angle, axis, m)
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3, rotation: AnyMat3) :Mat3x4 = {
        val m = Mat3x4(rotation)
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3, scale: AnyVec3) :Mat3x4 = {
        val m = Mat3x4(scale.x)
        m.m11 = scale.y
        m.m22 = scale.z
        m(3) = translation
        m
    }
    def apply(translation: AnyVec3, scale: Float) :Mat3x4 = {
        val m = Mat3x4(scale)
        m(3) = translation
        m
    }
    def apply(rotation: AnyQuat4, scale: AnyVec3) :Mat3x4 = {
        val m = Mat3x4(1)
        rotationMatFrom(rotation, m)
        m(0) *= scale.x
        m(1) *= scale.y
        m(2) *= scale.z
        m
    }
    def apply(angle: Float, axis: AnyVec3, scale: AnyVec3) :Mat3x4 = {
        val m = Mat3x4(1)
        rotationMatFrom(angle, axis, m)
        m(0) *= scale.x
        m(1) *= scale.y
        m(2) *= scale.z
        m
    }
    def apply(rotation: AnyMat3, scale: AnyVec3) :Mat3x4 = {
        val m = Mat3x4(rotation)
        m(0) *= scale.x
        m(1) *= scale.y
        m(2) *= scale.z
        m
    }
    def apply(rotation: AnyQuat4, scale: Float) :Mat3x4 = {
        val m = Mat3x4(1)
        rotationMatFrom(rotation, m)
        m(0) *= scale
        m(1) *= scale
        m(2) *= scale
        m
    }
    def apply(angle: Float, axis: AnyVec3, scale: Float) :Mat3x4 = {
        val m = Mat3x4(1)
        rotationMatFrom(angle, axis, m)
        m(0) *= scale
        m(1) *= scale
        m(2) *= scale
        m
    }
    def apply(rotation: AnyMat3, scale: Float) :Mat3x4 = {
        val m = Mat3x4(rotation)
        m(0) *= scale
        m(1) *= scale
        m(2) *= scale
        m
    }
}

object InverseTransform {
    // 2-Dimensional
    def apply(translation: AnyVec2, angle: Float, scale: AnyVec2) :Mat2x3 = {
        val m = Mat2x3(1)
        rotationMatFrom(angle, m)
        m(0) /= scale.x
        m(1) /= scale.y
        transposeSubMat2(m)
        val t = m.transformPoint(-translation)
        m(2) = t
        m
    }
    def apply(translation: AnyVec2, rotation: AnyMat2, scale: AnyVec2)
    :Mat2x3 =
    {
        val m = Mat2x3(rotation)
        m(0) /= scale.x
        m(1) /= scale.y
        transposeSubMat2(m)
        val t = m.transformPoint(-translation)
        m(2) = t
        m
    }
    def apply(translation: AnyVec2, angle: Float, scale: Float) :Mat2x3 = {
        val m = Mat2x3(1)
        rotationMatFrom(angle, m)
        m(0) /= scale
        m(1) /= scale
        transposeSubMat2(m)
        val t = m.transformPoint(-translation)
        m(2) = t
        m
    }
    def apply(translation: AnyVec2, rotation: AnyMat2, scale: Float)
    :Mat2x3 =
    {
        val m = Mat2x3(rotation)
        m(0) /= scale
        m(1) /= scale
        transposeSubMat2(m)
        val t = m.transformPoint(-translation)
        m(2) = t
        m
    }
    def apply(translation: AnyVec2, angle: Float) :Mat2x3 = {
        val m = Mat2x3(1)
        rotationMatFrom(angle, m)
        transposeSubMat2(m)
        val t = m.transformPoint(-translation)
        m(2) = t
        m
    }
    def apply(translation: AnyVec2, rotation: AnyMat2) :Mat2x3 = {
        val m = Mat2x3(rotation)
        transposeSubMat2(m)
        val t = m.transformPoint(-translation)
        m(2) = t
        m
    }
    def apply(translation: AnyVec2, scale: AnyVec2) :Mat2x3 = {
        val m = Mat2x3(1/scale.x)
        m.m11 = 1/scale.y
        val t = m.transformPoint(-translation)
        m(2) = t
        m
    }
    def apply(angle: Float, scale: AnyVec2) :Mat2x3 = {
        val m = Mat2x3(1)
        rotationMatFrom(angle, m)
        m(0) /= scale.x
        m(1) /= scale.y
        transposeSubMat2(m)
        m
    }
    def apply(angle: Float, scale: Float) :Mat2x3 = {
        val m = Mat2x3(1)
        rotationMatFrom(angle, m)
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        transposeSubMat2(m)
        m
    }
    def apply(rotation: AnyMat2, scale: AnyVec2) :Mat2x3 = {
        val m = Mat2x3(rotation)
        m(0) /= scale.x
        m(1) /= scale.y
        transposeSubMat2(m)
        m
    }
    def apply(rotation: AnyMat2, scale: Float) :Mat2x3 = {
        val m = Mat2x3(rotation)
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        transposeSubMat2(m)
        m
    }

    // 3-Dimensional
    def apply(translation: AnyVec3, rotation: AnyQuat4, scale: AnyVec3)
    :Mat3x4 =
    {
        val m = Mat3x4(1)
        rotationMatFrom(rotation, m)
        m(0) /= scale.x
        m(1) /= scale.y
        m(2) /= scale.z
        transposeSubMat3(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3, angle: Float, axis: AnyVec3, scale: AnyVec3)
    :Mat3x4 =
    {
        val m = Mat3x4(1)
        rotationMatFrom(angle, axis, m)
        m(0) /= scale.x
        m(1) /= scale.y
        m(2) /= scale.z
        transposeSubMat3(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3, rotation: AnyMat3, scale: AnyVec3)
    :Mat3x4 =
    {
        val m = Mat3x4(rotation)
        m(0) /= scale.x
        m(1) /= scale.y
        m(2) /= scale.z
        transposeSubMat3(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3, rotation: AnyQuat4, scale: Float)
    :Mat3x4 =
    {
        val m = Mat3x4(1)
        rotationMatFrom(rotation, m)
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        m(2) *= invs
        transposeSubMat3(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3, angle: Float, axis: AnyVec3, scale: Float)
    :Mat3x4 =
    {
        val m = Mat3x4(1)
        rotationMatFrom(angle, axis, m)
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        m(2) *= invs
        transposeSubMat3(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3, rotation: AnyMat3, scale: Float) :Mat3x4 = {
        val m = Mat3x4(rotation)
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        m(2) *= invs
        transposeSubMat3(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3, rotation: AnyQuat4) :Mat3x4 = {
        val m = Mat3x4(1)
        rotationMatFrom(rotation, m)
        transposeSubMat3(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3, angle: Float, axis: AnyVec3) :Mat3x4 = {
        val m = Mat3x4(1)
        rotationMatFrom(angle, axis, m)
        transposeSubMat3(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3, rotation: AnyMat3) :Mat3x4 = {
        val m = Mat3x4(rotation)
        transposeSubMat3(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3, scale: AnyVec3) :Mat3x4 = {
        val m = Mat3x4(1/scale.x)
        m.m11 = 1/scale.y
        m.m22 = 1/scale.z
        transposeSubMat3(m)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(translation: AnyVec3, scale: Float) :Mat3x4 = {
        val invs = 1/scale
        val m = Mat3x4(invs)
        val t = m.transformPoint(-translation)
        m(3) = t
        m
    }
    def apply(rotation: AnyQuat4, scale: AnyVec3) :Mat3x4 = {
        val m = Mat3x4(1)
        rotationMatFrom(rotation, m)
        m(0) /= scale.x
        m(1) /= scale.y
        m(2) /= scale.z
        transposeSubMat3(m)
        m
    }
    def apply(angle: Float, axis: AnyVec3, scale: AnyVec3) :Mat3x4 = {
        val m = Mat3x4(1)
        rotationMatFrom(angle, axis, m)
        m(0) /= scale.x
        m(1) /= scale.y
        m(2) /= scale.z
        transposeSubMat3(m)
        m
    }
    def apply(rotation: AnyMat3, scale: AnyVec3) :Mat3x4 = {
        val m = Mat3x4(rotation)
        m(0) /= scale.x
        m(1) /= scale.y
        m(2) /= scale.z
        transposeSubMat3(m)
        m
    }
    def apply(rotation: AnyQuat4, scale: Float) :Mat3x4 = {
        val m = Mat3x4(1)
        rotationMatFrom(rotation, m)
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        m(2) *= invs
        transposeSubMat3(m)
        m
    }
    def apply(angle: Float, axis: AnyVec3, scale: Float) :Mat3x4 = {
        val m = Mat3x4(1)
        rotationMatFrom(angle, axis, m)
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        m(2) *= invs
        transposeSubMat3(m)
        m
    }
    def apply(rotation: AnyMat3, scale: Float) :Mat3x4 = {
        val m = Mat3x4(rotation)
        val invs = 1/scale
        m(0) *= invs
        m(1) *= invs
        m(2) *= invs
        transposeSubMat3(m)
        m
    }
}

object Translation {
    def apply(t: AnyVec2) :Mat2x3 = {
        val m = Mat2x3(1)
        m(2) = t
        m
    }

    def apply(t: AnyVec3) :Mat3x4 = {
        val m = Mat3x4(1)
        m(3) = t
        m
    }
}

object Rotation {
    def apply(angle: Float) :Mat2x3 = {
        val m = Mat2x3(1)
        rotationMatFrom(angle, m)
        m
    }
    def apply(q: AnyQuat4) :Mat3x4 = {
        val m = Mat3x4(1)
        rotationMatFrom(q, m)
        m
    }
    def apply(angle: Float, axis: AnyVec3) :Mat3x4 = {
        val m = Mat3x4(1)
        rotationMatFrom(angle, axis, m)
        m
    }
}

object Scale {
    def apply(s: AnyVec2) :Mat2x3 = {
        val m = Mat2x3(s.x)
        m.m11 = s.y
        m
    }

    def apply(s: AnyVec3) :Mat3x4 = {
        val m = Mat3x4(s.x)
        m.m11 = s.y
        m.m22 = s.z
        m
    }
}
