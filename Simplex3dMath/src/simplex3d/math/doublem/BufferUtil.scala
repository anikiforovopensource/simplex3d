/*
 * Simplex3D, DoubleMath module
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

import java.nio._


/**
 * @author Aleksey Nikiforov (lex)
 */
object BufferUtil {

    /**
     * Column major order
     */
    def toArray(m: AnyMat2d, a: Array[Double]) {
        toArray(m, a, true, 0)
    }
    /**
     * Column major order
     */
    def toBuffer(m: AnyMat2d, buf: DoubleBuffer) {
        toBuffer(m, buf, true, 0)
    }
    /**
     * Column major order
     */
    def toArray(m: AnyMat2d, a: Array[Double], upcast: Boolean, offset: Int) {
        import m._

        if (upcast) {
            a(offset) = m00
            a(offset + 1) = m10
            a(offset + 2) = 0
            a(offset + 3) = 0
            a(offset + 4) = m01
            a(offset + 5) = m11
            a(offset + 6) = 0
            a(offset + 7) = 0
            a(offset + 8) = 0
            a(offset + 9) = 0
            a(offset + 10) = 0
            a(offset + 11) = 0
            a(offset + 12) = 0
            a(offset + 13) = 0
            a(offset + 14) = 0
            a(offset + 15) = 1
        } else {
            m.toArray(a, offset)
        }
    }
    /**
     * Column major order
     */
    def toBuffer(m: AnyMat2d, buf: DoubleBuffer, upcast: Boolean, offset: Int) {
        import m._

        buf.position(offset)

        if (upcast) {
            buf.put(m00)
            buf.put(m10)
            buf.put(0)
            buf.put(0)
            buf.put(m01)
            buf.put(m11)
            buf.put(0)
            buf.put(0)
            buf.put(0)
            buf.put(0)
            buf.put(0)
            buf.put(0)
            buf.put(0)
            buf.put(0)
            buf.put(0)
            buf.put(1)
        } else {
            buf.put(m00)
            buf.put(m10)
            buf.put(m01)
            buf.put(m11)
        }
    }

    /**
     * Column major order
     */
    def toArray(m: AnyMat2x3d, a: Array[Double]) {
        toArray(m, a, true, 0)
    }
    /**
     * Column major order
     */
    def toBuffer(m: AnyMat2x3d, buf: DoubleBuffer) {
        toBuffer(m, buf, true, 0)
    }
    /**
     * Column major order
     */
    def toArray(m: AnyMat2x3d, a: Array[Double], upcast: Boolean, offset: Int) {
        import m._

        if (upcast) {
            a(offset) = m00
            a(offset + 1) = m10
            a(offset + 2) = 0
            a(offset + 3) = 0
            a(offset + 4) = m01
            a(offset + 5) = m11
            a(offset + 6) = 0
            a(offset + 7) = 0
            a(offset + 8) = m02
            a(offset + 9) = m12
            a(offset + 10) = 0
            a(offset + 11) = 0
            a(offset + 12) = 0
            a(offset + 13) = 0
            a(offset + 14) = 0
            a(offset + 15) = 1
        } else {
            m.toArray(a, offset)
        }
    }
    /**
     * Column major order
     */
    def toBuffer(m: AnyMat2x3d, buf: DoubleBuffer, upcast: Boolean, offset: Int) {
        import m._

        buf.position(offset)

        if (upcast) {
            buf.put(m00)
            buf.put(m10)
            buf.put(0)
            buf.put(0)
            buf.put(m01)
            buf.put(m11)
            buf.put(0)
            buf.put(0)
            buf.put(m02)
            buf.put(m12)
            buf.put(0)
            buf.put(0)
            buf.put(0)
            buf.put(0)
            buf.put(0)
            buf.put(1)
        } else {
            buf.put(m00)
            buf.put(m10)
            buf.put(m01)
            buf.put(m11)
            buf.put(m02)
            buf.put(m12)
        }
    }

    /**
     * Column major order
     */
    def toArray(m: AnyMat3d, a: Array[Double]) {
        toArray(m, a, true, 0)
    }
    /**
     * Column major order
     */
    def toBuffer(m: AnyMat3d, buf: DoubleBuffer) {
        toBuffer(m, buf, true, 0)
    }
    /**
     * Column major order
     */
    def toArray(m: AnyMat3d, a: Array[Double], upcast: Boolean, offset: Int) {
        import m._

        if (upcast) {
            a(offset) = m00
            a(offset + 1) = m10
            a(offset + 2) = m20
            a(offset + 3) = 0
            a(offset + 4) = m01
            a(offset + 5) = m11
            a(offset + 6) = m21
            a(offset + 7) = 0
            a(offset + 8) = m02
            a(offset + 9) = m12
            a(offset + 10) = m22
            a(offset + 11) = 0
            a(offset + 12) = 0
            a(offset + 13) = 0
            a(offset + 14) = 0
            a(offset + 15) = 1
        } else {
            m.toArray(a, offset)
        }
    }
    /**
     * Column major order
     */
    def toBuffer(m: AnyMat3d, buf: DoubleBuffer, upcast: Boolean, offset: Int) {
        import m._

        buf.position(offset)

        if (upcast) {
            buf.put(m00)
            buf.put(m10)
            buf.put(m20)
            buf.put(0)
            buf.put(m01)
            buf.put(m11)
            buf.put(m21)
            buf.put(0)
            buf.put(m02)
            buf.put(m12)
            buf.put(m22)
            buf.put(0)
            buf.put(0)
            buf.put(0)
            buf.put(0)
            buf.put(1)
        } else {
            buf.put(m00)
            buf.put(m10)
            buf.put(m20)
            buf.put(m01)
            buf.put(m11)
            buf.put(m21)
            buf.put(m02)
            buf.put(m12)
            buf.put(m22)
        }
    }

    /**
     * Column major order
     */
    def toArray(m: AnyMat3x4d, a: Array[Double]) {
        toArray(m, a, true, 0)
    }
    /**
     * Column major order
     */
    def toBuffer(m: AnyMat3x4d, buf: DoubleBuffer) {
        toBuffer(m, buf, true, 0)
    }
    /**
     * Column major order
     */
    def toArray(m: AnyMat3x4d, a: Array[Double], upcast: Boolean, offset: Int) {
        import m._

        if (upcast) {
            a(offset) = m00
            a(offset + 1) = m10
            a(offset + 2) = m20
            a(offset + 3) = 0
            a(offset + 4) = m01
            a(offset + 5) = m11
            a(offset + 6) = m21
            a(offset + 7) = 0
            a(offset + 8) = m02
            a(offset + 9) = m12
            a(offset + 10) = m22
            a(offset + 11) = 0
            a(offset + 12) = m03
            a(offset + 13) = m13
            a(offset + 14) = m23
            a(offset + 15) = 1
        } else {
            m.toArray(a, offset)
        }
    }
    /**
     * Column major order
     */
    def toBuffer(m: AnyMat3x4d, buf: DoubleBuffer, upcast: Boolean, offset: Int) {
        import m._

        buf.position(offset)

        if (upcast) {
            buf.put(m00)
            buf.put(m10)
            buf.put(m20)
            buf.put(0)
            buf.put(m01)
            buf.put(m11)
            buf.put(m21)
            buf.put(0)
            buf.put(m02)
            buf.put(m12)
            buf.put(m22)
            buf.put(0)
            buf.put(m03)
            buf.put(m13)
            buf.put(m23)
            buf.put(1)
        } else {
            buf.put(m00)
            buf.put(m10)
            buf.put(m20)
            buf.put(m01)
            buf.put(m11)
            buf.put(m21)
            buf.put(m02)
            buf.put(m12)
            buf.put(m22)
            buf.put(m03)
            buf.put(m13)
            buf.put(m23)
        }
    }

    /**
     * Column major order
     */
    def toArray(m: AnyMat4d, a: Array[Double]) {
        toArray(m, a, 0)
    }
    /**
     * Column major order
     */
    def toBuffer(m: AnyMat4d, buf: DoubleBuffer) {
        toBuffer(m, buf, 0)
    }
    /**
     * Column major order
     */
    def toArray(m: AnyMat4d, a: Array[Double], offset: Int) {
        m.toArray(a, offset)
    }
    /**
     * Column major order
     */
    def toBuffer(m: AnyMat4d, buf: DoubleBuffer, offset: Int) {
        import m._

        buf.position(offset)

        buf.put(m00)
        buf.put(m10)
        buf.put(m20)
        buf.put(m30)
        buf.put(m01)
        buf.put(m11)
        buf.put(m21)
        buf.put(m31)
        buf.put(m02)
        buf.put(m12)
        buf.put(m22)
        buf.put(m32)
        buf.put(m03)
        buf.put(m13)
        buf.put(m23)
        buf.put(m33)
    }
}
