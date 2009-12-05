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

package simplex3d.math.floatm


/**
 * @author Aleksey Nikiforov (lex)
 */
abstract class ConstRotationSubMat3f {
    // Column major order.
    def m00: Float; def m10: Float; def m20: Float // column
    def m01: Float; def m11: Float; def m21: Float // column
    def m02: Float; def m12: Float; def m22: Float // column
}

trait RotationSubMat3f {
    // Column major order.
    var m00: Float; var m10: Float; var m20: Float // column
    var m01: Float; var m11: Float; var m21: Float // column
    var m02: Float; var m12: Float; var m22: Float // column

    def set(
        m00: Float, m10: Float, m20: Float,
        m01: Float, m11: Float, m21: Float,
        m02: Float, m12: Float, m22: Float
    ) {
        this.m00 = m00; this.m10 = m10; this.m20 = m20
        this.m01 = m01; this.m11 = m11; this.m21 = m21
        this.m02 = m02; this.m12 = m12; this.m22 = m22
    }
}
