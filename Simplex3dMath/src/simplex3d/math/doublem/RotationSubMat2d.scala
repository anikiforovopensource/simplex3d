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


/**
 * @author Aleksey Nikiforov (lex)
 */
trait ConstRotationSubMat2d {
    // Column major order.
    def m00: Double; def m10: Double // column
    def m01: Double; def m11: Double // column
}

trait RotationSubMat2d {
    // Column major order.
    var m00: Double; var m10: Double // column
    var m01: Double; var m11: Double // column

    def set(
        m00: Double, m10: Double,
        m01: Double, m11: Double
    ) {
        this.m00 = m00; this.m10 = m10
        this.m01 = m01; this.m11 = m11
    }
}
