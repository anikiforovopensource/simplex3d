/*
 * Simplex3d, BaseMath module
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

package simplex3d.math


/**
 * @author Aleksey Nikiforov (lex)
 */
private[math] abstract class Read2Int extends Swizzle2Read[Int] {
    def x: Int
    def y: Int
}

private[math] abstract class Read3Int extends Swizzle3Read[Int] {
    def x: Int
    def y: Int
    def z: Int
}

private[math] abstract class Read4Int extends Swizzle4Read[Int] {
    def x: Int
    def y: Int
    def z: Int
    def w: Int
}

private[math] abstract class Read2Float extends Swizzle2Read[Float] {
    def x: Float
    def y: Float
}

private[math] abstract class Read3Float extends Swizzle3Read[Float] {
    def x: Float
    def y: Float
    def z: Float
}

private[math] abstract class Read4Float extends Swizzle4Read[Float] {
    def x: Float
    def y: Float
    def z: Float
    def w: Float
}

private[math] abstract class Read2Double extends Swizzle2Read[Double] {
    def x: Double
    def y: Double
}

private[math] abstract class Read3Double extends Swizzle3Read[Double] {
    def x: Double
    def y: Double
    def z: Double
}

private[math] abstract class Read4Double extends Swizzle4Read[Double] {
    def x: Double
    def y: Double
    def z: Double
    def w: Double
}
