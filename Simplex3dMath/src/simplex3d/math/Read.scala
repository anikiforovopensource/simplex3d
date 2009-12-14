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

package simplex3d.math


/**
 * @author Aleksey Nikiforov (lex)
 */
private[math] abstract class Read2Int {
    def x: Int
    def y: Int
}

private[math] abstract class Read3Int {
    def x: Int
    def y: Int
    def z: Int
}

private[math] abstract class Read4Int {
    def x: Int
    def y: Int
    def z: Int
    def w: Int
}

private[math] abstract class Read2Float {
    def x: Float
    def y: Float
}

private[math] abstract class Read3Float {
    def x: Float
    def y: Float
    def z: Float
}

private[math] abstract class Read4Float {
    def x: Float
    def y: Float
    def z: Float
    def w: Float
}

private[math] abstract class Read2Double {
    def x: Double
    def y: Double
}

private[math] abstract class Read3Double {
    def x: Double
    def y: Double
    def z: Double
}

private[math] abstract class Read4Double {
    def x: Double
    def y: Double
    def z: Double
    def w: Double
}
