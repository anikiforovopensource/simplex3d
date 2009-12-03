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
sealed trait ReadAny[P]

private[math] trait Read1[P] extends ReadAny[P] {
    def value: P
}

private[math] trait Read2[P] extends ReadAny[P] {
    def x: P
    def y: P
}

private[math] trait Read3[P] extends ReadAny[P] {
    def x: P
    def y: P
    def z: P
}

private[math] trait Read4[P] extends ReadAny[P] {
    def x: P
    def y: P
    def z: P
    def w: P
}
