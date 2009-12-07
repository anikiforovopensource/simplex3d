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
sealed trait ReadAny[+P]

private[math] trait Read1[+P] extends ReadAny[P] {
    def value: P
}

private[math] trait Read2[+P] extends ReadAny[P] {
    def x: P
    def y: P
}

private[math] trait Read3[+P] extends ReadAny[P] {
    def x: P
    def y: P
    def z: P
}

private[math] trait Read4[+P] extends ReadAny[P] {
    def x: P
    def y: P
    def z: P
    def w: P
}

private[math] trait Read1Int extends Read1[Int] {
    def value: Int
}

private[math] trait Read2Int extends Read2[Int] {
    override def x: Int
    override def y: Int
}

private[math] trait Read3Int extends Read3[Int] {
    override def x: Int
    override def y: Int
    override def z: Int
}

private[math] trait Read4Int extends Read4[Int] {
    override def x: Int
    override def y: Int
    override def z: Int
    override def w: Int
}

private[math] trait Read1Float extends Read1[Float] {
    def value: Float
}

private[math] trait Read2Float extends Read2[Float] {
    override def x: Float
    override def y: Float
}

private[math] trait Read3Float extends Read3[Float] {
    override def x: Float
    override def y: Float
    override def z: Float
}

private[math] trait Read4Float extends Read4[Float] {
    override def x: Float
    override def y: Float
    override def z: Float
    override def w: Float
}

private[math] trait Read1Double extends Read1[Double] {
    def value: Double
}

private[math] trait Read2Double extends Read2[Double] {
    override def x: Double
    override def y: Double
}

private[math] trait Read3Double extends Read3[Double] {
    override def x: Double
    override def y: Double
    override def z: Double
}

private[math] trait Read4Double extends Read4[Double] {
    override def x: Double
    override def y: Double
    override def z: Double
    override def w: Double
}

private[math] class IntVal(val value: Int) extends Read1Int
private[math] class FloatVal(val value: Float) extends Read1Float
private[math] class DoubleVal(val value: Double) extends Read1Double

private[math] trait ReadFloatMat {
    def rows: Int
    def columns: Int
    def toArray(array: Array[Float], offset: Int) :Unit
}

private[math] trait ReadDoubleMat {
    def rows: Int
    def columns: Int
    def toArray(array: Array[Double], offset: Int) :Unit
}
