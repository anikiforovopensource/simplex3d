/*
 * Simplex3d, BaseMath module
 * Copyright (C) 2010 Simplex3d Team
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

import java.nio._


/**
 * @author Aleksey Nikiforov (lex)
 */
object BufferSupport {
  def toArray(array: Array[Float], m: ReadMat[_]) { toArray(array, 0, m) }

  /** Column major order.
   *
   */
  def toArray(array: Array[Float], offset: Int, m: ReadMat[_]) {
    array(offset + 0) = m.f00
    array(offset + 1) = m.f10
    array(offset + 2) = m.f20
    array(offset + 3) = m.f30

    array(offset + 4) = m.f01
    array(offset + 5) = m.f11
    array(offset + 6) = m.f21
    array(offset + 7) = m.f31

    array(offset + 8) = m.f02
    array(offset + 9) = m.f12
    array(offset + 10)= m.f22
    array(offset + 11)= m.f32

    array(offset + 12)= m.f03
    array(offset + 13)= m.f13
    array(offset + 14)= m.f23
    array(offset + 15)= m.f33
  }

  /** Use buffer.position to change offset.
   *
   */
  def toBuffer(buffer: FloatBuffer, m: ReadMat[_]) {
    buffer.put(m.f00)
    buffer.put(m.f10)
    buffer.put(m.f20)
    buffer.put(m.f30)

    buffer.put(m.f01)
    buffer.put(m.f11)
    buffer.put(m.f21)
    buffer.put(m.f31)

    buffer.put(m.f02)
    buffer.put(m.f12)
    buffer.put(m.f22)
    buffer.put(m.f32)

    buffer.put(m.f03)
    buffer.put(m.f13)
    buffer.put(m.f23)
    buffer.put(m.f33)
  }

  def toArray(array: Array[Double], m: ReadMat[_]) { toArray(array, 0, m) }

  /** Column major order.
   *
   */
  def toArray(array: Array[Double], offset: Int, m: ReadMat[_]) {
    array(offset + 0) = m.d00
    array(offset + 1) = m.d10
    array(offset + 2) = m.d20
    array(offset + 3) = m.d30

    array(offset + 4) = m.d01
    array(offset + 5) = m.d11
    array(offset + 6) = m.d21
    array(offset + 7) = m.d31

    array(offset + 8) = m.d02
    array(offset + 9) = m.d12
    array(offset + 10)= m.d22
    array(offset + 11)= m.d32

    array(offset + 12)= m.d03
    array(offset + 13)= m.d13
    array(offset + 14)= m.d23
    array(offset + 15)= m.d33
  }

  def toBuffer(buffer: DoubleBuffer, m: ReadMat[_]) {
    buffer.put(m.d00)
    buffer.put(m.d10)
    buffer.put(m.d20)
    buffer.put(m.d30)

    buffer.put(m.d01)
    buffer.put(m.d11)
    buffer.put(m.d21)
    buffer.put(m.d31)

    buffer.put(m.d02)
    buffer.put(m.d12)
    buffer.put(m.d22)
    buffer.put(m.d32)

    buffer.put(m.d03)
    buffer.put(m.d13)
    buffer.put(m.d23)
    buffer.put(m.d33)
  }
}
