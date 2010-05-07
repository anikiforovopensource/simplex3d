/*
 * Simplex3d, BaseBuffer module
 * Copyright (C) 2010 Simplex3d Team
 *
 * This file is part of Simplex3dBuffer.
 *
 * Simplex3dBuffer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dBuffer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.buffer

import java.nio._

private[buffer] class BufferUtil
object BufferUtil {

  def allocateByteBuffer(size: Int) = {
    val direct = ByteBuffer.allocateDirect(size)
    direct.order(ByteOrder.nativeOrder())
  }
  def allocateShortBuffer(size: Int) = {
    val direct = ByteBuffer.allocateDirect(size*2)
    direct.order(ByteOrder.nativeOrder()).asShortBuffer()
  }
  def allocateCharBuffer(size: Int) = {
    val direct = ByteBuffer.allocateDirect(size*2)
    direct.order(ByteOrder.nativeOrder()).asCharBuffer()
  }
  def allocateIntBuffer(size: Int) = {
    val direct = ByteBuffer.allocateDirect(size*4)
    direct.order(ByteOrder.nativeOrder()).asIntBuffer()
  }
  def allocateFloatBuffer(size: Int) = {
    val direct = ByteBuffer.allocateDirect(size*4)
    direct.order(ByteOrder.nativeOrder()).asFloatBuffer()
  }
  def allocateDoubleBuffer(size: Int) = {
    val direct = ByteBuffer.allocateDirect(size*8)
    direct.order(ByteOrder.nativeOrder()).asDoubleBuffer()
  }
}
