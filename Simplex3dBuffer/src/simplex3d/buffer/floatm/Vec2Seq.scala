/*
 * Simplex3d, FloatBuffer module
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

package simplex3d.buffer.floatm

import java.nio._
import simplex3d.math._
import simplex3d.math.floatm._
import simplex3d.buffer._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[buffer] sealed abstract class BaseVec2f[+D <: ReadFloat](
  seq: ContiguousSeq[Float1, D]
) extends GenericSeq[Vec2f, D](seq) {
  final def components: Int = 2

  protected final def translatePut(
    destOffset: Int,
    src: ContiguousSeq[Float1, _],
    srcOffset: Int,
    srcStep: Int,
    srcLim: Int
  ) {
    val dest = backingSeq

    var desti = destOffset
    var srci = srcOffset

    while (srci < srcLim)  {
      dest(desti) = src(srci)
      dest(desti + 1) = src(srci + 1)
      desti += step
      srci += srcStep
    }
  }

  final def apply(i: Int) :AnyVec2f = {
    val j = offset + i*step
    ConstVec2f(
      seq(j),
      seq(j + 1)
    )
  }
  final def update(i: Int, v: AnyVec2f) {
    val j = offset + i*step
    seq(j) = v.x
    seq(j + 1) = v.y
  }

  override def makeArray(size: Int) =
    new ArrayVec2f[D](backingSeq.makeArray(size*2))
  override def makeBuffer(size: Int) =
    new BufferVec2f[D](backingSeq.makeBuffer(size*2))
//  override def makeView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
//    new ViewVec2f[D](backingSeq.makeBuffer(byteBuffer), offset, stride))
}

private[buffer] final class ArrayVec2f[+D <: ReadFloat](
  override val backingSeq: DataArray[Float1, D]
) extends BaseVec2f[D](backingSeq) with DataArray[Vec2f, D]

private[buffer] final class BufferVec2f[+D <: ReadFloat](
  override val backingSeq: DataBuffer[Float1, D]
) extends BaseVec2f[D](backingSeq) with DataBuffer[Vec2f, D]

private[buffer] final class ViewVec2f[+D <: ReadFloat](
  override val backingSeq: DataBuffer[Float1, D],
  val offset: Int,
  val stride: Int
) extends BaseVec2f[D](backingSeq) with DataView[Vec2f, D]
