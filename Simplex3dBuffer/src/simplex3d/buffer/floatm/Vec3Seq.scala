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
import scala.annotation.unchecked._
import simplex3d.math._
import simplex3d.math.floatm._
import simplex3d.buffer._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[buffer] sealed abstract class BaseVec3f[+D <: ReadFloat](
  seq: ContiguousSeq[Float1, D]
) extends GenericSeq[Vec3f, D](seq) {
  final def components: Int = 3

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
      dest(desti + 2) = src(srci + 2)
      desti += step
      srci += srcStep
    }
  }

  final def apply(i: Int) :AnyVec3f = {
    val j = offset + i*step
    ConstVec3f(
      seq(j),
      seq(j + 1),
      seq(j + 2)
    )
  }
  final def update(i: Int, v: AnyVec3f) {
    val j = offset + i*step
    seq(j) = v.x
    seq(j + 1) = v.y
    seq(j + 2) = v.z
  }

  final def makeArray(size: Int) =
    new ArrayVec3f[D](backingSeq.makeArray(size*3))
  final def makeArray(array: D#ArrayType @uncheckedVariance) =
    new ArrayVec3f[D](backingSeq.makeArray(array))
  final def makeBuffer(size: Int) =
    new BufferVec3f[D](backingSeq.makeBuffer(size*3))
  final def makeBuffer(byteBuffer: ByteBuffer) =
    new BufferVec3f[D](backingSeq.makeBuffer(byteBuffer))
  final def makeView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec3f[D](backingSeq.makeBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ArrayVec3f[+D <: ReadFloat](
  override val backingSeq: DataArray[Float1, D]
) extends BaseVec3f[D](backingSeq) with DataArray[Vec3f, D]

private[buffer] final class BufferVec3f[+D <: ReadFloat](
  override val backingSeq: DataBuffer[Float1, D]
) extends BaseVec3f[D](backingSeq) with DataBuffer[Vec3f, D]

private[buffer] final class ViewVec3f[+D <: ReadFloat](
  override val backingSeq: DataBuffer[Float1, D],
  val offset: Int,
  val stride: Int
) extends BaseVec3f[D](backingSeq) with DataView[Vec3f, D]
