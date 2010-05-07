/*
 * Simplex3d, IntBuffer module
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

package simplex3d.buffer.intm

import java.nio._
import simplex3d.math._
import simplex3d.math.intm._
import simplex3d.buffer._


private[buffer] sealed abstract class BaseVec3i[+D <: ReadInt](
  seq: ContiguousSeq[Int1, D]
) extends GenericSeq[Vec3i, D](seq) {
  final def components: Int = 3

  protected final def translatePut(
    destOffset: Int,
    src: ContiguousSeq[Int1, _],
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

  final def apply(i: Int) :AnyVec3i = {
    val j = offset + i*step
    ConstVec3i(
      seq(j),
      seq(j + 1),
      seq(j + 2)
    )
  }
  final def update(i: Int, v: AnyVec3i) {
    val j = offset + i*step
    seq(j) = v.x
    seq(j + 1) = v.y
    seq(j + 2) = v.z
  }
}

private[buffer] final class ArrayVec3i[+D <: ReadInt](
  override val backingSeq: DataArray[Int1, D]
) extends BaseVec3i[D](backingSeq) with DataArray[Vec3i, D]

private[buffer] final class BufferVec3i[+D <: ReadInt](
  override val backingSeq: DataBuffer[Int1, D]
) extends BaseVec3i[D](backingSeq) with DataBuffer[Vec3i, D]

private[buffer] final class ViewVec3i[+D <: ReadInt](
  override val backingSeq: DataBuffer[Int1, D],
  val offset: Int,
  val stride: Int
) extends BaseVec3i[D](backingSeq) with DataView[Vec3i, D]
