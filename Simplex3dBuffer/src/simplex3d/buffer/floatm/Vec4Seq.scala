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
private[buffer] sealed abstract class BaseVec4f[+D <: ReadFloat](
  seq: ContiguousSeq[Float1, D]
) extends GenericSeq[Vec4f, D](seq) {
  final def components: Int = 4

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
      dest(desti + 3) = src(srci + 3)
      desti += step
      srci += srcStep
    }
  }

  def apply(i: Int) :AnyVec4f = {
    val j = offset + i*step
    ConstVec4f(
      seq(j),
      seq(j + 1),
      seq(j + 2),
      seq(j + 3)
    )
  }
  def update(i: Int, v: AnyVec4f) {
    val j = offset + i*step
    seq(j) = v.x
    seq(j + 1) = v.y
    seq(j + 2) = v.z
    seq(j + 3) = v.w
  }

  final def mkDataArray(size: Int) = {
    backingSeq match {
      case b: ArrayFloat1NUByte =>
        new ArrayVec4fNUByte(
          b.mkDataArray(size*4)
        ).asInstanceOf[DataArray[Vec4f, D]]
      case _ =>
        new ArrayVec4f[D](backingSeq.mkDataArray(size*4))
    }
  }
  final def mkDataArray(array: D#ArrayType @uncheckedVariance) = {
    backingSeq match {
      case b: ArrayFloat1NUByte =>
        new ArrayVec4fNUByte(
          b.mkDataArray(array.asInstanceOf[Array[Byte]])
        ).asInstanceOf[DataArray[Vec4f, D]]
      case _ =>
        new ArrayVec4f[D](backingSeq.mkDataArray(array))
    }
  }
  final def mkDataBuffer(size: Int) = {
    backingSeq match {
      case b: BufferFloat1NUByte =>
        new BufferVec4fNUByte(
          b.mkDataBuffer(size*4)
        ).asInstanceOf[DataBuffer[Vec4f, D]]
      case _ =>
        new BufferVec4f[D](backingSeq.mkDataBuffer(size*4))
    }
  }
  final def mkDataBuffer(byteBuffer: ByteBuffer) = {
    backingSeq match {
      case b: BufferFloat1NUByte =>
        new BufferVec4fNUByte(
          b.mkDataBuffer(byteBuffer)
        ).asInstanceOf[DataBuffer[Vec4f, D]]
      case _ =>
        new BufferVec4f[D](backingSeq.mkDataBuffer(byteBuffer))
    }
  }
  final def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    backingSeq match {
      case b: BufferFloat1NUByte =>
        new ViewVec4fNUByte(
          b.mkDataBuffer(byteBuffer), offset, stride
        ).asInstanceOf[DataView[Vec4f, D]]
      case _ =>
        new ViewVec4f[D](backingSeq.mkDataBuffer(byteBuffer), offset, stride)
    }
  }
}

private[buffer] final class ArrayVec4f[+D <: ReadFloat](
  override val backingSeq: DataArray[Float1, D]
) extends BaseVec4f[D](backingSeq) with DataArray[Vec4f, D]

private[buffer] final class BufferVec4f[+D <: ReadFloat](
  override val backingSeq: DataBuffer[Float1, D]
) extends BaseVec4f[D](backingSeq) with DataBuffer[Vec4f, D]

private[buffer] final class ViewVec4f[+D <: ReadFloat](
  override val backingSeq: DataBuffer[Float1, D],
  val offset: Int,
  val stride: Int
) extends BaseVec4f[D](backingSeq) with DataView[Vec4f, D]


// Optimised NUByte
private[buffer] final class ArrayVec4fNUByte(
  override val backingSeq: ArrayFloat1NUByte
) extends BaseVec4f[NUByte](backingSeq) with DataArray[Vec4f, NUByte] {
  override def apply(i: Int) :AnyVec4f = {
    val j = i*4
    ConstVec4f(
      backingSeq(j),
      backingSeq(j + 1),
      backingSeq(j + 2),
      backingSeq(j + 3)
    )
  }
  override def update(i: Int, v: AnyVec4f) {
    val j = i*4
    backingSeq(j) = v.x
    backingSeq(j + 1) = v.y
    backingSeq(j + 2) = v.z
    backingSeq(j + 3) = v.w
  }
}

private[buffer] final class BufferVec4fNUByte(
  override val backingSeq: BufferFloat1NUByte
) extends BaseVec4f[NUByte](backingSeq) with DataBuffer[Vec4f, NUByte] {
  override def apply(i: Int) :AnyVec4f = {
    val j = i*4
    ConstVec4f(
      backingSeq(j),
      backingSeq(j + 1),
      backingSeq(j + 2),
      backingSeq(j + 3)
    )
  }
  override def update(i: Int, v: AnyVec4f) {
    val j = i*4
    backingSeq(j) = v.x
    backingSeq(j + 1) = v.y
    backingSeq(j + 2) = v.z
    backingSeq(j + 3) = v.w
  }
}

private[buffer] final class ViewVec4fNUByte(
  override val backingSeq: BufferFloat1NUByte,
  val offset: Int,
  val stride: Int
) extends BaseVec4f[NUByte](backingSeq) with DataView[Vec4f, NUByte] {
  override def apply(i: Int) :AnyVec4f = {
    val j = offset + i*step
    ConstVec4f(
      backingSeq(j),
      backingSeq(j + 1),
      backingSeq(j + 2),
      backingSeq(j + 3)
    )
  }
  override def update(i: Int, v: AnyVec4f) {
    val j = offset + i*step
    backingSeq(j) = v.x
    backingSeq(j + 1) = v.y
    backingSeq(j + 2) = v.z
    backingSeq(j + 3) = v.w
  }
}
