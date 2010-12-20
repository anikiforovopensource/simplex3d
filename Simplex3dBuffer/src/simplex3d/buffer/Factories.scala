/*
 * Simplex3d, CoreBuffer module
 * Copyright (C) 2010, Simplex3d Team
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
import scala.annotation._
import scala.annotation.unchecked._
import RawType._


/**
 * @author Aleksey Nikiforov (lex)
 */
trait DataFactory[E <: Meta, +R <: Raw] {
  def rawType: Int
  def components: Int
  def elemManifest: ClassManifest[E]

  def mkDataArray(array: R#Array @uncheckedVariance) :DataArray[E, R]
  def mkReadDataBuffer(byteBuffer: ByteBuffer) :ReadDataBuffer[E, R]
  protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) :ReadDataView[E, R]


  final def mkDataArray(size: Int) :DataArray[E, R] = {
    val array = ((rawType: @switch) match {
      case SByte | UByte => new Array[Byte](size*components)
      case SShort | HFloat=> new Array[Short](size*components)
      case UShort => new Array[Char](size*components)
      case SInt | UInt => new Array[Int](size*components)
      case RFloat => new Array[Float](size*components)
      case RDouble => new Array[Double](size*components)
    }).asInstanceOf[AnyRef]

    mkDataArray(array.asInstanceOf[R#Array])
  }


  final def mkDataBuffer(
    byteBuffer: ByteBuffer
  ) :DataBuffer[E, R] = {
    if (byteBuffer.isReadOnly) throw new IllegalArgumentException(
      "The buffer must not be read-only."
    )
    mkReadDataBuffer(byteBuffer).asInstanceOf[DataBuffer[E, R]]
  }

  final def mkDataBuffer(size: Int) :DataBuffer[E, R] = {
    mkDataBuffer(ByteBuffer.allocateDirect(size*RawType.byteLength(rawType)*components))
  }


  final def mkReadDataView(
    byteBuffer: ByteBuffer, offset: Int, stride: Int
  ) :ReadDataView[E, R] = {
    mkViewOrBuffer(byteBuffer, offset, stride)
  }

  final def mkDataView(
    byteBuffer: ByteBuffer, offset: Int, stride: Int
  ) :DataView[E, R] = {
    if (byteBuffer.isReadOnly) throw new IllegalArgumentException(
      "The buffer must not be read-only."
    )
    mkViewOrBuffer(byteBuffer, offset, stride).asInstanceOf[DataView[E, R]]
  }

  private[this] final def mkViewOrBuffer(byteBuffer: ByteBuffer, offset: Int, stride: Int) :ReadDataView[E, R] = {
    if (offset == 0 && stride == components) mkReadDataBuffer(byteBuffer)
    else mkReadDataViewInstance(byteBuffer, offset, stride)
  }
}


trait IndexFactory[+R <: Unsigned] extends DataFactory[SInt, R] {
  def mkReadIndexBuffer(byteBuffer: ByteBuffer) :ReadIndexBuffer[R] =
    mkReadDataBuffer(byteBuffer).asInstanceOf[ReadIndexBuffer[R]]

  def mkIndexArray(size: Int) :IndexArray[R] = mkDataArray(size)
  def mkIndexArray(array: R#Array @uncheckedVariance) :IndexArray[R] = mkDataArray(array)
  def mkIndexBuffer(size: Int) :IndexBuffer[R] = mkDataBuffer(size)
  def mkIndexBuffer(byteBuffer: ByteBuffer) :IndexBuffer[R] = mkDataBuffer(byteBuffer)
}


trait CompositeFactory //TODO
