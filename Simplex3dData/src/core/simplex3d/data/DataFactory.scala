/*
 * Simplex3dData - Core Module
 * Copyright (C) 2010-2012, Aleksey Nikiforov
 *
 * This file is part of Simplex3dData.
 *
 * Simplex3dData is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dData is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.data

import java.nio._
import scala.annotation._
import scala.annotation.unchecked._
import RawType._


/**
 * @author Aleksey Nikiforov (lex)
 */
trait DataFactory[F <: Format, +R <: Raw] {
  def rawType: Int
  def components: Int
  def bytesPerComponent: Int
  def formatManifest: ClassManifest[F]
  def accessorManifest: ClassManifest[F#Accessor]
  def primitives: DataFactory[F#Component, R]

  def mkDataArray(array: R#Array @uncheckedVariance) :DataArray[F, R]
  def mkReadDataBuffer(byteBuffer: ByteBuffer) :ReadDataBuffer[F, R]
  protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) :ReadDataView[F, R]


  final def mkDataArray(size: Int) :DataArray[F, R] = {
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
  ) :DataBuffer[F, R] = {
    if (byteBuffer.isReadOnly) throw new IllegalArgumentException(
      "The buffer must not be read-only."
    )
    mkReadDataBuffer(byteBuffer).asInstanceOf[DataBuffer[F, R]]
  }

  final def mkDataBuffer(size: Int) :DataBuffer[F, R] = {
    mkDataBuffer(ByteBuffer.allocateDirect(size*RawType.byteLength(rawType)*components))
  }


  final def mkReadDataView(
    byteBuffer: ByteBuffer, offset: Int, stride: Int
  ) :ReadDataView[F, R] = {
    mkViewOrBuffer(byteBuffer, offset, stride)
  }

  final def mkDataView(
    byteBuffer: ByteBuffer, offset: Int, stride: Int
  ) :DataView[F, R] = {
    if (byteBuffer.isReadOnly) throw new IllegalArgumentException(
      "The buffer must not be read-only."
    )
    mkViewOrBuffer(byteBuffer, offset, stride).asInstanceOf[DataView[F, R]]
  }

  private[this] final def mkViewOrBuffer(byteBuffer: ByteBuffer, offset: Int, stride: Int) :ReadDataView[F, R] = {
    if (offset == 0 && stride == components) mkReadDataBuffer(byteBuffer)
    else mkReadDataViewInstance(byteBuffer, offset, stride)
  }
}


object DataFactory {
  def apply[F <: Format, R <: Raw with Tangible](
    implicit composition: CompositionFactory[F, _ >: R], primitives: PrimitiveFactory[F#Component, R]
  ) :DataFactory[F, R] = {
    composition.mkDataArray(primitives.mkDataArray(0))
  }
}
