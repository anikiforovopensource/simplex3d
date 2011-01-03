/*
 * Simplex3d, CoreData module
 * Copyright (C) 2010-2011, Simplex3d Team
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
trait DataFactory[E <: Meta, +R <: Raw] {
  def rawType: Int
  def components: Int
  def elemManifest: ClassManifest[E]
  def readManifest: ClassManifest[E#Read]
  def backing: DataFactory[E#Component, R]

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


trait PrimitiveFactory[E <: Primitive, +R <: Raw] extends DataFactory[E, R]


trait CompositionFactory[E <: Meta, B <: Defined] {
  def components: Int
  def elemManifest: ClassManifest[E]
  def readManifest: ClassManifest[E#Read]

  def mkReadDataArray[P <: B](primitive: ReadDataArray[E#Component, P]) :ReadDataArray[E, P]
  def mkReadDataBuffer[P <: B](primitive: ReadDataBuffer[E#Component, P]) :ReadDataBuffer[E, P]

  protected def mkReadDataViewInstance[P <: B](
    primitive: ReadDataBuffer[E#Component, P], offset: Int, stride: Int
  ) :ReadDataView[E, P]


  final def mkReadDataView[P <: B](
    primitive: ReadDataBuffer[E#Component, P], offset: Int, stride: Int
  ) :ReadDataView[E, P] = {
    mkViewOrBuffer(primitive, offset, stride)
  }

  final def mkDataArray[P <: B](primitive: DataArray[E#Component, P]) :DataArray[E, P] = {
    if (primitive.readOnly) throw new IllegalArgumentException(
      "The DataArray must not be read-only."
    )
    mkReadDataArray(primitive).asInstanceOf[DataArray[E, P]]
  }
  final def mkDataBuffer[P <: B](primitive: DataBuffer[E#Component, P]) :DataBuffer[E, P] = {
    if (primitive.readOnly) throw new IllegalArgumentException(
      "The DataBuffer must not be read-only."
    )
    mkReadDataBuffer(primitive).asInstanceOf[DataBuffer[E, P]]
  }
  final def mkDataView[P <: B](primitive: DataBuffer[E#Component, P], offset: Int, stride: Int) :DataView[E, P] = {
    if (primitive.readOnly) throw new IllegalArgumentException(
      "The DataBuffer must not be read-only."
    )
    mkViewOrBuffer(primitive, offset, stride).asInstanceOf[DataView[E, P]]
  }
  
  private[this] final def mkViewOrBuffer[P <: B](
    primitive: ReadDataBuffer[E#Component, P], offset: Int, stride: Int
  ) :ReadDataView[E, P] = {
    if (offset == 0 && stride == components) mkReadDataBuffer(primitive)
    else mkReadDataViewInstance(primitive, offset, stride)
  }
}
