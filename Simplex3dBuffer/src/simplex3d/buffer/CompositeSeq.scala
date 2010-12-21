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
import scala.annotation.unchecked._


/**
 * Extend this class and add implicit factories to your package object.
 *
 * @author Aleksey Nikiforov (lex)
 */
abstract class CompositeSeq[E <: Composite, +R <: Raw, B <: Defined](
  primitive: ReadContiguous[E#Component, R],
  off: Int, str: Int
) extends BaseSeq[E, E#Const, E#Read, R](
  primitive.sharedStore, primitive, primitive.readOnly,
  off, str
) with CompositionFactory[E, B] {
  final def rawType = backing.rawType
  final def normalized: Boolean = backing.normalized

  def mkReadDataArray[P <: B](primitive: ReadDataArray[E#Component, P]) :ReadDataArray[E, P]
  def mkReadDataBuffer[P <: B](primitive: ReadDataBuffer[E#Component, P]) :ReadDataBuffer[E, P]
  protected def mkReadDataViewInstance[P <: B](primitive: ReadDataBuffer[E#Component, P], off: Int, str: Int) :ReadDataView[E, P]


  final def mkDataArray(array: R#Array @uncheckedVariance) :DataArray[E, R] =
    mkReadDataArray(
      backing.mkDataArray(array).asInstanceOf[DataArray[E#Component, B]]
    ).asInstanceOf[DataArray[E, R]]

  final def mkReadDataBuffer(byteBuffer: ByteBuffer) :ReadDataBuffer[E, R] =
    mkReadDataBuffer(
      backing.mkReadDataBuffer(byteBuffer).asInstanceOf[ReadDataBuffer[E#Component, B]]
    ).asInstanceOf[ReadDataBuffer[E, R]]

  protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) :ReadDataView[E, R] =
    mkReadDataView(
      backing.mkReadDataBuffer(byteBuffer).asInstanceOf[ReadDataBuffer[E#Component, B]], off, str
    ).asInstanceOf[ReadDataView[E, R]]


  private[buffer] final def mkReadOnlyInstance() :ReadDataSeq[E, R] = {
    val self: AnyRef = this
    (self match {
      case _: DataArray[_, _] => mkReadDataArray(
          backing.asReadOnly().asInstanceOf[DataArray[E#Component, B]]
        )
      case _: DataBuffer[_, _] => mkReadDataBuffer(
          backing.asReadOnly().asInstanceOf[DataBuffer[E#Component, B]]
        )
      case _: DataView[_, _] => mkReadDataView(
          backing.asReadOnly().asInstanceOf[DataBuffer[E#Component, B]], offset, stride
        )
    }).asInstanceOf[ReadDataSeq[E, R]]
  }
}
