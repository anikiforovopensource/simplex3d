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
import scala.annotation.unchecked._


/**
 * Extend this class and add implicit factories to your package object.
 *
 * @author Aleksey Nikiforov (lex)
 */
abstract class CompositeSeq[E <: Composite, +R <: Raw, B <: Defined](
  prim: ReadContiguous[E#Component, R],
  off: Int, str: Int
) extends AbstractData[E, E#Const, E#Read, R](
  prim.sharedStore, prim, prim.readOnly,
  off, str
) with CompositionFactory[E, B] {
  final def rawType = primitive.rawType
  final def normalized: Boolean = primitive.normalized

  def mkReadDataArray[P <: B](prim: ReadDataArray[E#Component, P]) :ReadDataArray[E, P]
  def mkReadDataBuffer[P <: B](prim: ReadDataBuffer[E#Component, P]) :ReadDataBuffer[E, P]
  protected def mkReadDataViewInstance[P <: B](
    prim: ReadDataBuffer[E#Component, P], off: Int, str: Int
  ) :ReadDataView[E, P]


  final def mkDataArray(array: R#Array @uncheckedVariance) :DataArray[E, R] =
    mkReadDataArray(
      primitive.mkDataArray(array).asInstanceOf[DataArray[E#Component, B]]
    ).asInstanceOf[DataArray[E, R]]

  final def mkReadDataBuffer(byteBuffer: ByteBuffer) :ReadDataBuffer[E, R] =
    mkReadDataBuffer(
      primitive.mkReadDataBuffer(byteBuffer).asInstanceOf[ReadDataBuffer[E#Component, B]]
    ).asInstanceOf[ReadDataBuffer[E, R]]

  protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) :ReadDataView[E, R] =
    mkReadDataView(
      primitive.mkReadDataBuffer(byteBuffer).asInstanceOf[ReadDataBuffer[E#Component, B]], off, str
    ).asInstanceOf[ReadDataView[E, R]]


  private[data] final def mkReadOnlyInstance() :ReadDataSeq[E, R] = {
    val self: AnyRef = this
    (self match {
      case _: DataArray[_, _] => mkReadDataArray(
          primitive.asReadOnly().asInstanceOf[DataArray[E#Component, B]]
        )
      case _: DataBuffer[_, _] => mkReadDataBuffer(
          primitive.asReadOnly().asInstanceOf[DataBuffer[E#Component, B]]
        )
      case _: DataView[_, _] => mkReadDataView(
          primitive.asReadOnly().asInstanceOf[DataBuffer[E#Component, B]], offset, stride
        )
    }).asInstanceOf[ReadDataSeq[E, R]]
  }
}
