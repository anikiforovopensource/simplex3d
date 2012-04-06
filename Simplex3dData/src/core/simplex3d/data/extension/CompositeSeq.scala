/*
 * Simplex3dData - Core Module
 * Copyright (C) 2010-2011, Aleksey Nikiforov
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
package extension

import java.nio._
import scala.annotation.unchecked._


/** Extend this class and add implicit factories to your package object.
 *
 * @author Aleksey Nikiforov (lex)
 */
abstract class CompositeSeq[F <: CompositeFormat, +R <: Raw, B <: Tangible](
  prim: ReadContiguous[F#Component, R],
  off: Int, str: Int
) extends AbstractData[F#Accessor#Const, F#Accessor#Read](
  null, prim, prim.isReadOnly,
  off, str
) with DataSeq[F, R] with CompositionFactory[F, B] {
  
  final def rawType = primitives.rawType
  final def isNormalized: Boolean = primitives.isNormalized

  def mkReadDataArray[P <: B](prim: ReadDataArray[F#Component, P]) :ReadDataArray[F, P]
  def mkReadDataBuffer[P <: B](prim: ReadDataBuffer[F#Component, P]) :ReadDataBuffer[F, P]
  protected def mkReadDataViewInstance[P <: B](
    prim: ReadDataBuffer[F#Component, P], off: Int, str: Int
  ) :ReadDataView[F, P]


  final def mkDataArray(array: R#Array @uncheckedVariance) :DataArray[F, R] =
    mkReadDataArray(
      primitives.mkDataArray(array).asInstanceOf[DataArray[F#Component, B]]
    ).asInstanceOf[DataArray[F, R]]

  final def mkReadDataBuffer(byteBuffer: ByteBuffer) :ReadDataBuffer[F, R] =
    mkReadDataBuffer(
      primitives.mkReadDataBuffer(byteBuffer).asInstanceOf[ReadDataBuffer[F#Component, B]]
    ).asInstanceOf[ReadDataBuffer[F, R]]

  protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) :ReadDataView[F, R] =
    mkReadDataView(
      primitives.mkReadDataBuffer(byteBuffer).asInstanceOf[ReadDataBuffer[F#Component, B]], off, str
    ).asInstanceOf[ReadDataView[F, R]]


  private[data] final def mkReadOnlyInstance() :Read = {
    val self: AnyRef = this
    (self match {
      case _: DataArray[_, _] => mkReadDataArray(
          primitives.asReadOnly().asInstanceOf[DataArray[F#Component, B]]
        )
      case _: DataBuffer[_, _] => mkReadDataBuffer(
          primitives.asReadOnly().asInstanceOf[DataBuffer[F#Component, B]]
        )
      case _: DataView[_, _] => mkReadDataView(
          primitives.asReadOnly().asInstanceOf[DataBuffer[F#Component, B]], offset, stride
        )
    }).asInstanceOf[Read]
  }
}
