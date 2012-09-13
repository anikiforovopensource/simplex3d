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


/**
 * @author Aleksey Nikiforov (lex)
 */
trait CompositionFactory[F <: Format, B <: Raw with Tangible] {
  def components: Int
  def formatManifest: ClassManifest[F]
  def accessorManifest: ClassManifest[F#Accessor]

  def mkReadDataArray[P <: B](primitives: ReadDataArray[F#Component, P]) :ReadDataArray[F, P]
  def mkReadDataBuffer[P <: B](primitives: ReadDataBuffer[F#Component, P]) :ReadDataBuffer[F, P]

  protected def mkReadDataViewInstance[P <: B](
    primitives: ReadDataBuffer[F#Component, P], offset: Int, stride: Int
  ) :ReadDataView[F, P]


  final def mkReadDataView[P <: B](
    primitives: ReadDataBuffer[F#Component, P], offset: Int, stride: Int
  ) :ReadDataView[F, P] = {
    mkViewOrBuffer(primitives, offset, stride)
  }

  final def mkDataArray[P <: B](primitives: DataArray[F#Component, P]) :DataArray[F, P] = {
    if (primitives.isReadOnly) throw new IllegalArgumentException(
      "The DataArray must not be read-only."
    )
    mkReadDataArray(primitives).asInstanceOf[DataArray[F, P]]
  }
  final def mkDataBuffer[P <: B](primitives: DataBuffer[F#Component, P]) :DataBuffer[F, P] = {
    if (primitives.isReadOnly) throw new IllegalArgumentException(
      "The DataBuffer must not be read-only."
    )
    mkReadDataBuffer(primitives).asInstanceOf[DataBuffer[F, P]]
  }
  final def mkDataView[P <: B](primitives: DataBuffer[F#Component, P], offset: Int, stride: Int) :DataView[F, P] = {
    if (primitives.isReadOnly) throw new IllegalArgumentException(
      "The DataBuffer must not be read-only."
    )
    mkViewOrBuffer(primitives, offset, stride).asInstanceOf[DataView[F, P]]
  }
  
  private[this] final def mkViewOrBuffer[P <: B](
    primitives: ReadDataBuffer[F#Component, P], offset: Int, stride: Int
  ) :ReadDataView[F, P] = {
    if (offset == 0 && stride == components) mkReadDataBuffer(primitives)
    else mkReadDataViewInstance(primitives, offset, stride)
  }
}
