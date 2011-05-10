/*
 * Simplex3d, CoreData module
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


/**
 * @author Aleksey Nikiforov (lex)
 */
trait CompositionFactory[E <: Meta, B <: Defined] {
  def components: Int
  def metaManifest: ClassManifest[E]
  def readManifest: ClassManifest[E#Read]

  def mkReadDataArray[P <: B](primitives: ReadDataArray[E#Component, P]) :ReadDataArray[E, P]
  def mkReadDataBuffer[P <: B](primitives: ReadDataBuffer[E#Component, P]) :ReadDataBuffer[E, P]

  protected def mkReadDataViewInstance[P <: B](
    primitives: ReadDataBuffer[E#Component, P], offset: Int, stride: Int
  ) :ReadDataView[E, P]


  final def mkReadDataView[P <: B](
    primitives: ReadDataBuffer[E#Component, P], offset: Int, stride: Int
  ) :ReadDataView[E, P] = {
    mkViewOrBuffer(primitives, offset, stride)
  }

  final def mkDataArray[P <: B](primitives: DataArray[E#Component, P]) :DataArray[E, P] = {
    if (primitives.isReadOnly) throw new IllegalArgumentException(
      "The DataArray must not be read-only."
    )
    mkReadDataArray(primitives).asInstanceOf[DataArray[E, P]]
  }
  final def mkDataBuffer[P <: B](primitives: DataBuffer[E#Component, P]) :DataBuffer[E, P] = {
    if (primitives.isReadOnly) throw new IllegalArgumentException(
      "The DataBuffer must not be read-only."
    )
    mkReadDataBuffer(primitives).asInstanceOf[DataBuffer[E, P]]
  }
  final def mkDataView[P <: B](primitives: DataBuffer[E#Component, P], offset: Int, stride: Int) :DataView[E, P] = {
    if (primitives.isReadOnly) throw new IllegalArgumentException(
      "The DataBuffer must not be read-only."
    )
    mkViewOrBuffer(primitives, offset, stride).asInstanceOf[DataView[E, P]]
  }
  
  private[this] final def mkViewOrBuffer[P <: B](
    primitives: ReadDataBuffer[E#Component, P], offset: Int, stride: Int
  ) :ReadDataView[E, P] = {
    if (offset == 0 && stride == components) mkReadDataBuffer(primitives)
    else mkReadDataViewInstance(primitives, offset, stride)
  }
}
