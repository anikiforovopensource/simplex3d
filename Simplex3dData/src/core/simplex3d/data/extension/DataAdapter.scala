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
import scala.reflect._
import scala.reflect.runtime.universe._
import scala.annotation.unchecked._
import simplex3d.math._


/**
 * @author Aleksey Nikiforov (lex)
 */
@SerialVersionUID(8104346712419693669L)
abstract class DataAdapter[F <: CompositeFormat, B <: Raw with Tangible](final val components: Int)(
  implicit
  final val formatTag: ClassTag[F],
  final val accessorTag: ClassTag[F#Accessor],
  @transient boundTag: TypeTag[B]
)
extends CompositionFactory[F, B] with Serializable {
  
  def apply(primitives: inContiguous[F#Component, Raw], j: Int) :F#Accessor#Const
  def update(primitives: Contiguous[F#Component, Raw], j: Int, value: F#Accessor#Read) :Unit

  def mkReadDataArray[P <: B](primitives: ReadDataArray[F#Component, P])
  :ReadDataArray[F, P] = {
    enforceRawEnum(primitives.rawEnum)
    new GenericArray(this, primitives)
  }
  def mkReadDataBuffer[P <: B](primitives: ReadDataBuffer[F#Component, P])
  :ReadDataBuffer[F, P] = {
    enforceRawEnum(primitives.rawEnum)
    new GenericBuffer(this, primitives)
  }
  protected[data] def mkReadDataViewInstance[P <: B](primitives: ReadDataBuffer[F#Component, P], off: Int, str: Int)
  :ReadDataView[F, P] = {
    enforceRawEnum(primitives.rawEnum)
    new GenericView(this, primitives, off, str)
  }

  private[this] final val allowedTypes: Array[Int] = {
    val allowed = for (t <- RawEnum.TypeTags.AllTangible if t.tpe <:< boundTag.tpe) yield RawEnum.TypeTags.toRawEnum(t)
    allowed.toArray
  }
  private[this] def enforceRawEnum(rawEnum: Int) {
    var i = 0; while (i < allowedTypes.size) {
      if (allowedTypes(i) == rawEnum) return
      i += 1
    }

    throw new IllegalArgumentException(
      RawEnum.toString(rawEnum) + " is not one of the allowed types: " +
      allowedTypes.map(RawEnum.toString(_)).mkString(", ") + "."
    )
  }
}
