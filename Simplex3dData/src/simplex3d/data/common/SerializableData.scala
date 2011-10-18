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
package common

import java.io._


/**
 * @author Aleksey Nikiforov (lex)
 */
@SerialVersionUID(8104346712419693669L)
sealed abstract class SerializableData extends Serializable {
  final var content: AnyRef = _
}

@SerialVersionUID(8104346712419693669L)
private[data] abstract class SerializablePrimitive extends SerializableData with Serializable {
  final var readOnly: Boolean = _

  @throws(classOf[ObjectStreamException])
  final def readResolve() :Object = {
    if (readOnly) toReadDataArray().asReadOnly()
    else toReadDataArray()
  }

  protected def toReadDataArray(): ReadDataArray[_, _]
}

@SerialVersionUID(8104346712419693669L)
abstract class SerializableComposite extends SerializableData with Serializable {
  @throws(classOf[ObjectStreamException])
  final def readResolve() :Object = {
    toReadDataArray(content.asInstanceOf[ReadDataArray[_ <: PrimitiveFormat, _]])
  }

  protected def toReadDataArray(primitives: ReadDataArray[_ <: PrimitiveFormat, _])
  :ReadDataArray[_ <: CompositeFormat, _]
}
