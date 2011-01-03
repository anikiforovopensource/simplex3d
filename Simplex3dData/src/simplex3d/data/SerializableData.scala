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

import java.io._


/**
 * @author Aleksey Nikiforov (lex)
 */
@serializable @SerialVersionUID(8104346712419693669L)
sealed abstract class SerializableData {
  final var content: AnyRef = _
}

@serializable @SerialVersionUID(8104346712419693669L)
private[data] abstract class SerializablePrimitive extends SerializableData {
  final var readOnly: Boolean = _

  @throws(classOf[ObjectStreamException])
  final def readResolve() :Object = {
    if (readOnly) toReadDataArray().asReadOnly()
    else toReadDataArray()
  }

  protected def toReadDataArray(): ReadDataArray[_, _]
}

@serializable @SerialVersionUID(8104346712419693669L)
abstract class SerializableComposite extends SerializableData {
  @throws(classOf[ObjectStreamException])
  final def readResolve() :Object = {
    toReadDataArray(content.asInstanceOf[ReadDataArray[_ <: Primitive, _]])
  }

  protected def toReadDataArray(primitive: ReadDataArray[_ <: Primitive, _]): ReadDataArray[_ <: Composite, _]
}
