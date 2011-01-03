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


/**
 * @author Aleksey Nikiforov (lex)
 */
@serializable @SerialVersionUID(8104346712419693669L)
private[data] class PrimitiveSInt(val rawType: Int) extends SerializablePrimitive {
  protected def toReadDataArray(): ReadDataArray[_, _] = {
    import RawType._

    rawType match {
      case SByte => DataArray[SInt, SByte](content.asInstanceOf[Array[Byte]])
      case UByte => DataArray[SInt, UByte](content.asInstanceOf[Array[Byte]])
      case SShort => DataArray[SInt, SShort](content.asInstanceOf[Array[Short]])
      case UShort => DataArray[SInt, UShort](content.asInstanceOf[Array[Char]])
      case SInt => DataArray[SInt, SInt](content.asInstanceOf[Array[Int]])
      case UInt => DataArray[SInt, UInt](content.asInstanceOf[Array[Int]])
    }
  }
}


@serializable @SerialVersionUID(8104346712419693669L)
private[data] class CompositeSInt(val components: Int) extends SerializableComposite {
  protected def toReadDataArray(primitive: ReadDataArray[_ <: Primitive, _]): ReadDataArray[_ <: Composite, _] = {
    components match {
      case 2 => FactoryVec2i.mkReadDataArray(primitive.asInstanceOf[ReadDataArray[SInt, _ <: DefinedInt]])
      case 3 => FactoryVec3i.mkReadDataArray(primitive.asInstanceOf[ReadDataArray[SInt, _ <: DefinedInt]])
      case 4 => FactoryVec4i.mkReadDataArray(primitive.asInstanceOf[ReadDataArray[SInt, _ <: DefinedInt]])
    }
  }
}
