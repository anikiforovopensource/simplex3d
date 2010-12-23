/*
 * Simplex3d, CoreData module
 * Copyright (C) 2010, Simplex3d Team
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
import scala.reflect._
import scala.annotation.unchecked._
import simplex3d.math._
import simplex3d.data._


/**
 * @author Aleksey Nikiforov (lex)
 */
@serializable @SerialVersionUID(8104346712419693669L)
abstract class DataAdapter[E <: Composite, B <: Defined](final val components: Int)(implicit
  final val elemManifest: ClassManifest[E],
  final val readManifest: ClassManifest[E#Read],
  final val boundManifest: Manifest[B]
)
extends CompositionFactory[E, B] {
  def apply(backing: inContiguous[E#Component, Raw], j: Int) :E#Const
  def update(backing: outContiguous[E#Component, Raw], j: Int, value: E#Read) :Unit

  def mkReadDataArray[P <: B](primitive: ReadDataArray[E#Component, P])
  :ReadDataArray[E, P] = {
    enforceRawType(primitive.rawType)
    new GenericArray(this, primitive)
  }
  def mkReadDataBuffer[P <: B](primitive: ReadDataBuffer[E#Component, P])
  :ReadDataBuffer[E, P] = {
    enforceRawType(primitive.rawType)
    new GenericBuffer(this, primitive)
  }
  protected[data] def mkReadDataViewInstance[P <: B](primitive: ReadDataBuffer[E#Component, P], off: Int, str: Int)
  :ReadDataView[E, P] = {
    enforceRawType(primitive.rawType)
    new GenericView(this, primitive, off, str)
  }

  // Rework this initialization code when/if intersectingType manifest has an accesible list of parents
  // or proper >:> method.
  private[this] final val allowedTypes: Array[Int] = {
    def extractIntersecting(m: Manifest[_]) :Seq[ClassManifest[_]] = {
      val names = m.toString.split(" with ")
      for (name <- names) yield {
        ClassManifest.classType(java.lang.Class.forName(name))
      }
    }

    val bounds = extractIntersecting(boundManifest)
    val allowed = for (m <- RawManifest.AllDefined if bounds.forall(_ >:> m)) yield RawManifest.toRawType(m)
    allowed.toArray
  }
  private[this] def enforceRawType(rawType: Int) {
    var i = 0; while (i < allowedTypes.size) {
      if (allowedTypes(i) == rawType) return
      i += 1
    }
    throw new ClassCastException()
  }
}
