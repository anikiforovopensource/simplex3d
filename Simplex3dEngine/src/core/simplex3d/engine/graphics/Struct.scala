/*
 * Simplex3dEngine - Core Module
 * Copyright (C) 2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dEngine.
 *
 * Simplex3dEngine is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dEngine is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.engine
package graphics

import java.util.logging._
import simplex3d.math.types._
import simplex3d.engine.util._


/** All Struct subclasses must define a no-argument constructor.
 */
trait ReadStruct extends Protected with Binding with StructuralChangeNotifier {
  type Read <: ReadStruct
  type Mutable <: Struct
}
  
/** All Struct subclasses must define a no-argument constructor.
 */
trait Struct extends ReadStruct with Accessible {
  import Struct.logger._
  
  protected def mkMutable() :Mutable
  
  override def mutableCopy(): Mutable = {
    val copy = mkMutable()
    copy := this.asInstanceOf[copy.Read]
    copy
  }
  
  def fieldNames: ReadArray[String]
  def fields: ReadArray[Binding]
  def listDeclarations: ReadArray[ListDeclaration]
  
  
  private[this] val bindingFromName = (name: String) => {//XXX rather duplicate PathUtil.resolve() method
    val id = PathUtil.find(fieldNames, name)
    if (id == -1) null else fields(id)
  }
  final def resolvePath(path: String) :Binding = PathUtil.resolve(path, bindingFromName)
}

object Struct {
  private final val logger = Logger.getLogger(classOf[RenderContext].getName)
}
