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

import simplex3d.math.types._
import simplex3d.data._


@SerialVersionUID(8104346712419693669L)
sealed abstract class ReadTextureBinding[T <: Texture[_]] (protected[engine] var texture: T)
extends Readable[TextureBinding[T]] with Cloneable[ReadTextureBinding[T]] with NestedBinding with Serializable
{
  final def mutableCopy() = new TextureBinding[T](texture)
  
  final def bound: T = texture
  final def isBound = (texture != null)
  final def unbind() { texture = null.asInstanceOf[T] }
  
  def isAccessible = (isBound && bound.isAccessible)
  def isWritable = (isBound && bound.isWritable)
  def src: DirectSrc with ContiguousSrc = if (isBound) bound.src else null
  
  
  // XXX enable after the next Scala release.
//  final override def equals(other: Any) :Boolean = {
//    other match {
//      case r: ReadTextureBinding[_] => texture == r.texture
//      case t: Texture[_] => texture == t
//      case _ => false
//    }
//  }
  
  final override def hashCode() :Int = texture.hashCode
  final override def toString() :String = "TextureBinding" + "(" + texture + ")"
}

@SerialVersionUID(8104346712419693669L)
final class TextureBinding[T <: Texture[_]](texture: T = null) extends ReadTextureBinding[T](texture)
with Writable[TextureBinding[T]] with Cloneable[TextureBinding[T]] with Serializable
{
  type Read = ReadTextureBinding[T]
  
  override def clone() = new TextureBinding[T](texture)

  def :=(r: Readable[TextureBinding[T]]) { texture_=(r.asInstanceOf[ReadTextureBinding[T]]texture) }
  def :=(t: T) { texture_=(t) }
}
