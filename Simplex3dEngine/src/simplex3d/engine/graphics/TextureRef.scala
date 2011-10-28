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
sealed abstract class ReadTextureRef[T <: Texture[_]] (protected[engine] var texture: T)
extends Readable[ReadTextureRef[T]] with Cloneable[ReadTextureRef[T]] with NestedBinding with Serializable
{
  type Mutable = TextureRef[T]
  final def mutableCopy() = new TextureRef[T](texture)
  
  final def bound: T = texture
  final def isBound = (texture != null)
  final def unbind() { texture = null.asInstanceOf[T] }
  
  def isAccessible = (isBound && bound.isAccessible)
  def isWritable = (isBound && bound.isWritable)
  def src: DirectSrc with ContiguousSrc = if (isBound) bound.src else null

  
  final override def equals(other: Any) :Boolean = {
    other match {
      case r: ReadTextureRef[_] => texture == r.texture
      case t: Texture[_] => texture == t
      case _ => false
    }
  }

  final override def hashCode() :Int = texture.hashCode
  final override def toString() :String = "TextureRef" + "(" + texture + ")"
}

@SerialVersionUID(8104346712419693669L)
final class TextureRef[T <: Texture[_]](texture: T = null) extends ReadTextureRef[T](texture)
with Mutable[ReadTextureRef[T]] with Cloneable[TextureRef[T]] with Serializable
{
  override def clone() = new TextureRef[T](texture)

  def :=(r: ReadTextureRef[T]) { texture_=(r.texture) }
  def :=(t: T) { texture_=(t) }
}
