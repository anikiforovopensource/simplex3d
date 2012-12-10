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

import java.util.HashMap
import simplex3d.math.types._
import simplex3d.data._


@SerialVersionUID(8104346712419693669L)
sealed abstract class ReadTextureBinding[T <: Texture[_] with Tangible : ClassManifest]
extends Protected with Cloneable with Binding with Serializable
{
  type Clone <: ReadTextureBinding[T]
  type Read = ReadTextureBinding[T]
  type Mutable = TextureBinding[T]
  
  final def readType = classOf[ReadTextureBinding[T]]
  final def mutableCopy() = new TextureBinding[T](texture)
  
  final val bindingManifest = implicitly[ClassManifest[T]]
  protected[engine] var texture: T = _
  
  final def bound: T = if (texture == null) throw new NoSuchElementException else texture
  final def isBound = (texture != null)
  final def unbind() { texture = null.asInstanceOf[T] }
  
  def isAccessible = (isBound && bound.isAccessible)
  def isWritable = (isBound && bound.isWritable)
  
  def read: ReadData[T#Accessor] with DirectSrc with ContiguousSrc =
    bound.read.asInstanceOf[ReadData[T#Accessor] with DirectSrc with ContiguousSrc]
  
  def write: Data[T#Accessor] with DirectSrc with ContiguousSrc =
    bound.write.asInstanceOf[Data[T#Accessor] with DirectSrc with ContiguousSrc]
  
  def src: DirectSrc with ContiguousSrc = bound.src
  
  
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
  
  def samplerRemapping(path: String, remapping: HashMap[String, String]) {
    val key = {
      val idx = path.lastIndexOf('.')
      val name = if (idx > 0) path.substring(idx + 1) else path
      if (path.contains("[*]")) name + "[*]" else name
    }
    
    val existing = remapping.put(key, path)
    if (existing != null) {
      //XXX warn non-unique names
    }
  }
}


@SerialVersionUID(8104346712419693669L)
final class TextureBinding[T <: Texture[_] with Tangible : ClassManifest] extends ReadTextureBinding[T]
with Accessible with Serializable
{
  def this(texture: T) {
    this()
    this.texture = texture
  }
  
  type Clone = TextureBinding[T]
  override def clone() = new TextureBinding[T](texture)

  def :=(r: ReadTextureBinding[T]) { texture_=(r.asInstanceOf[ReadTextureBinding[T]]texture) }
  def :=(t: T) { texture_=(t) }
}


object TextureBinding {
  import simplex3d.engine.util._
  
  // XXX this is fixed in unreleased Scala 2.10, remove when it gets released.
  def avoidCompilerCrash(a: Any) = a.asInstanceOf[ReadTextureBinding[_]]
  def avoidCompilerCrash(a: ReadArray[Any]) = a.asInstanceOf[ReadArray[ReadTextureBinding[_]]]
  def avoidCompilerCrashB(a: Any) :Boolean = a.isInstanceOf[ReadTextureBinding[_]]
}
