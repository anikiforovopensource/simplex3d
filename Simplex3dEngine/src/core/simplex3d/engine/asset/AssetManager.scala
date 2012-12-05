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

package simplex3d.engine.asset

import scala.collection.mutable.ArrayBuffer
import simplex3d.math.types._
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.engine.graphics._


// XXX add loaded caching and linked sources
abstract class AssetManager {
  // XXX work out default settings, add mass changes
  private val magFilter = ImageFilter.Linear
  private val minFilter = ImageFilter.Linear
  private val mipMapFilter = MipMapFilter.Linear
  private val anisotropyLevel = 4
  
  
  protected val loaders = ArrayBuffer[AssetLoader]()
  
  protected def resolve[R](loadOperation: (AssetLoader) => Option[R]) :Option[R] = {
    var loaded: Option[R] = None
    var i = 0; while (!loaded.isDefined && i < loaders.size) {
      loaded = loadOperation.apply(loaders(i))
      i += 1
    }
    loaded
  }
  
  def loadTexture2d[A <: Accessor](path: String)
    (implicit accessorManifest: ClassManifest[A])
  :Option[Texture2d[A]] = {
    val img = accessorManifest match {
      case Vec3.Manifest => resolve(_.loadRgbImg(path))
      case Vec4.Manifest => resolve(_.loadRgbaImg(path))
      case _ => None
    }
    
    if (img.isDefined) {
      val (dimensions, data) = img.get
      val texture = Texture2d.fromUncheckedSrc[A](dimensions, data)
      
      texture.parameters.magFilter = magFilter
      texture.parameters.minFilter = minFilter
      texture.parameters.mipMapFilter = mipMapFilter
      texture.parameters.anisotropyLevel = anisotropyLevel
      
      Some(texture)
    }
    else None
  }
  
  // getLinkedTexture // shared, link-cached
  // getReadableTexture // shared, data-cached
  // getWritableTexture // unique, data-cached
  // uncacheTexture // clear caches
  
  
  def loadObj(path: String)
  :Option[(
    DataBuffer[SInt, Unsigned],// indices
    DataBuffer[Vec3, RFloat], // vertices
    Option[DataBuffer[Vec3, RFloat]], // normals
    Option[DataBuffer[Vec2, RFloat]] // texCoords
  )] = {
    resolve(_.loadObj(path))
  }
}
