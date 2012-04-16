/*
 * Simplex3dEngine - Renderer Module
 * Copyright (C) 2012, Aleksey Nikiforov
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
package renderer

import simplex3d.math.double._
import simplex3d.engine.graphics._


sealed abstract class ReadTextureUnit extends ReadOnly[TextureUnit] {
  def texture: ReadTextureBinding[Texture2d[_]]
  def transformation: ReadMat3x2
}

final class TextureUnit extends ReadTextureUnit with prototype.Struct[TextureUnit] {
  def this(texture: Texture2d[_], transformation: inMat3x2 = Mat3x2.Identity) {
    this()
    
    this.texture := texture
    this.transformation := transformation
  }
  
  type Read = ReadTextureUnit
  protected def mkMutable() = new TextureUnit
  
  val texture = new TextureBinding[Texture2d[_]]
  val transformation = Mat3x2(1)
  
  init(classOf[TextureUnit])
}
