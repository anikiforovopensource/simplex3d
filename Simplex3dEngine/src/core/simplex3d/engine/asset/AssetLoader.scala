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

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.data._
import simplex3d.data.double._


// XXX split asset loader into format resolver and format loader
trait AssetLoader {
  type RgbTextureData = Data[Vec3] with DirectSrc with ContiguousSrc
  type RgbaTextureData = Data[Vec4] with DirectSrc with ContiguousSrc
  
  def loadRgbImg(path: String) :Option[(ConstVec2i, RgbTextureData)]
  def loadRgbaImg(path: String) :Option[(ConstVec2i, RgbaTextureData)]
  
  def loadObj(path: String)
  :Option[(
    DataBuffer[SInt, Unsigned],// indices
    DataBuffer[Vec3, RFloat], // vertices
    Option[DataBuffer[Vec3, RFloat]], // normals
    Option[DataBuffer[Vec2, RFloat]] // texCoords
  )]
}
