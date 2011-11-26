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

import javax.imageio.ImageIO
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.data._
import simplex3d.data.double._


// XXX decouple asset loader into resolver and format loader
class ClasspathLoader extends AssetLoader {
  
  def loadRgbImg(path: String) :Option[(ConstVec2i, RgbTextureData)] = {
    val stream = this.getClass.getClassLoader.getResourceAsStream(path)
    if (stream == null) return None
    val img = ImageIO.read(stream)
    
    val width = img.getWidth
    val height = img.getHeight
    
    val data = DataBuffer[Vec3, UByte](width*height)
    val buffer = data.buffer()
    
    var y = 0; while (y < height) {
      var x = 0; while (x < width) {
        
        val pixel = img.getRGB(x, y)
        val index = (x + (height - 1 - y)*width)*3
        buffer.put(index, (pixel >> 16).toByte)
        buffer.put(index + 1, (pixel >> 8).toByte)
        buffer.put(index + 2, pixel.toByte)
        
        x += 1
      }
      y += 1
    }
    
    Some(ConstVec2i(width, height), data)
  }
  
  def loadRgbaImg(path: String) :Option[(ConstVec2i, RgbaTextureData)] = {
    val stream = this.getClass.getClassLoader.getResourceAsStream(path)
    if (stream == null) return None
    val img = ImageIO.read(stream)
    
    val width = img.getWidth
    val height = img.getHeight
    
    val data = DataBuffer[Vec4, UByte](width*height)
    val buffer = data.buffer()
    
    var y = 0; while (y < height) {
      var x = 0; while (x < width) {
        
        val i = img.getRGB(x, y)
        val pixel = img.getRGB(x, y)
        val index = (x + (height - 1 - y)*width)*3
        buffer.put(index, (pixel >> 16).toByte)
        buffer.put(index + 1, (pixel >> 8).toByte)
        buffer.put(index + 2, pixel.toByte)
        buffer.put(index + 3, (pixel >> 24).toByte)
        
        x += 1
      }
      y += 1
    }
    
    Some(ConstVec2i(width, height), data)
  }
}
