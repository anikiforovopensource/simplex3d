/*
 * Simplex3dEngine - Test Package
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

package simplex3d.script

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.engine._
import simplex3d.engine.graphics._
import simplex3d.engine.util._


abstract class FunctionRendererApp extends default.FullscreenEffectApp {
  
  protected def animateFunction(function: (inVec2i, Double, inVec2) => ReadVec3) {
    effect.function = function
  }
  
  
  protected val effect = new FunctionRenderer
  
  protected final class FunctionRenderer extends FullscreenEffect("Function Renderer") {
    var function: (inVec2i, Double, inVec2) => ReadVec3 = _
    
    protected val texture = Value(new TextureBinding[Texture2d[Vec3]])
    private val textureDims = Vec2i(0)

    private val renderLine = (img: Data[Vec3], dims: inVec2i, uptime: Double, y: Int) => {
      val pixel = Vec2(0)
      
      var x = 0; while (x < dims.x) { val i = x + y*dims.x
        
        pixel.x = x; pixel.y = y
        img(i) = function(dims, uptime, pixel)
        
        x += 1
      }
    }
    
    override protected def render(renderManager: RenderManager, time: TimeStamp) {
      val dims = renderManager.renderContext.viewportDimensions()
      if (textureDims != dims) {
        textureDims := dims
        
        val texture = Texture2d.fromData(dims, DataBuffer[Vec3, UByte](dims.x*dims.y))
        texture.mipMapFilter = MipMapFilter.Disabled
        texture.anisotropyLevel = 1
        
        this.texture.update := texture
      }
      
      if (function != null) {
        val img = texture.get.bound.write
        (0 until dims.y).par.foreach(y => renderLine(img, dims, time.total, y))
      }
      
      super.render(renderManager, time)
    }
    
    val fragmentShader = """
      uniform ivec2 se_viewDimensions;
      uniform sampler2D texture;
      
      void main() {
        gl_FragColor = texture2D(texture, gl_FragCoord.xy/vec2(se_viewDimensions), 0.5);
      }
    """
  }
}
