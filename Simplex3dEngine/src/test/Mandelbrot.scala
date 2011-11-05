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

package test

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.engine._
import simplex3d.engine.graphics._


object Mandelbrot extends default.BasicFullscreenEffectApp {
  val title = "Mandelbrot"
  
  override lazy val settings = new Settings(
    fullscreen = false,
    verticalSync = true,
    capabilitiesLog = true,
    performanceLog = true,
    resolution = None//Some(Vec2i(800, 600))
  )
  
  
  val effect = new FullscreenEffect("Mandelbrot Fractal") {
    var startZoom = 30.0
    var zoomSpeed = 1.2
    
    // Non-private ShaderProperty values are automatically bound to shader uniforms with matching name and type.
    val zoomPoint = DefinedProperty[Vec2](Vec2(
      -0.743643887037158704752191506114774,
       0.131825904205311970493132056385139
    ))
    
    
    protected val zoom = DefinedProperty[DoubleRef](1.0)
    protected val iterations = DefinedProperty[IntRef](0)
    
    protected val colorTexture = DefinedProperty[TextureBinding[Texture2d[Vec3]]](new TextureBinding);
    {
      val colors: Array[ConstVec3] = ColorPreset.generate()
      val texture = Texture2d(Vec2i(colors.length, 1), DataBuffer[Vec3, UByte](colors: _*))
      
      colorTexture.mutable := texture
      iterations.mutable := colors.length
    }
    
    override def update(time: TimeStamp) {
      zoom.mutable := pow(zoomSpeed, startZoom + time.total)
    }
    
    val shaderSrc = """
uniform ivec2 se_viewDimensions;

// Non-private ShaderProperty values are automatically bound to shader uniforms with matching name and type.
uniform vec2 zoomPoint;

uniform float zoom;
uniform int iterations;

uniform sampler2D colorTexture;

const int extraIterations = 4;


void main() {
  vec2 pixel = gl_FragCoord.xy;
  vec2 mid = vec2(se_viewDimensions)*0.5;

  vec2 c = (pixel - mid)/zoom + zoomPoint;
  vec2 z = vec2(0);

  int i = 0; while (z.x*z.x + z.y*z.y <= 4.0 && i < iterations) {
    float xtemp = z.x*z.x - z.y*z.y + c.x;

    z.y = 2.0*z.x*z.y + c.y;
    z.x = xtemp;

    i += 1;
  }

  int extra = i + extraIterations;
  while (i < extra) {
    float xtemp = z.x*z.x - z.y*z.y + c.x;

    z.y = 2.0*z.x*z.y + c.y;
    z.x = xtemp;

    i += 1;
  }

  float g = float(i) - log2(abs(log2(length(z))));
  g = clamp(g, 0.0, float(iterations - 1));

  gl_FragColor = texture2D(colorTexture, vec2(g/float(iterations), 0.5));
}
  """
  }
}


/** Generates an array of colors.
 */
object ColorPreset {
  
  def generate() :Array[ConstVec3] = {
    val bands = new ColorBands()
    bands.start = Vec3(0, 0, 1)
    bands put new Gradient(Vec3(0.9), 10)
    bands put new Gradient(Vec3(0, 0, 1), 10)
    bands put new Shade(Vec3(0.1), 0.7, 20)
    bands put new Gradient(Vec3(1, 0.55, 0), 10)
    bands put new Shade(Vec3(0.1), 0.7, 20)
    bands put new Gradient(Vec3(0, 0, 1), 10)
    bands put new Shade(Vec3(0.1), 0.7, 20)
    bands put new Gradient(Vec3(0.58, 0, 0.827), 50)
    bands put new Gradient(Vec3(0.2, 0, 0), 100)
    bands.generate()
  }
  
  
  private class ColorBands {
    import scala.collection.mutable.ListBuffer

    var start = Vec3(1)
    private val generators = ListBuffer[ColorGen]()

    def put(g: ColorGen) { generators += g }

    def generate() :Array[ConstVec3] = {
      val bands = ListBuffer[ConstVec3]()
      var from = start

      for (gen <- generators) {
        bands appendAll gen.generate(from)
        from = gen.to
      }

      bands.toArray
    }
  }

  private abstract class ColorGen {
    def to: Vec3
    def generate(from: inVec3) :List[ConstVec3]

    final def gradient(start: inVec3, end: inVec3, count: Int) = {
      (for (i <- 0 until count) yield {
        mix(start, end, i.toDouble/count).toConst
      }).toList
    }
  }

  private class Gradient(val to: Vec3, val count: Int) extends ColorGen {
    def generate(from: inVec3) = {
      gradient(from, to, count)
    }
  }

  private class Shade(val shade: Vec3, val contrast: Double, val count: Int) extends ColorGen {
    val to = Vec3(0)
    def generate(from: inVec3) = {
      to := from

      val clamped = clamp(contrast, 0, 1)

      val pre = (round(count*clamped)).toInt
      val post = count - pre

      gradient(from, shade, pre) :::
      gradient(mix(shade, from, clamped), from, post)
    }
  }
}
