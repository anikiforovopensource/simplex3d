/*
 * Simplex3d, MathTest package
 * Copyright (C) 2010 Simplex3d Team
 *
 * This file is part of Simplex3dMathTest.
 *
 * Simplex3dMathTest is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dMathTest is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package visual.math

import visual.math.draw._

import simplex3d.math._
import simplex3d.math.doublem.renamed._
import simplex3d.math.doublem.DoubleMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
object MandelbrotSmoothTest {

  def main(args: Array[String]) {
    FunFrame.launch(new Fun {

    val dynamicColor = false

    val startScale: Double = 200
    val zoomSpeed: Double = 1.1

//    val zoomPoint = Vec2(
//        -0.743643887037158704752191506114774,
//        0.131825904205311970493132056385139
//    )
    val zoomPoint = Vec2(0.001643721971153, -0.822467633298876)

    val staticMap = {
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
    val dynamicMap = {
      val bands = new ColorBands()
      bands.start = Vec3(0, 0, 1)
      bands put new Shade(Vec3(0.1), 0.4, 20)
      bands put new Gradient(Vec3(1, 0.55, 0), 10)
      bands put new Shade(Vec3(0.1), 0.4, 20)
      bands put new Gradient(Vec3(0, 0, 1), 10)
      bands put new Shade(Vec3(0.1), 0.4, 20)
      bands put new Gradient(Vec3(0.58, 0, 0.827), 40)
      bands put new Gradient(Vec3(0.2, 0, 0), 100)
      bands.generate()
    }
    val colors = if (dynamicColor) dynamicMap else staticMap
    val iterations = colors.length
    val extraIterations = 3
    val escapeColor = colors(iterations - 1)

    final def apply(pixel: AnyVec2, time: Double)
    :AnyVec3 =
    {
      val mid = dimensions/2
      val zoom = startScale + pow(zoomSpeed, 30 + time)
      val c = (pixel - mid)/zoom + zoomPoint
      
      val z = Vec2(0)
      import z.{x, y}

      var i = 0; while (x*x + y*y <= 4 && i < iterations) {
        val xt = x*x - y*y + c.x

        y = 2*x*y + c.y
        x = xt

        i += 1
      }

      val extra = i + extraIterations
      while (i < extra) {
        val xt = x*x - y*y + c.x

        y = 2*x*y + c.y
        x = xt

        i += 1
      }

      var g = i - log2(abs(log2(length(z))))
      if (dynamicColor) { g = g - pow(log(zoom), 1.35) }
      g = clamp(g, 0, colors.length - 2)
      val j = int(g)
      mix(colors(j), colors(j + 1), fract(g))
    }})
  }

class ColorBands {
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

abstract class ColorGen {
  def to: Vec3
  def generate(from: Vec3) :List[ConstVec3]

  final def gradient(start: Vec3, end: Vec3, count: Int) = {
    (for (i <- 0 until count) yield {
      ConstVec3(mix(start, end, double(i)/count))
    }).toList
  }
}

class Gradient(val to: Vec3, val count: Int) extends ColorGen {
  def generate(from: Vec3) = gradient(from, to, count)
}

class Shade(val shade: Vec3, val contrast: Double, val count: Int)
extends ColorGen
{
  val to = Vec3(0)
  def generate(from: Vec3) = {
    to := from

    val clamped = clamp(contrast, 0, 1)

    val pre = int(round(count*clamped))
    val post = count - pre

    gradient(from, shade, pre) :::
    gradient(mix(shade, from, clamped), from, post)
  }
}

}
