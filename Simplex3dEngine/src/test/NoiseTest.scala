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
import simplex3d.algorithm.noise._
import simplex3d.engine._


object NoiseTest extends FunctionRendererApp with impl.lwjgl.App {
  val title = "Noise Test"
  
  def main(args: Array[String]) {
    val settings = new Settings(
      verticalSync = true,
      performanceLog = true,
      resolution = Some(Vec2i(800, 600))
    )
    
    launch(settings)
  }

  
  val noise = new Noise(ClassicalGradientNoise)
    
  val zoom = 1.0/50
  val changeSpeed = 1.0/3
  val scrollSpeed = 10
  
  animateFunction { (dims, time, pixel) =>
    val p = pixel + time*scrollSpeed
    Vec3((noise(Vec3(p*zoom, time*changeSpeed)) + 1)/2)
  }
}
