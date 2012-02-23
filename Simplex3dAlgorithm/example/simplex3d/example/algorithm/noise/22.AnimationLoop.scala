package simplex3d.example.algorithm.noise

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.algorithm.noise._
import simplex3d.script.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object AnimationLoop extends App {

  val turbulence = new Noise1(new TiledTurbulence(
    ClassicalGradientNoise,
    tile = Vec4(3, 3, 1, 3),
    frequency = 1,
    octaves = 3, lacunarity = 2.0, persistence = 0.5,
    roundness = 0.3
  ))

  animateFunction("Animation Loop (5 Seconds)") { (dims, time, pixel) =>
    val p = Vec3(pixel/100, time*0.2)
    val tiled = turbulence(p)*0.6
    Vec3(tiled)
  }

}
