package simplex3d.example.algorithm.noise

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.algorithm.noise._
import simplex3d.script.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object Tiled extends App {

  val turbulence = new Noise1(new TiledTurbulence(
    ClassicalGradientNoise,
    tile = Vec4(2),
    frequency = 1,
    octaves = 3, lacunarity = 2.5, persistence = 0.5,
    roundness = 0.3
  ))

  drawFunction("Tiled") { (dims, pixel) =>
    val p = pixel/150
    val tiled = turbulence(p)*0.6
    Vec3(tiled)
  }

}
