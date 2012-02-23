package simplex3d.example.algorithm.noise.advanced

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.algorithm.noise._
import simplex3d.script.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object Flame extends App {

  val zoom = 1.0/300
  val changeSpeed = 0.4

  val turbulence = new Noise1(new Turbulence(
    ClassicalGradientNoise,
    frequency = 1.5,
    octaves = 3, lacunarity = 1.5, persistence = 0.5,
    roundness = 0.3
  ))

  animateFunction("Flame", Vec2i(250, 250)) { (dims, time, pixel) =>
    val u = Vec2(pixel.x - dims.x*0.5, dims.y - pixel.y)*zoom
    val p = Vec3(u, time*changeSpeed)
    val n = turbulence(p*1.1)*0.01
    val a = (0.1/(u.x*u.x + n))*smootherstep(0, 2, u.y - 0.0)
    val c = a
    Vec3(c, c*0.15, 0)
  }
}
