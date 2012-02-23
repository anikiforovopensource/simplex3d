package simplex3d.example.algorithm.noise

import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.algorithm.noise._
import simplex3d.script.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object Noise2D extends App {

  val zoom = 1.0/30
  val scrollSpeed = 20

  val noise = new Noise1(ClassicalGradientNoise)

  animateFunction("Noise2D") { (dims, time, pixel) =>
    val p = pixel + time*scrollSpeed
    Vec3((noise(p*zoom) + 1)/2)
  }
}
