package example.simplex3d.procedural.animation

import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.noise._
import simplex3d.console.extension.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object Noise2D extends App {

  val zoom = 1.0/30
  val scrollSpeed = 20

  val noise = new Noise(ClassicalGradientNoise)

  animateFunction("Noise2D") { (dims, time, pixel) =>
    val p = pixel + time*scrollSpeed
    Vec3((noise(p*zoom) + 1)/2)
  }
}
