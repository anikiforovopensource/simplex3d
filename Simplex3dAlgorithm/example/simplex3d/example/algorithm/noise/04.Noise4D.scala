package simplex3d.example.algorithm.noise

import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.algorithm.noise._
import simplex3d.script.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object Noise4D extends App {

  val zoom = 1.0/50
  val changeSpeed = 1.0/3
  val scrollSpeed = 10

  val noise = new Noise(ClassicalGradientNoise)

  animateFunction("Noise4D") { (dims, time, pixel) =>
    val h = dims*0.5 - pixel
    val p = pixel + time*scrollSpeed
    Vec3((noise(Vec4(p*zoom, time*changeSpeed, length(h)*zoom)) + 1)/2)
  }
}
