package example.simplex3d.procedural.texture

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.algorithm.noise._
import simplex3d.console.script.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object Noise2D extends App {

  val noise = new Noise(ClassicalGradientNoise)

  drawFunction("Noise2D") { (dims, p) =>
    val n = noise(p/50)
    Vec3((n + 1)/2)
  }

}
