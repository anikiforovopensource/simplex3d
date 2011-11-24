package example.simplex3d.procedural.animation.misc

import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.algorithm.noise._
import simplex3d.console.script.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object Biomass extends App {

  val zoom = 1.0/70
  val changeSpeed = 1.0/10

  val persistence = 0.5
  val expectedMagnitude = 0.7

  val amplitudeFactors = (for (i <- 0 until 3) yield pow(persistence, i)).toArray
  val noise = new Noise(ClassicalGradientNoise)

  def noiseSum(p: inVec3) = {
    val varying = 2 + noise(p)*0.02

    def octave(i: Int, p: inVec3, c: Vec3) = {
      val frequencyFactor = pow(varying, i)
      c(i) = noise(p*frequencyFactor)*amplitudeFactors(i)
    }

    val color = Vec3(0)

    var i = 0; while (i < 3) {
      octave(i, p + i, color)
      i += 1
    }

    color
  }

  animateFunction("Biomass") { (dims, time, pixel) =>
    val p = Vec2(pixel.x + 5000, pixel.y)
    val n = noiseSum(Vec3(p*zoom , time*changeSpeed))
    Vec3((n + expectedMagnitude)/(2*expectedMagnitude))
  }
}
