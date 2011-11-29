package simplex3d.example.algorithm.noise.advanced

import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.algorithm.noise._
import simplex3d.script.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object VaryingFrequency extends App {

  val zoom = 1.0/70
  val changeSpeed = 1.0/10

  val octaves = 3
  val persistence = 0.5
  val expectedMagnitude = 1.5

  val amplitudeFactors = (for (i <- 0 until octaves) yield pow(persistence, i)).toArray
  val noise = new Noise(ClassicalGradientNoise)

  def noiseSum(p: inVec3) = {
    val varying = 2 + noise(p)*0.02

    def octave(i: Int, p: inVec3) = {
      val frequencyFactor = pow(varying, i)
      noise(p*frequencyFactor)*amplitudeFactors(i)
    }

    var sum = 0.0; var i = 0; while (i < octaves) {
      sum += octave(i, p + i)
      i += 1
    }
    sum
  }

  animateFunction("Varying Frequency") { (dims, time, pixel) =>
    val p = Vec2(pixel.x + 1500, pixel.y)
    val n = noiseSum(Vec3(p*zoom , time*changeSpeed))
    Vec3((n + expectedMagnitude)/(2*expectedMagnitude))
  }
}
