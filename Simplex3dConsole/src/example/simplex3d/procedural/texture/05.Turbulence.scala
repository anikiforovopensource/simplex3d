package example.simplex3d.procedural.texture

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.noise._
import simplex3d.console.extension.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object Turbulence extends App {

  val octaves = 3
  val lacunarity = 2
  val persistence = 0.5
  val roundness = 0.3
  val expectedMagnitude = 1.6

  val frequencyFactors = (for (i <- 0 until octaves) yield pow(lacunarity, i)).toArray
  val amplitudeFactors = (for (i <- 0 until octaves) yield pow(persistence, i)).toArray

  val noise = new Noise(ClassicalGradientNoise)

  def noiseSum(p: inVec2) = {
    def octave(i: Int, p: inVec2) = {
      val f = amplitudeFactors(i)
      abs(noise(p*frequencyFactors(i)) - roundness*f)*f
    }

    var sum = 0.0; var i = 0; while (i < octaves) {
      sum += octave(i, p + i)
      i += 1
    }
    sum
  }

  drawFunction("Turbulence") { (dims, pixel) =>
    val p = pixel/100
    Vec3(noiseSum(p)/expectedMagnitude)
  }

}
