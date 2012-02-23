package simplex3d.example.algorithm.noise

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.algorithm.noise._
import simplex3d.script.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object MultifractalNoise extends App {

  val octaves = 5
  val lacunarity = 2
  val expectedMagnitude = 1.5

  val frequencyFactors = (for (i <- 0 until octaves) yield pow(lacunarity, i)).toArray
  val noise = new Noise1(ClassicalGradientNoise)

  def noiseSum(p: inVec2) = {
    val varying = lacunarity + noise(p)*0.5

    def octave(i: Int, p: inVec2) = {
      val amplitudeFactor = pow(varying, -i)
      noise(p*frequencyFactors(i))*amplitudeFactor
    }

    var sum = 0.0; var i = 0; while (i < octaves) {
      sum += octave(i, p + i)
      i += 1
    }
    sum
  }

  drawFunction("Multifractal Noise") { (dims, pixel) =>
    val p = pixel/100
    Vec3((noiseSum(p) + expectedMagnitude)/(2*expectedMagnitude))
  }

}
