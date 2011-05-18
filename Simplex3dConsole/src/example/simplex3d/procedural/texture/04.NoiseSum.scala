package example.simplex3d.procedural.texture

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.console.extension.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object NoiseSum extends App {

  val octaves = 6
  val lacunarity = 1.7
  val amplitudeDivisor = 1.9
  val expectedMagnitude = 1.5

  val frequencyFactors = (for (i <- 0 until octaves) yield pow(lacunarity, i)).toArray
  val amplitudeFactors = (for (i <- 0 until octaves) yield pow(amplitudeDivisor, -i)).toArray

  def noiseSum(p: inVec2) = {
    def octave(i: Int, p: inVec2) = {
      noise1(p*frequencyFactors(i))*amplitudeFactors(i)
    }

    var sum = 0.0; var i = 0; while (i < octaves) {
      sum += octave(i, p + i)
      i += 1
    }
    sum
  }

  drawFunction("Noise Sum") { (dims, pixel) =>
    val p = pixel/200
    Vec3((noiseSum(p) + expectedMagnitude)/(2*expectedMagnitude))
  }

}
