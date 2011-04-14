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
object VaryingFrequency extends Application {

  val octaves = 5
  val amplitudeDivisor = 2
  val expectedMagnitude = 1.5

  val amplitudeFactors = (for (i <- 0 until octaves) yield pow(amplitudeDivisor, -i)).toArray

  def noiseSum(p: inVec2) = {
    val varying = amplitudeDivisor + noise1(p)*0.01

    def octave(i: Int, p: inVec2) = {
      val frequencyFactor = pow(varying, i)
      noise1(p*frequencyFactor)*amplitudeFactors(i)
    }

    var sum = 0.0; var i = 0; while (i < octaves) {
      sum += octave(i, p + i)
      i += 1
    }
    sum
  }

  drawFunction("Varying Frequency") { (dims, pixel) =>
    val p = (dims/2.0 + pixel)/80
    Vec3((noiseSum(p) + expectedMagnitude)/(2*expectedMagnitude))
  }

}
