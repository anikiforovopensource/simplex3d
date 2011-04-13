package example.simplex3d.procedural.animation

import simplex3d.math.double._
import simplex3d.math.doublex.functions._
import simplex3d.console.extension.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object VaryingFrequency extends Application {

  val zoom = 1.0/100
  val changeSpeed = 1.0/10

  val octaves = 3
  val amplitudeDivisor = 2
  val expectedMagnitude = 1.5

  val amplitudeFactors = (for (i <- 0 until octaves) yield pow(amplitudeDivisor, -i)).toArray

  def noiseSum(p: inVec3) = {
    val varying = amplitudeDivisor + noise1(p)*0.02

    def octave(i: Int, p: inVec3) = {
      val frequencyFactor = pow(varying, i)
      noise1(p*frequencyFactor)*amplitudeFactors(i)
    }

    var sum = 0.0; var i = 0; while (i < octaves) {
      sum += octave(i, p + i)
      i += 1
    }
    sum
  }

  animateFunction("Varying Frequency") { (dims, time, pixel) =>
    val p = Vec2(pixel.x + 5000, pixel.y)
    val noise = noiseSum(Vec3(p*zoom , time*changeSpeed))
    Vec3((noise + expectedMagnitude)/(2*expectedMagnitude))
  }
}
