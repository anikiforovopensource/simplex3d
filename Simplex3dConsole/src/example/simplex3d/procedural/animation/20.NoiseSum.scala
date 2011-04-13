package example.simplex3d.procedural.animation

import simplex3d.math.double._
import simplex3d.math.doublex.functions._
import simplex3d.console.extension.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object NoiseSum extends Application {

  val zoom = 1.0/200
  val scrollSpeed = 5.0
  val changeSpeed = 1.0/10

  val octaves = 4
  val lacunarity = 1.8
  val amplitudeDivisor = 2
  val expectedMagnitude = 1.5

  val frequencyFactors = (for (i <- 0 until octaves) yield pow(lacunarity, i)).toArray
  val amplitudeFactors = (for (i <- 0 until octaves) yield pow(amplitudeDivisor, -i)).toArray

  def noiseSum(p: inVec3) = {
    def octave(i: Int, p: inVec3) = {
      noise1(p*frequencyFactors(i))*amplitudeFactors(i)
    }

    var sum = 0.0; var i = 0; while (i < octaves) {
      sum += octave(i, p + i)
      i += 1
    }
    sum
  }

  animateFunction("Noise Sum") { (dims, time, pixel) =>
    val p = pixel + time*scrollSpeed
    val noise = noiseSum(Vec3(p*zoom , time*changeSpeed))
    Vec3((noise + expectedMagnitude)/(2*expectedMagnitude))
  }
}
