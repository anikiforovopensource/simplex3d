package example.simplex3d.procedural.animation

import simplex3d.math.double._
import simplex3d.math.doublex.functions._
import simplex3d.console.extension.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object Turbulence extends Application {

  val zoom = 1.0/150
  val scrollSpeed = 0.0//5.0
  val changeSpeed = 0.7/10

  val octaves = 3
  val lacunarity = 2.2
  val amplitudeDivisor = 2.5

  val frequencyFactors = (for (i <- 0 until octaves) yield pow(lacunarity, i)).toArray
  val amplitudeFactors = (for (i <- 0 until octaves) yield pow(amplitudeDivisor, -i)).toArray

  def noiseSum(p: inVec3) = {
    def octave(i: Int, p: inVec3) = {
      val f = amplitudeFactors(i)
      abs(noise1(p*frequencyFactors(i)) - 0.3*f)*f
    }

    var sum = 0.0; var i = 0; while (i < octaves) {
      sum += octave(i, p + i)
      i += 1
    }
    sum
  }

  animateFunction("Turbulence") { (dims, time, pixel) =>
    val p = pixel + time*scrollSpeed
    Vec3(noiseSum(Vec3(p*zoom , time*changeSpeed))*0.6)
  }
}
