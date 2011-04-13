package example.simplex3d.procedural.animation

import simplex3d.math.double._
import simplex3d.math.doublex.functions._
import simplex3d.console.extension.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object Biomass extends Application {

  val zoom = 1.0/100
  val changeSpeed = 1.0/10

  val amplitudeDivisor = 2
  val expectedMagnitude = 0.75

  val amplitudeFactors = (for (i <- 0 until 3) yield pow(amplitudeDivisor, -i)).toArray

  def noiseSum(p: inVec3) = {
    val varying = amplitudeDivisor + noise1(p)*0.02

    def octave(i: Int, p: inVec3, c: outVec3) = {
      val frequencyFactor = pow(varying, i)
      c(i) = noise1(p*frequencyFactor)*amplitudeFactors(i)
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
    val noise = noiseSum(Vec3(p*zoom , time*changeSpeed))
    Vec3((noise + expectedMagnitude)/(2*expectedMagnitude))
  }
}
