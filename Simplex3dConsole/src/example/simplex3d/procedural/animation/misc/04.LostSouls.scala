package example.simplex3d.procedural.animation.misc

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.doublex.functions._
import simplex3d.console.extension.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object LostSouls extends App {

  val zoom = 1.0/150
  val changeSpeed = 1.0/10

  val octaves = 4
  val amplitudeDivisor = 2
  val expectedMagnitude = 1.5

  val amplitudeFactors = (for (i <- 0 until octaves) yield pow(amplitudeDivisor, -i)).toArray

  def noiseSum(p: inVec3) = {
    val varying = amplitudeDivisor + noise1(p)*0.02

    def octave(i: Int, p: inVec3) = {
      val frequencyFactor = pow(varying, i)
      val f = amplitudeFactors(i)
      abs(noise1(p*frequencyFactor) - 0.3*f)*f
    }

    val c1 = (octave(1, p + 1) + octave(3, p + 3))*1.2
    val c2 = (octave(0, p + 0) + octave(2, p + 2))*1.2
    Vec3(0, c1*0.3 + c2*0.5, c1 + c2)
  }

  animateFunction("Lost Souls", Vec2i(400, 300)) { (dims, time, pixel) =>
    val p = Vec2(pixel.x + 2000, pixel.y)
    var noise = noiseSum(Vec3(p*zoom , time*changeSpeed))
    Vec3(noise*0.6)
  }
}
