package example.simplex3d.procedural.animation

import simplex3d.math.double._
import simplex3d.math.doublex.functions._
import simplex3d.console.extension.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object Sun extends App {

  val zoom = 1.0/200
  val changeSpeed = 0.7/10

  val octaves = 3
  val lacunarity = 2.5
  val amplitudeDivisor = 1.5

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


  val background = ConstVec3(0)
  val low = ConstVec3(1, 0.3, 0)
  val high = ConstVec3(1, 0.8, 0)

  animateFunction("Sun") { (dims, time, pixel) =>
    val u = (pixel - dims*0.5)*zoom
    val p = Vec3(u, time*changeSpeed)
    val a = length(u) + noiseSum(p)*0.05
    val b = smoothstep(0.5, 1.3, a)
    mix(mix(low, high, 1 - a)*1.5, background, b)
  }
}
