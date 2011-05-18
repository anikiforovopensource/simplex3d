package example.simplex3d.procedural.animation

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.doublex.functions._
import simplex3d.console.extension.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object Fire extends App {

  val zoom = 1.0/150
  val scrollSpeed = 0.0//5.0
  val changeSpeed = 4.0/10

  val octaves = 3
  val lacunarity = 1.8
  val amplitudeDivisor = 1.7

  val frequencyFactors = (for (i <- 0 until octaves) yield pow(lacunarity, i)).toArray
  val amplitudeFactors = (for (i <- 0 until octaves) yield pow(amplitudeDivisor, -i)).toArray

  def noiseSum(p: inVec3) = {
    def octave(i: Int, p: inVec3) = {
      val f = amplitudeFactors(i)
      sin(p.y + 0.5 + abs(noise1(p*frequencyFactors(i)) - 0.3*f)*f)
    }

    var sum = 0.0; var i = 0; while (i < octaves) {
      sum += octave(i, p + i)
      i += 1
    }
    sum
  }

  animateFunction("Fire", Vec2i(600, 280)) { (dims, time, pixel) =>
    val p = (pixel + time*scrollSpeed)*zoom
    val noise = noiseSum(Vec3(p , time*changeSpeed))*0.9
    Vec3(noise, noise*0.3, 0)
  }
}
