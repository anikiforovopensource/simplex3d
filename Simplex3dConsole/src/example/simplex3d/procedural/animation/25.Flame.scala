package example.simplex3d.procedural.animation

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.doublex.functions._
import simplex3d.console.extension.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object Flame extends App {

  val zoom = 1.0/300
  val changeSpeed = 0.4

  val octaves = 3
  val lacunarity = 1.5
  val amplitudeDivisor = 2.0

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

  animateFunction("Flame", Vec2i(250, 250)) { (dims, time, pixel) =>
    val u = Vec2(pixel.x - dims.x*0.5, dims.y - pixel.y)*zoom
    val p = Vec3(u, time*changeSpeed)
    val n = noiseSum(p*1.1)*0.01
    val a = (0.1/(u.x*u.x + n))*smootherstep(0, 2, u.y - 0.0)
    val c = a
    Vec3(c, c*0.15, 0)
  }
}
