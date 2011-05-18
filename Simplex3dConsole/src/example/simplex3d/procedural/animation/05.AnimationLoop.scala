package example.simplex3d.procedural.animation

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.doublex.functions._
import simplex3d.console.extension.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object AnimationLoop extends App {

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

  def loop(period: Double, noise: inVec3 => Double, u: inVec3) :Double = {
    val p = Vec3(u.x, u.y, mod(u.z, period))
    ((period - p.z)*noise(p) + p.z*noise(Vec3(p.x, p.y, p.z - period)))/period
  }

  val noise = (p: inVec3) => noiseSum(p)
  animateFunction("Loop") { (dims, time, pixel) =>
    val u = (pixel + time*scrollSpeed)*zoom
    val p = Vec3(u, time*changeSpeed)
    val intensity = loop(10*changeSpeed, noise, p)*0.6
    Vec3(intensity)
  }
}
