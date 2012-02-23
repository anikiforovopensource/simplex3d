package simplex3d.example.algorithm.noise

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.algorithm.noise._
import simplex3d.script.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object NoiseDistribution extends App {

  val zoom = 1.0/50
  val changeSpeed1 = 1.0/3
  val changeSpeed2 = 1.0/5
  val scrollSpeed = 10

  val noise = new Noise1(ClassicalGradientNoise)

  animateFunction("Noise Distribution") { (dims, time, pixel) =>
    val p = pixel + time*scrollSpeed

    val timeSlot = (time.toInt/10)%4
    val n = {
      if (timeSlot == 0) noise(p.x*zoom)
      else if (timeSlot == 1) noise(p*zoom)
      else if (timeSlot ==2) noise(Vec3(p*zoom, time*changeSpeed1))
      else noise(Vec4(p*zoom, time*changeSpeed1, time*changeSpeed2))
    }

    val scaledNoise = (n + 1)/2
    if (scaledNoise > 0.75) color(Vec3(1, 0, 0), (scaledNoise - 0.75)/0.25)
    else if (scaledNoise > 0.5) color(Vec3(1, 0, 1), (scaledNoise - 0.5)/0.25)
    else if (scaledNoise > 0.25) color(Vec3(0, 0, 1), (scaledNoise - 0.25)/0.25)
    else color(Vec3(0, 1, 1), (scaledNoise)/0.25)
  }

  def color(shadeColor: inVec3, intencity: Double) :Vec3 = {
    mix(shadeColor/2, shadeColor, intencity)
  }
}
