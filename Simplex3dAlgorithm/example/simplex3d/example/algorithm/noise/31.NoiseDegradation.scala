package simplex3d.example.algorithm.noise

import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.algorithm.noise._
import simplex3d.script.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object NoiseDegradation extends App {

  
  val scale = 1.0/50
  val noiseSpeed = 1.0/3
  val scrollSpeed = 10

  val noise = new Noise1(ClassicalGradientNoise)
  
  val f = 3.0
  def largeOffset(time: Double) :Double = {
    val local = time.toInt % 18
    if (local < 3) 5e13
    else if (local < 6) 5e13*f
    else if (local < 9) 5e13*f*f
    else if (local < 12) 5e13*f*f*f
    else if (local < 15) 5e13*f*f*f*f
    else 5e13*f*f*f*f*f
  }

  // Shows gradual noise degradation at large offsets.
  animateFunction("Noise Degradation") { (dims, time, pixel) =>
    val p = pixel + time*scrollSpeed
    Vec3((noise(Vec3(p*scale + largeOffset(time), time*noiseSpeed)) + 1)/2)
  }
}
