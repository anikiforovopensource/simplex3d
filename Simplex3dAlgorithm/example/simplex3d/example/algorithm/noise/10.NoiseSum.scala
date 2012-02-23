package simplex3d.example.algorithm.noise

import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.algorithm.noise._
import simplex3d.script.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object NoiseSum extends App {

  val zoom = 1.0/150
  val scrollSpeed = 0.1
  val changeSpeed = 0.1

  val noiseSum = new Noise1(new NoiseSum(
    ClassicalGradientNoise,
    frequency = 1,
    octaves = 4, lacunarity = 1.8, persistence = 0.5
  ))

  animateFunction("Noise Sum") { (dims, time, pixel) =>
    val expectedMagnitude = 1.5
    val p = Vec3(pixel*zoom + time*scrollSpeed, time*changeSpeed)
    val noise = noiseSum(p)
    Vec3((noise + expectedMagnitude)/(2*expectedMagnitude))
  }
}
