package example.simplex3d.procedural.animation

import simplex3d.math.double._
import simplex3d.math.doublex.functions._
import simplex3d.console.extension.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object NoiseSum extends App {

  val zoom = 1.0/150
  val scrollSpeed = 5.0
  val changeSpeed = 1.0/10

  val noiseSum = new NoiseSum(
    frequency = 1,
    octaves = 4, lacunarity = 1.8, persistence = 0.5
  )

  animateFunction("Noise Sum") { (dims, time, pixel) =>
    val expectedMagnitude = 1.5
    val p = pixel + time*scrollSpeed
    val noise = noiseSum(Vec3(p*zoom , time*changeSpeed))
    Vec3((noise + expectedMagnitude)/(2*expectedMagnitude))
  }
}
