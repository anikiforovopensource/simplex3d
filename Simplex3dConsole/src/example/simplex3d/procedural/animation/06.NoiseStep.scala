package example.simplex3d.procedural.animation

import simplex3d.math.double._
import simplex3d.math.doublex.functions._
import simplex3d.console.extension.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object NoiseStep extends Application {

  val zoom = 1.0/50
  val changeSpeed = 1.0/3
  val scrollSpeed = 10

  animateFunction("Noise Step") { (dims, time, pixel) =>
    val p = pixel + time*scrollSpeed
    smoothstep(0, 0.7, noise3(Vec3(p*zoom, time*changeSpeed)))
  }
}
