package example.simplex3d.procedural.animation

import simplex3d.math.double._
import simplex3d.math.doublex.functions._
import simplex3d.console.extension.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object Noise2D extends Application {

  val zoom = 1.0/50
  val scrollSpeed = 10

  animateFunction("Noise2D") { (dims, time, pixel) =>
    val p = pixel + time*scrollSpeed
    Vec3((noise1(p*zoom) + 1)/2)
  }
}
