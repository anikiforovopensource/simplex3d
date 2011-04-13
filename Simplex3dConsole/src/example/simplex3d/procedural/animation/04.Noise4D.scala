package example.simplex3d.procedural.animation

import simplex3d.math.double._
import simplex3d.math.doublex.functions._
import simplex3d.console.extension.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object Noise4D extends Application {

  val zoom = 1.0/50
  val changeSpeed = 1.0/3
  val scrollSpeed = 10

  animateFunction("Noise4D") { (dims, time, pixel) =>
    val h = dims*0.5 - pixel
    val p = pixel + time*scrollSpeed
    Vec3((noise1(Vec4(p*zoom, time*changeSpeed, length(h)*zoom)) + 1)/2)
  }
}
