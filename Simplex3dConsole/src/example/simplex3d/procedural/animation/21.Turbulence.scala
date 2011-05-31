package example.simplex3d.procedural.animation

import simplex3d.math.double._
import simplex3d.math.doublex.functions._
import simplex3d.console.extension.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object Turbulence extends App {

  val zoom = 1.0/150
  val scrollSpeed = 0.0//5.0
  val changeSpeed = 0.7/10

  val turbulence = new Turbulence(
    frequency = 1,
    octaves = 3, lacunarity = 2.2, persistence = 0.4,
    roundness = 0.3
  )

  animateFunction("Turbulence") { (dims, time, pixel) =>
    val p = (pixel + time*scrollSpeed)*zoom
    Vec3(turbulence(Vec3(p, time*changeSpeed))*0.6)
  }
}
