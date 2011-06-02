package example.simplex3d.procedural.animation

import simplex3d.math.double._
import simplex3d.math.doublex.functions._
import simplex3d.console.extension.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object Turbulence extends App {

  val zoom = 1.0/150
  val changeSpeed = 0.07

  val turbulence = new Turbulence(
    frequency = 1,
    octaves = 3, lacunarity = 2.2, persistence = 0.4,
    roundness = 0.3
  )

  animateFunction("Turbulence") { (dims, time, pixel) =>
    val p = Vec3(pixel*zoom, time*changeSpeed)
    Vec3(turbulence(p)*0.6)
  }
}
