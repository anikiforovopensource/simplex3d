package example.simplex3d.procedural.animation

import simplex3d.math.double._
import simplex3d.math.doublex.functions._
import simplex3d.console.extension.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object Sun extends App {

  val zoom = 1.0/200
  val changeSpeed = 0.7/10

  val background = ConstVec3(0)
  val low = ConstVec3(1, 0.3, 0)
  val high = ConstVec3(1, 0.8, 0)

  val turbulence = new Turbulence(
    frequency = 1.7,
    octaves = 3, lacunarity = 2.5, persistence = 0.66,
    roundness = 0.3
  )

  animateFunction("Sun") { (dims, time, pixel) =>
    val u = (pixel - dims*0.5)*zoom
    val p = Vec3(u, time*changeSpeed)
    val a = length(u) + turbulence(p)*0.05
    val b = smoothstep(0.5, 1.3, a)
    mix(mix(low, high, 1 - a)*1.5, background, b)
  }
}
