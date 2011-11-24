package example.simplex3d.procedural.animation

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.algorithm.noise._
import simplex3d.console.script.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object InterpolationLooping extends App {

  def loop(period: Double, noise: inVec3 => Double, u: inVec3) :Double = {
    val p = Vec3(u.x, u.y, mod(u.z, period))
    ((period - p.z)*noise(p) + p.z*noise(Vec3(p.x, p.y, p.z - period)))/period
  }

  val turbulence = new Turbulence(
    ClassicalGradientNoise,
    frequency = 1.2,
    octaves = 3, lacunarity = 2.2, persistence = 0.4,
    roundness = 0.3
  )
  val noise = (p: inVec3) => turbulence(p)

  // Interpolation looping produces blurring artifacts.
  animateFunction("Interpolation Looping") { (dims, time, pixel) =>
    val speed = 0.1
    val u = pixel/150
    val p = Vec3(u, time*speed)
    val intensity = loop(5*speed, noise, p)*0.6
    Vec3(intensity)
  }
}
