package example.simplex3d.procedural.animation

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.doublex.functions._
import simplex3d.console.extension.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object Wood extends App {

  val zoom = 1.0/150

  val octaves = 4
  val lacunarity = 2.0
  val amplitudeDivisor = 2.0

  val frequencyFactors = (for (i <- 0 until octaves) yield pow(lacunarity, i)).toArray
  val amplitudeFactors = (for (i <- 0 until octaves) yield pow(amplitudeDivisor, -i)).toArray

  def noiseSum(p: inVec3) = {
    def octave(i: Int, p: inVec3) = {
      val f = amplitudeFactors(i)
      abs(noise1(p*frequencyFactors(i))*f)
    }

    var sum = 0.0; var i = 0; while (i < octaves) {
      sum += octave(i, p)
      i += 1
    }
    sum
  }


  animateFunction("Wood", Vec2i(400, 800)) { (dims, time, pixel) =>
    val zSlice = 3.0
    val zoomed = (pixel - dims*0.5)*zoom
    val p = Vec3(zoomed.x, zoomed.y + time*0.1, zSlice)
    wood(p)
  }


  // Trunk is around y = 0, xz form the plane with growth circles.
  // Use z to control the distance from the trunk center. Take different slices by rotating the position around x axis.
  // Start by making the growth circles: val dist = length(p.xz); var a = abs(noise1(dist*ringFreq)); mix(darkerCircles, lighterCircles, a)
  // Add irregularity to the circles: val dist = length(p.xz) + noise1(Vec3(p.x, p.y*verticalChange, p.z))*irregularity
  // Add small perturbations to irregularities by addings sum of abs noise octaves: val a = abs(noise1(dist*ringFreq + noiseSum(p)*0.2))
  // Add low frequency blemish in lighter areas: val b = smootherstep(0.1, 1, a)*noise1(Vec2(p.x*90, p.y*10))*0.05; color + blemish*b
  // Best slices are at smaller angles, from 0 to 10 degrees.
  // Increase irregularities to widen the useful pattern. Be very gentle with verticalChange factor if you decide to play with it.

  val rotation = Quat4 rotateX(radians(1))
  val lighterCircles = ConstVec3(0.68, 0.45, 0.19)
  val darkerCircles = ConstVec3(0.52, 0.23, 0.11)
  val blemish = ConstVec3(0.63, 0.42, 0.17)
  val ringFreq = 8.5
  val verticalChange = 0.3
  val irregularity = 0.1

  def wood(pos: inVec3) :Vec3 = {
    val p = rotateVector(pos, rotation)
    val dist = length(p.xz) + noise1(Vec3(p.x, p.y*verticalChange, p.z))*irregularity
    val a = abs(noise1(dist*ringFreq + noiseSum(p)*0.2))
    val color = mix(darkerCircles, lighterCircles, a)
    val b = smootherstep(0.1, 1, a)*noise1(Vec2(p.x*50, p.y*5))*0.05
    color + blemish*b
  }
}
