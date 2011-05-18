package example.simplex3d.procedural.texture

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.console.extension.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object Tiling extends App {

  def tile(dims: inVec2, noise: inVec2 => Double, u: inVec2) :Double = {
    val p = mod(u, dims)
    (
      noise(p) * (dims.x - p.x) * (dims.y - p.y) +
      noise(Vec2(p.x - dims.x, p.y)) * (p.x) * (dims.y - p.y) +
      noise(Vec2(p.x - dims.x, p.y - dims.y)) * (p.x) * (p.y) +
      noise(Vec2(p.x, p.y - dims.y)) * (dims.x - p.x) * (p.y)
    ) / (dims.x * dims.y)
  }


  val octaves = 3
  val lacunarity = 2.5
  val amplitudeDivisor = 2
  val expectedMagnitude = 1.7

  val frequencyFactors = (for (i <- 0 until octaves) yield pow(lacunarity, i)).toArray
  val amplitudeFactors = (for (i <- 0 until octaves) yield pow(amplitudeDivisor, -i)).toArray

  def noiseSum(p: inVec2) = {
    def octave(i: Int, p: inVec2) = {
      val f = amplitudeFactors(i)
      abs(noise1(p*frequencyFactors(i)) - 0.3*f)*f
    }

    var sum = 0.0; var i = 0; while (i < octaves) {
      sum += octave(i, p + i)
      i += 1
    }
    sum
  }

  
  def contrast(f: Double, x: Double) :Double = f*(x - 0.5) + 0.5

  drawFunction("Tiled Turbulence") { (dims, pixel) =>
    val scale = 1.0/200
    val p = pixel*scale
    val tiled = tile(Vec2(300*scale), noiseSum(_), p)/expectedMagnitude
    Vec3(contrast(1.5, tiled))
  }
}
