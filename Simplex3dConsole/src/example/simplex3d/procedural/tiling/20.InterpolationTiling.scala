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
object InterpolationTiling extends App {

  def tile(dims: inVec2, noise: inVec2 => Double, u: inVec2) :Double = {
    val p = mod(u, dims)
    (
      noise(p) * (dims.x - p.x) * (dims.y - p.y) +
      noise(Vec2(p.x - dims.x, p.y)) * (p.x) * (dims.y - p.y) +
      noise(Vec2(p.x - dims.x, p.y - dims.y)) * (p.x) * (p.y) +
      noise(Vec2(p.x, p.y - dims.y)) * (dims.x - p.x) * (p.y)
    ) / (dims.x * dims.y)
  }

  def contrast(f: Double, x: Double) :Double = f*(x - 0.5) + 0.5

  val turbulence = new Turbulence(
    frequency = 1,
    octaves = 3, lacunarity = 2.5, persistence = 0.5,
    roundness = 0.3
  )
  val noise = (p: inVec2) => turbulence(p + 10)

  drawFunction("Interpolation Tiling") { (dims, pixel) =>
    val scale = 1.0/150
    val p = pixel*scale
    val tiled = tile(Vec2(300*scale), noise, p)*0.7
    Vec3(contrast(1.2, tiled))
  }
}
