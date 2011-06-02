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
object Tiled extends App {

  val turbulence = new TiledTurbulence(
    tile = Vec4(2),
    frequency = 1,
    octaves = 3, lacunarity = 2.5, persistence = 0.5,
    roundness = 0.3
  )
  val noise = (p: inVec2) => turbulence(p)

  drawFunction("Tiled") { (dims, pixel) =>
    val p = pixel/150
    val tiled = turbulence(p)*0.6
    Vec3(tiled)
  }

}
