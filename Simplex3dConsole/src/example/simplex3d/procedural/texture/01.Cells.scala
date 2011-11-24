package example.simplex3d.procedural.texture

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.console.script.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object Cells extends App {

  def cell(x: Double, size: Double) = {
    floor(x/size)*size
  }

  drawFunction("Cells") { (dims, p) =>
    Vec3(cell(p.x, 40)/dims.x, cell(p.y, 40)/dims.y, 1)
  }

}
