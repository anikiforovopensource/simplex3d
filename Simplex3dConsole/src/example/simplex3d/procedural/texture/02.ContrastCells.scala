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
object ContrastCells extends Application {

  def cell(x: Double, size: Double) = {
    val s = x/size + 1e-6
    (ceil(s) - fract(s))*size
  }

  drawFunction("Contrast Cells") { (dims, p) =>
    Vec3(cell(p.x, 40)/dims.x, cell(p.y, 40)/dims.y, 1)
  }

}
