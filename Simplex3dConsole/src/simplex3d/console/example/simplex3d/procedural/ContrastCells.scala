package simplex3d.console.example.simplex3d.procedural

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.console.extension.ImageUtils._


object ContrastCells extends Application {

  val size = ConstVec2i(600, 400)

  def cell(x: Double, size: Double) = {
    val s = x/size + 1e-6
    (ceil(s) - fract(s))*size
  }

  genImage("Contrast Cells", size){ p =>
    Vec3(cell(p.x, 40)/size.x, cell(p.y, 40)/size.y, 1)
  }

}
