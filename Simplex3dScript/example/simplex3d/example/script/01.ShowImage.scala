package simplex3d.example.script

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.script.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object ShowImage extends App {

  {
    val dims = ConstVec2i(600, 400)
    val data = DataArray[Vec3, UByte](dims.x*dims.y)

    for (y <- 0 until dims.y; x <- 0 until dims.x) {
        data(y*dims.x + x) = Vec3(x.toDouble/dims.x, y.toDouble/dims.y, 1)
    }

    showImage(dims, data)
  }

}
