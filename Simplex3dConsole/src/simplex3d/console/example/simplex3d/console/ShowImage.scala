package simplex3d.console.example.simplex3d.console

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.console.extension.ImageUtils._


object ShowImage extends Application {

  {
    val size = Vec2i(600, 400)
    val data = DataArray[Vec3, UByte](size.x*size.y)

    for (y <- 0 until size.y; x <- 0 until size.x) {
        data(y*size.x + x) = Vec3(x.toDouble/size.x, y.toDouble/size.y, 1)
    }

    showImage(size, data)
  }

}
