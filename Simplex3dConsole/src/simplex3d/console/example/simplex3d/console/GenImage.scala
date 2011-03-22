package simplex3d.console.example.simplex3d.console

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.console.extension.ImageUtils._


object GenImage extends Application {

  val size = ConstVec2i(600, 400)

  genImage(size){ p =>
    Vec3(p/size, 1)
  }

}
