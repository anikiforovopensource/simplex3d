package example.simplex3d.console

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.console.extension.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object AnimateFunction extends Application {

  animateFunction { (dims, time, pixel) =>
    val f = 2*fract(time*0.1)
    val g = min(f, 2 - f)
    Vec3(pixel/dims, g)
  }

}
