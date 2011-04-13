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
object Noise2D extends Application {

  drawFunction("Noise2D") { (dims, p) =>
    val n = noise1(p/100)
    Vec3((n + 1)/2)
  }

}
