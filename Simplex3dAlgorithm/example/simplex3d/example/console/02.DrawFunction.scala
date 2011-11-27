package example.simplex3d.console

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.script.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object DrawFunction extends App {

  drawFunction { (dims, p) =>
    Vec3(p/dims, 1)
  }

}
