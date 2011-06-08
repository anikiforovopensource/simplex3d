package example.simplex3d.functions

import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.console.extension.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object Pulse extends App {

  def pulse(edge0: Double, edge1: Double, x: Double): Double = {
    if (!(edge0 <= edge1)) scala.Double.NaN
    else if (x >= edge1) 0
    else if (x <= edge0) 0
    else {
      val t = (2*x - (edge0 + edge1))/(edge1 - edge0)
      t*(t*t*(t*t*((-2048/432.0)*t*t + (6144/432.0)) + (-6144/432.0)) + (2048/432.0))
    }
  }

  
  val lineWidth = 2.5
  val axisWidth = 1.5
  val white = ConstVec3(1)
  val background = white
  val axisColor = ConstVec3(0)

  drawFunction("Plot") { (dims, pixel) =>
    val mid = dims/2.0
    val p = pixel - mid

    val color: Vec3 = background

    color *= {
      val shade = clamp(abs(p.x)/axisWidth, 0, 1)
      mix(axisColor, background, shade)
    }
    color *= {
      val shade = clamp(abs(p.y)/axisWidth, 0, 1)
      mix(axisColor, background, shade)
    }

    color *= {
      val scale = 2/mid.x

      val x = p.x*scale
      val y = p.y*scale

      val f = pulse(-1, 1, x)
      val shade = clamp(abs(f - y)/(scale*lineWidth), 0, 1)
      mix(Vec3(1, 0, 0), Vec3(1), shade)
    }

    color
  }
}
