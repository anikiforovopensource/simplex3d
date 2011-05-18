package example.simplex3d.procedural.animation

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.doublex.functions._
import simplex3d.console.extension.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object Noise1D extends App {

  val lineWidth = 2.5
  val axisWidth = 1.5
  val white = ConstVec3(1)
  val background = white
  val axisColor = ConstVec3(0)

  animateFunction("Noise1D", ConstVec2i(800, 300)) { (dims, time, pixel) =>
    val mid = dims/2.0
    val u = pixel - mid

    //val color = out
    //color := background
    val color: Vec3 = background

    color *= {
      val shade = clamp(abs(u.x)/axisWidth, 0, 1)
      mix(axisColor, background, shade)
    }
    color *= {
      val shade = clamp(abs(u.y)/axisWidth, 0, 1)
      mix(axisColor, background, shade)
    }

    color *= {
      val scale = 2/mid.y

      val x = u.x*scale
      val y = u.y*scale

      val f = noise1(x + time)
      val shade = clamp(abs(f - y)/(scale*lineWidth), 0, 1)
      mix(Vec3(1, 0, 0), Vec3(1), shade)
    }

    color
  }
}
