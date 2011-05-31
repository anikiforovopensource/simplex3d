package example.simplex3d.lines

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.doublex.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.console.extension.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object SimpleLines extends App {

  val genLines = (for (i <- 0 until 30) yield {
    val offset = Vec2(0, -800 + i*50)

    List(
      Vec2(0) + offset,
      Vec2(1000 + offset)
    )
  }).flatten

  val lines = DataArray[Vec2, RFloat](genLines: _*)
  val colors = DataArray[Vec3, UByte](lines.size)

  for (i <- 0 until colors.size/2) { colors(i*2) = Vec3(0, 1, 0); colors(i*2 + 1) = Vec3(0, 0, 1) }


  drawLines("Lines", Vec3(0)) { (dims) =>
    (lines, colors, lines.size)
  }

}
