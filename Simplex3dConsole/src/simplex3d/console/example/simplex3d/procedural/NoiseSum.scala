package simplex3d.console.example.simplex3d.procedural

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.console.extension.ImageUtils._


object NoiseSum extends Application {

  val size = ConstVec2i(800, 600)

  def octave(i: Int, p: inVec2) = {
    val s = pow(2, i)
    noise1(p*s)/s
  }

  genImage("Noise Sum", size){ pix =>
    val p = pix/200
    val n = {
      octave(0, p) +
      octave(1, p) +
      octave(2, p)
    }
    Vec3((n + 1)/2)
  }

}
