package simplex3d.console.example.simplex3d.procedural

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.console.extension.ImageUtils._


object Turbulence extends Application {

  val size = ConstVec2i(800, 600)

  val octaves = 4
  val lacunarity = 2
  val amplitude = 2

  def noiseSum(p: inVec2) = {
    def octave(i: Int, p: inVec2) = {
      val s1 = pow(lacunarity, i)
      val s2 = pow(amplitude, -i)
      abs(noise1(p*s1)*s2)
    }

    var sum = 0.0; var i = 0; while (i < octaves) {
      sum += octave(i, p + i)
      i += 1
    }
    sum
  }

  genImage("Noise Sum Parameters", size){ pix =>
    val p = pix/200
    Vec3(noiseSum(p)*0.8)
  }

}
