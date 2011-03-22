package simplex3d.console.example.simplex3d.procedural

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.console.extension.ImageUtils._


object VaryingFrequency extends Application {

  val size = ConstVec2i(800, 600)
  
  val octaves = 5
  val amplitude = 2
  val halfScale = 1.5

  def noiseSum(p: inVec2) = {
    val varying = amplitude + noise1(p)*0.01

    def octave(i: Int, p: inVec2) = {
      val s1 = pow(varying, i)
      val s2 = pow(amplitude, -i)
      noise1(p*s1)*s2
    }

    var sum = 0.0; var i = 0; while (i < octaves) {
      sum += octave(i, p + i)
      i += 1
    }
    sum
  }

  genImage("Varying Frequency", size){ pix =>
    val p = (size/2 + pix)/80
    Vec3((noiseSum(p) + halfScale)/(2*halfScale))
  }

}
