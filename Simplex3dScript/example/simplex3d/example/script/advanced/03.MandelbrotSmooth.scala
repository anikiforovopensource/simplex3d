package example.simplex3d.procedural.animation

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.script.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object MandelbrotSmooth extends App {

  val startScale: Double = 200
  val zoomSpeed: Double = 1.1

  //val zoomPoint = Vec2(-0.743643887037158704752191506114774, 0.131825904205311970493132056385139)
  val zoomPoint = Vec2(0.001643721971153, -0.822467633298876)

  val colors = {
    val bands = new ColorBands()
    bands.start = Vec3(0, 0, 1)
    bands put new Gradient(Vec3(0.9), 10)
    bands put new Gradient(Vec3(0, 0, 1), 10)
    bands put new Shade(Vec3(0.1), 0.7, 20)
    bands put new Gradient(Vec3(1, 0.55, 0), 10)
    bands put new Shade(Vec3(0.1), 0.7, 20)
    bands put new Gradient(Vec3(0, 0, 1), 10)
    bands put new Shade(Vec3(0.1), 0.7, 20)
    bands put new Gradient(Vec3(0.58, 0, 0.827), 50)
    bands put new Gradient(Vec3(0.2, 0, 0), 100)
    bands.generate()
  }

  val iterations = colors.length
  val escapeColor = colors(iterations - 1)
  val extraIterations = 3

  animateFunction("Mandelbrot") { (dims, time, pixel) =>
    val mid = dims/2.0
    val zoom = startScale + pow(zoomSpeed, 30 + time)
    val c = (pixel - mid)/zoom + zoomPoint

    val z = Vec2(0)
    import z.{x, y}

    var i = 0; while (x*x + y*y <= 4 && i < iterations) {
      val xt = x*x - y*y + c.x

      y = 2*x*y + c.y
      x = xt

      i += 1
    }

    val extra = i + extraIterations
    while (i < extra) {
      val xt = x*x - y*y + c.x

      y = 2*x*y + c.y
      x = xt

      i += 1
    }

    var grad = i - log2(abs(log2(length(z))))
    grad = clamp(grad, 0, colors.length - 2)
    val index = grad.toInt
    mix(colors(index), colors(index + 1), fract(grad))
  }

  
  class ColorBands {
    import scala.collection.mutable.ListBuffer

    var start = Vec3(1)
    private val generators = ListBuffer[ColorGen]()

    def put(g: ColorGen) { generators += g }

    def generate() :Array[ConstVec3] = {
      val bands = ListBuffer[ConstVec3]()
      var from = start

      for (gen <- generators) {
        bands appendAll gen.generate(from)
        from = gen.to
      }

      bands.toArray
    }
  }

  abstract class ColorGen {
    def to: Vec3
    def generate(from: Vec3) :List[ConstVec3]

    final def gradient(start: Vec3, end: Vec3, count: Int) = {
      (for (i <- 0 until count) yield {
        mix(start, end, i.toDouble/count).toConst
      }).toList
    }
  }

  class Gradient(val to: Vec3, val count: Int) extends ColorGen {
    def generate(from: Vec3) = gradient(from, to, count)
  }

  class Shade(val shade: Vec3, val contrast: Double, val count: Int)
  extends ColorGen
  {
    val to = Vec3(0)
    def generate(from: Vec3) = {
      to := from

      val clamped = clamp(contrast, 0, 1)

      val pre = (round(count*clamped)).toInt
      val post = count - pre

      gradient(from, shade, pre) :::
      gradient(mix(shade, from, clamped), from, post)
    }
  }
}
