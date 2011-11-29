package simplex3d.example.engine

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.algorithm.noise._
import simplex3d.engine._
import simplex3d.script._


object NoiseTest extends FunctionRendererApp {
  val title = "Noise Test"
  
  override lazy val settings = new Settings(
    verticalSync = true,
    performanceLog = true,
    resolution = Some(Vec2i(800, 600))
  )
  
  
  val noise = new Noise(ClassicalGradientNoise)
    
  val zoom = 1.0/50
  val changeSpeed = 1.0/3
  val scrollSpeed = 10
  
  animateFunction { (dims, time, pixel) =>
    val p = pixel + time*scrollSpeed
    Vec3((noise(Vec3(p*zoom, time*changeSpeed)) + 1)/2)
  }
}
