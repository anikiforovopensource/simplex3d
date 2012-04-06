package simplex3d.example.script.advanced

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.script.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object Simplex extends App {

{
  // Simplex.
  val side = 8.0
  val height2d = sqrt(3.0/4.0*side*side)
  val radius2d = side/sqrt(3.0)
  val centerz = sqrt(radius2d*radius2d - 1.0/4.0*side*side)
  val height3d = side*sqrt(6.0)/3.0
  val radius3d = side*sqrt(3.0/8.0)
  val centery = sqrt(radius3d*radius3d - radius2d*radius2d)

  val simplexTranslation = Mat4x3 translate(-Vec3(side/2, centery, centerz))

  val simplex = List(
    // Base.
    Vec3(0, 0, 0), Vec3(side, 0, 0),
    Vec3(side, 0, 0), Vec3(side/2, 0, height2d),
    Vec3(side/2, 0, height2d), Vec3(0, 0, 0),
    // The remaining 3 edges.
    Vec3(0, 0, 0), Vec3(side/2, height3d, centerz),
    Vec3(side, 0, 0), Vec3(side/2, height3d, centerz),
    Vec3(side/2, 0, height2d), Vec3(side/2, height3d, centerz)
  ).map(p => simplexTranslation.transformPoint(p))


  val lines = DataArray[Vec3, RFloat](simplex: _*)
  val linesOut = DataArray[Vec2, RFloat](lines.size)
  
  val colors = DataArray[Vec3, UByte](lines.size)
  for (i <- 0 until colors.size/2) { colors(i*2) = Vec3(1, 0, 0); colors(i*2 + 1) = Vec3(0, 0, 1) }


  // All 3D transformations happen here.
  def transform(modelMat: inMat4x3, cameraMat: inMat4x3, projectionMat: inMat4, dims: inVec2i) {
    val modelViewProjectionMat = projectionMat*Mat4(modelMat.concat(inverse(cameraMat)))

    val viewportScale = dims*0.5
    val viewportOffset = dims*0.5

    var i = 0; while (i < lines.size - 1) {
      val c0 = modelViewProjectionMat*Vec4(lines(i), 1)
      val c1 = modelViewProjectionMat*Vec4(lines(i + 1), 1)

      val n0 = c0.xyz/c0.w
      val n1 = c1.xyz/c1.w

      linesOut(i) = n0.xy*viewportScale + viewportOffset
      linesOut(i + 1) = n1.xy*viewportScale + viewportOffset

      i += 2
    }
  }


  animateLines("Simplex", Vec3(0)) { (dims, time) =>
    val camRotation = Quat4 rotateX(radians(-20)) rotateY(radians(30)*time)
    val camTranslation = camRotation.rotateVector(Vec3(0, 0, 20))

    val cameraMat = Mat4x3 rotate(camRotation) translate(camTranslation)
    val modelMat = Mat4x3 Identity
    val projectionMat = perspectiveProj(radians(45), dims.x.toDouble/dims.y, 10, 100)

    transform(modelMat, cameraMat, projectionMat, dims)

    (linesOut, colors, linesOut.size)
  }
}

}
