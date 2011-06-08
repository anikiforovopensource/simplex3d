package example.simplex3d.lines

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.console.extension.ImageUtils._


/**
 * @author Aleksey Nikiforov (lex)
 */
object CoordinateSpacesShort extends App {

{
  // Define a box so we have something to render.
  val s = 8.0
  val boxTranslation = Mat3x4 translate(Vec3(-s/2))
  val box = List(
    // Front.
    Vec3(0, 0, 0), Vec3(s, 0, 0),
    Vec3(s, 0, 0), Vec3(s, s, 0),
    Vec3(s, s, 0), Vec3(0, s, 0),
    Vec3(0, s, 0), Vec3(0, 0, 0),
    // Back.
    Vec3(0, 0, s), Vec3(s, 0, s),
    Vec3(s, 0, s), Vec3(s, s, s),
    Vec3(s, s, s), Vec3(0, s, s),
    Vec3(0, s, s), Vec3(0, 0, s),
    // Sides.
    Vec3(0, 0, 0), Vec3(0, 0, s),
    Vec3(s, 0, 0), Vec3(s, 0, s),
    Vec3(s, s, 0), Vec3(s, s, s),
    Vec3(0, s, 0), Vec3(0, s, s)
  ).map(p => boxTranslation.transformPoint(p))
  
  val lines = DataArray[Vec3, RFloat](box: _*)
  val linesOut = DataArray[Vec2, RFloat](lines.size)
  
  val colors = DataArray[Vec3, UByte](lines.size)
  for (i <- 0 until colors.size/2) { colors(i*2) = Vec3(1, 0, 1); colors(i*2 + 1) = Vec3(0, 0, 1) }


  // All 3D transformations happen here.
  def transform(modelMat: inMat3x4, cameraMat: inMat3x4, projectionMat: inMat4, dims: inVec2i) {
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


  animateLines("Box", Vec3(0)) { (dims, time) =>
    val camRotation = Quat4 rotateX(radians(-20)) rotateY(radians(30)*time)
    val camTranslation = camRotation.rotateVector(Vec3(0, 0, 20))

    val cameraMat = Mat3x4 rotate(camRotation) translate(camTranslation)
    val modelMat = Mat3x4 Identity
    val projectionMat = perspectiveProj(radians(45), dims.x.toDouble/dims.y, 10, 100)

    transform(modelMat, cameraMat, projectionMat, dims)

    (linesOut, colors, linesOut.size)
  }
}

}
