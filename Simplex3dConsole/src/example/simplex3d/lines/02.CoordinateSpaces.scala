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
object CoordinateSpaces extends App {

{
  // Define axis so we have something to render.
  val axisLength = 4.0
  val axis = List(
    Vec3(0, 0, 0), Vec3(axisLength, 0, 0),
    Vec3(0, 0, 0), Vec3(0, axisLength, 0),
    Vec3(0, 0, 0), Vec3(0, 0, axisLength)
  )

  // Define axis labels.
  val h = axisLength*0.1 // Letter height.
  val x = Vec3(axisLength + h, h, h) // X-label offset
  val y = Vec3(h, axisLength + h, h) // Y-label offset
  val z = Vec3(h, h, axisLength + h) // Z-label offset
  val labels = List(
    // X.
    x + Vec3(-h, h, 0), x + Vec3(h, -h, 0),
    x + Vec3(h, h, 0), x + Vec3(-h, -h, 0),
    // Y.
    y + Vec3(-h, h, 0), y + Vec3(0),
    y + Vec3(h, h, 0), y + Vec3(0),
    y + Vec3(0), y + Vec3(0, -h, 0),
    // Z.
    z + Vec3(-h, h, 0), z + Vec3(h, h, 0),
    z + Vec3(h, h, 0), z + Vec3(-h, -h, 0),
    z + Vec3(-h, -h, 0), z + Vec3(h, -h, 0)
  )
  
  val lines = DataArray[Vec3, RFloat]((axis ::: labels): _*)
  val linesOut = DataArray[Vec2, RFloat](lines.size)
  
  val colors = DataArray[Vec3, UByte](lines.size)
  for (i <- 0 until colors.size/2) { colors(i*2) = Vec3(0, 1, 0); colors(i*2 + 1) = Vec3(0, 0, 1) }


  /* Lines are transformed to window coordinates as follows:
   *
   * - Find the CameraMatrix that positions camera in world coordinates.
   *   Invert it to find the ViewMatrix which transforms world
   *   coordinates to view coordinates.
   *   
   * - Find the ModelMatrix, which positions the model in world coordinates.
   *   This matrix will transform model coordinates to world coordinates.
   * 
   * - Mutliply the ViewMatrix by the ModelMatrix to produce ModelViewMatrix,
   *   which transforms ModelCoordinates to view coordinates.
   *   
   * - Find the ProjectionMatrix, which transforms view coordinates to
   *   ClipCoordinates. ProjectionMatrix can be set as perspective projection
   *   or ortho projection.
   *   
   * - Multiply the ProjectionMatrix by the ModelViewMatrix to produce
   *   ModelViewProjectionMatrix. This matrix transforms ModelCoordinates to
   *   ClipCoordinates.
   *   
   * - Extend ModelCoordinates to HomogeneousCoordinates by taking
   *   x, y, and z and setting w to 1.
   *   
   * - Multiply the ModelViewProjectionMatrix by the HomogeneousCoordinates
   *   to produce ClipCoordinates.
   *   
   * - ClipCoordinates with XYZ components outside the range (-W, W) lie outside
   *   the view frustrum and should be clipped.
   *   
   * - Perform PerspectiveDivision on ClippedCoordinates to produce
   *   NormalizedDeviceCoordinates. NormalizedDeviceCoordinates are within
   *   a cube where each component is in range (-1, 1).
   *   
   * - Scale and translate the NormalizedDeviceCoordinates according to
   *   the Viewport parameters to produce the WindowCoordinates.
   */
  animateLines("Axis", Vec3(0)) { (dims, time) =>
    val camRotationSpeed = radians(30) // 30 degrees per second
    val camRotation = Quat4 rotateX(radians(-20)) rotateY(camRotationSpeed*time)
    val camTranslation = camRotation.rotateVector(Vec3(0, 0, 20))

    val cameraMatrix = Mat3x4 rotate(camRotation) translate(camTranslation)
    val viewMatrix = inverse(cameraMatrix)

    val modelMatrix = Mat3x4 Identity
    val modelViewMatrix = modelMatrix.concat(viewMatrix)
        
    val projectionMatrix = perspectiveProj(
      radians(45), // view angle
      dims.x.toDouble/dims.y, // aspect ratio
      10, 100 // near and far planes
    )

    val modelViewProjectionMatrix = projectionMatrix*Mat4(modelViewMatrix)
        
    val viewportScale = dims*0.5
    val viewportOffset = dims*0.5
          
    var i = 0; while (i < lines.size/2) {
      val j = i*2
      
      val modelCoordinates0 = lines(j)
      val modelCoordinates1 = lines(j + 1)

      val homogeneousCoordinates0 = Vec4(modelCoordinates0, 1)
      val homogeneousCoordinates1 = Vec4(modelCoordinates1, 1)
      
      val clipCoordinates0 = modelViewProjectionMatrix*homogeneousCoordinates0
      val clipCoordinates1 = modelViewProjectionMatrix*homogeneousCoordinates1

      // For simplicity clipping is omited.

      val normalizedDeviceCoordinates0 = clipCoordinates0.xyz/clipCoordinates0.w
      val normalizedDeviceCoordinates1 = clipCoordinates1.xyz/clipCoordinates1.w

      val windowCoordinates0 = normalizedDeviceCoordinates0.xy*viewportScale + viewportOffset
      val windowCoordinates1 = normalizedDeviceCoordinates1.xy*viewportScale + viewportOffset
      
      linesOut(j) = windowCoordinates0
      linesOut(j + 1) = windowCoordinates1
      
      i += 1
    }

    (linesOut, colors, linesOut.size)
  }
}

}
