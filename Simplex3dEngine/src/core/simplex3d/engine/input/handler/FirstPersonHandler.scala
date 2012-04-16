/*
 * Simplex3dEngine - Core Module
 * Copyright (C) 2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dEngine.
 *
 * Simplex3dEngine is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dEngine is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.engine
package input
package handler

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.engine.input._
import simplex3d.engine.transformation._


class FirstPersonHandler(
  val transformation: ComponentTransformation3d,
  val motionSpeed: Double = 20.0,
  val mouseRotationSpeed: Double = 0.3,
  val keyboardRotationSpeed: Double = 100
) extends InputListener {
  
  
  override def update(input: Input, time: TimeStamp) {
    val keyDown = input.keyboard.isKeyDown(_); import KeyCode._
    
    val rotated = transformation.rotation.rotateVector(Vec3.UnitZ)
    val xzPlaneVec = normalize(rotated.xz)

    val px = clamp(xzPlaneVec.y, -1, 1)
    val py = clamp(dot(Vec3(xzPlaneVec.x, 0, xzPlaneVec.y), rotated), -1, 1)
    
    var horizontalAngle = sign(rotated.x)*degrees(acos(px))
    var verticalAngle = -sign(rotated.y)*degrees(acos(py))
    
    if (hasErrors(horizontalAngle)) horizontalAngle = 0
    if (hasErrors(verticalAngle)) verticalAngle = 0
    
    
    horizontalAngle -= input.mouse.delta.x*mouseRotationSpeed
    verticalAngle += input.mouse.delta.y*mouseRotationSpeed
    
    if (keyDown(K_Left)) horizontalAngle += time.interval*keyboardRotationSpeed
    if (keyDown(K_Right)) horizontalAngle -= time.interval*keyboardRotationSpeed
    if (keyDown(K_Up)) verticalAngle += time.interval*keyboardRotationSpeed
    if (keyDown(K_Down)) verticalAngle -= time.interval*keyboardRotationSpeed
    
    verticalAngle = clamp(verticalAngle, -89, 89)
    
    val mutable = transformation.update
    mutable.rotation := Quat4 rotateX(radians(verticalAngle)) rotateY(radians(horizontalAngle))
    
    
    val direction = rotateVector(Vec3(0, 0, -1), mutable.rotation)
    val left = rotateVector(Vec3(-1, 0, 0), mutable.rotation)
    
    val position = transformation.update.translation
    if (keyDown(K_w)) position += direction*time.interval*motionSpeed
    if (keyDown(K_s)) position -= direction*time.interval*motionSpeed
    if (keyDown(K_a)) position += left*time.interval*motionSpeed
    if (keyDown(K_d)) position -= left*time.interval*motionSpeed
  }
}
