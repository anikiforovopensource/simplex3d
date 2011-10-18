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
package handler

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.engine.input._
import simplex3d.engine.transformation._


class FirstPersonHandler(
  val transformation: ComponentTransformation3d,
  val motionSpeed: Double = 20.0,
  val rotationSpeed: Double = 0.3
) extends InputListener {
  
  var horizontalAngle = 0.0
  var verticalAngle = 0.0
  val position = Vec3(0)
  

  override def update(input: Input, time: TimeStamp) {
    val keyDown = input.keyboard.isKeyDown(_); import KeyCode._
    
    val rotation = Mat3x4 rotateX(radians(verticalAngle)) rotateY(radians(horizontalAngle))
    val direction = rotation.transformVector(Vec3(0, 0, -1))
    val left = rotation.transformVector(Vec3(-1, 0, 0))
    
    if (keyDown(K_w)) position += direction*time.interval*motionSpeed
    if (keyDown(K_s)) position -= direction*time.interval*motionSpeed
    if (keyDown(K_a)) position += left*time.interval*motionSpeed
    if (keyDown(K_d)) position -= left*time.interval*motionSpeed
  
    horizontalAngle -= input.mouse.delta.x*rotationSpeed
    verticalAngle += input.mouse.delta.y*rotationSpeed
    verticalAngle = clamp(verticalAngle, -89, 89)
    
    val mutable = transformation.mutable
    mutable.rotation := Quat4 rotateX(radians(verticalAngle)) rotateY(radians(horizontalAngle))
    mutable.translation := position
  }
}
