/*
 * Simplex3dEngine - Test Package
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

package test

import scala.collection.mutable.ArrayBuffer
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.algorithm.noise._
import simplex3d.engine._
import simplex3d.engine.graphics._
import simplex3d.engine.app._
import simplex3d.engine.bounding._
import simplex3d.engine.input._
import simplex3d.engine.input.handler._
import simplex3d.engine.scenegraph._
import simplex3d.engine.transformation._
import simplex3d.engine.impl._


object Main {
  
  def main(args: Array[String]) {
    val t1 = new ComponentTransformation3d;
    {
      val t = t1.mutable
      t.scale := 10
      t.rotation.applyRotationX(radians(30))
      t.translation := Vec3(10)
    }
    
    val t2 = new ComponentTransformation3d;
    {
      val t = t2.mutable
      t.scale := 20
      t.rotation.applyRotationY(radians(40))
      t.translation := Vec3(3)
    }
    
    val t3 = new ComponentTransformation3d
    t1.propagateChanges(t2, t3)
    
    val m1 = Mat3x4 scale 10 rotateX radians(30) translate Vec3(10)
    val m2 = Mat3x4 scale 20 rotateY radians(40) translate Vec3(3)
    val m3 = m1 concat m2
    
    println(t3.matrix - m3)
  }
}
