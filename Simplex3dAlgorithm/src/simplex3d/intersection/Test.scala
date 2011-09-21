/*
 * Simplex3d, Intersection module
 * Copyright (C) 2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dAlgorithm.
 *
 * Simplex3dAlgorithm is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dAlgorithm is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.intersection

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._


/** 
 *
 * TODO Use Separating Axis Test for collision detection.
 * 
 * @author Aleksey Nikiforov (lex)
 */
object Test {
  def main(args: Array[String]): Unit = {
    testAabr
  }
  
  def testAabr() {
    val res = Aabr.intersectAabr(Vec2(0), Vec2(2, 10))(Vec2(0), Vec2(2, 10))
    println(res)
  }
  
  def testRay() {
    val ray = Ray(Vec3(-1, 1, 0), Vec3(1, 0, 1)*100)
    val res = ray.intersectObb(Vec3(0), Vec3(10), Mat3x4 scale 2 translate Vec3(1, 0, 0))
    
    if (res.length == 0) println("no intersection")
    else println(ray.point(res(0)) + " " + ray.point(res(1)))
  }
}
