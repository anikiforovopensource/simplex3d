/*
 * Simplex3d, MathTest package
 * Copyright (C) 2010 Simplex3d Team
 *
 * This file is part of Simplex3dMathTest.
 *
 * Simplex3dMathTest is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dMathTest is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package visual.math

import visual.math.draw._

import simplex3d.math._
import simplex3d.math.doublem.renamed._
import simplex3d.math.doublem.DoubleMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
object NoiseContribution {

  def main(args: Array[String]) {
    FunFrame.launch(new Fun {
    final def apply(pixel: AnyVec2, time: Double)
    :AnyVec3 =
    {
      val lineWidth = 2.5
      val axisWidth = 1.5
      val white = Vec3(1)
      val background = Vec3(1)
      val axisColor = Vec3(0)

      val mid = dimensions/2
      val u = pixel - mid

      val color = background

      color *= {
        val shade = clamp(abs(u.x)/axisWidth, 0, 1)
        mix(axisColor, white, shade)
      }
      color *= {
        val shade = clamp(abs(u.y)/axisWidth, 0, 1)
        mix(axisColor, white, shade)
      }

      val z = 0.06*time
      val w = 0.04*time

      color *= {
        val scale = 2/mid.y

        val x = u.x*scale
        val y = u.y*scale

        val timeSlot = (int(time)/5)%9
        val shade = 
          if (timeSlot == 0) nc1(x)
          else if (timeSlot == 1) nc2(x, y)
          else if (timeSlot < 5) nc3(x, y, z)
          else nc4(x, y, z, w)

        if (shade <= 0.7071067811865475244) { // (shade <= sqrt(0.5)
          mix(Vec3(0, 0, 0), Vec3(1, 0, 0), shade)
        }
        else {
          mix(Vec3(0, 0, 0), Vec3(1), shade)
        }

//        // 1d and 2d overlayed
//        val shade1 = nc1(x)
//        val shade2 = nc2(x, y)
//        val c1 = if (shade1 <= 0.7071067811865475244) {
//          mix(Vec3(0, 0, 0), Vec3(1, 0, 0), shade1)
//        }
//        else {
//          mix(Vec3(0, 0, 0), Vec3(1), shade1)
//        }
//        val c2 = if (shade2 <= 0.7071067811865475244) {
//          mix(Vec3(0, 0, 0), Vec3(0, 0, 1), shade2)
//        }
//        else {
//          mix(Vec3(0, 0, 0), Vec3(1), shade2)
//        }
//        c1 + c2

      }
      
      color
    }})
  }

  final def ifloor(x: Double) :Int = {
    val i = int(x)
    if (x > 0 || x == i) i else i - 1
  }

  final val F1 = 1.4142135623730950488 //Math.sqrt(2.0)
  final val G1 = 0.7071067811865475244 //(1/Math.sqrt(2.0))

  final def nc1(x: Double) :Double = {
    val pix = ifloor(x*F1)

    val p0x = x - pix*G1

    length(p0x)
  }

  final val F2 = 0.36602540378443864676 //(Math.sqrt(3.0) - 1.0) / 2.0
  final val G2 = 0.21132486540518711775 //(3.0 - Math.sqrt(3.0)) / 6.0
  final val G22 = 0.57735026918962576451 //1 - 2.0 * (3.0 - Math.sqrt(3.0)) / 6.0

  final def nc2(x: Double, y: Double) :Double = {
    // Skew the (x,y) space to determine which cell of 2 simplices we're in
    val s = (x + y) * F2 // Hairy factor for 2D skewing
    val pix = ifloor(x + s)
    val piy = ifloor(y + s)
    val t = (pix + piy) * G2 // Hairy factor for unskewing

    // The x,y distances from the cell origin
    val p0x = x - pix + t
    val p0y = y - piy + t

    val v = Vec2(p0x, p0y)
    length(v)
  }

  final val F3 = 1 / 3.0
  final val G3 = 1 / 6.0
  final val G32 = 2 / 6.0
  final val G33 = 1 - 3 / 6.0

  final def nc3(x: Double, y: Double, z:Double) = {
    // Skew the (x,y,z) space to determine which cell of 6 simplices we're in
    val s = (x + y + z) * F3 // Factor for 3D skewing
    val pix = ifloor(x + s)
    val piy = ifloor(y + s)
    val piz = ifloor(z + s)
    val t = (pix + piy + piz) * G3

    // The x,y,z distances from the cell origin
    val p0x = x - pix + t
    val p0y = y - piy + t
    val p0z = z - piz + t

    val v = Vec3(p0x, p0y, p0z)
    length(v)
  }

  final val F4 = 0.3090169943749474241 //(Math.sqrt(5.0) - 1.0) / 4.0
  final val G4 = 0.13819660112501051518 //(5.0 - Math.sqrt(5.0)) / 20.0
  final val G42 = 0.27639320225002103036 //2.0 * ((5.0 - Math.sqrt(5.0)) / 20.0)
  final val G43 = 0.41458980337503154554 //3.0 * ((5.0 - Math.sqrt(5.0)) / 20.0)
  final val G44 = 0.44721359549995793928 //1 - 4.0 * ((5.0 - Math.sqrt(5.0)) / 20.0)

  final def nc4(x: Double, y: Double, z:Double, w:Double) :Double = {
    // Skew the (x,y,z,w) space to determine which cell of 24 simplices we're in
    val s = (x + y + z + w) * F4 // Factor for 4D skewing
    val pix = ifloor(x + s)
    val piy = ifloor(y + s)
    val piz = ifloor(z + s)
    val piw = ifloor(w + s)
    val t = (pix + piy + piz + piw) * G4

    // The x,y,z,w distances from the cell origin
    val p0x = x - pix + t
    val p0y = y - piy + t
    val p0z = z - piz + t
    val p0w = w - piw + t

    val v = Vec4(p0x, p0y, p0z, p0w)
    length(v)
  }
}
