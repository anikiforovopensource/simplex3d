/*
 * Simplex3dAlgorithm - Intersection Module
 * Copyright (C) 2010-2011, Aleksey Nikiforov
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

package simplex3d.algorithm.intersection

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import Collision._

/**
 * 
 * TODO add specialized intersection tests that return and reuse information about "safe" planes.
 * @author Aleksey Nikiforov (lex)
 */
class Frustum protected (
  final val leftNormal: ConstVec3, final val leftCoefficient: Double,
  final val rightNormal: ConstVec3, final val rightCoefficient: Double,
  final val bottomNormal: ConstVec3, final val bottomCoefficient: Double,
  final val topNormal: ConstVec3, final val topCoefficient: Double,
  final val nearNormal: ConstVec3, final val nearCoefficient: Double,
  final val farNormal: ConstVec3, final val farCoefficient: Double
) {
  
  final def intersectPoint(point: inVec3) :Int = {
    if (dot(point, leftNormal) < -leftCoefficient) Outside
    else if (dot(point, rightNormal) < -rightCoefficient) Outside
    else if (dot(point, bottomNormal) < -bottomCoefficient) Outside
    else if (dot(point, topNormal) < -topCoefficient) Outside
    else if (dot(point, nearNormal) < -nearCoefficient) Outside
    else if (dot(point, farNormal) < -farCoefficient) Outside
    else Inside
  }
  
  final def intersectSphere(translation: inVec3, radius: Double) :Int = {
    var res = Inside

    {
      val test = dot(translation, leftNormal) + leftCoefficient
      if (test <= -radius) return Outside else if (test < radius) res = Intersecting
    }
    {
      val test = dot(translation, rightNormal) + rightCoefficient
      if (test <= -radius) return Outside else if (test < radius) res = Intersecting
    }
    {
      val test = dot(translation, bottomNormal) + bottomCoefficient
      if (test <= -radius) return Outside else if (test < radius) res = Intersecting
    }
    {
      val test = dot(translation, topNormal) + topCoefficient
      if (test <= -radius) return Outside else if (test < radius) res = Intersecting
    }
    {
      val test = dot(translation, nearNormal) + nearCoefficient
      if (test <= -radius) return Outside else if (test < radius) res = Intersecting
    }
    {
      val test = dot(translation, farNormal) + farCoefficient
      if (test <= -radius) return Outside else if (test < radius) res = Intersecting
    }

    res
  }

  final def intersectAabb(min: inVec3, max: inVec3) :Int = {
    
    def testPlane(planeNormal: inVec3, planeCoef: Double, res: Int) :Int = {
      val pSelector = greaterThanEqual(planeNormal, Vec3.Zero)
      val pVertex = mix(min, max, pSelector)

      val pTest = dot(pVertex, planeNormal)
      if (pTest <= -planeCoef) Outside
      else {
        val nVertex = mix(min, max, not(pSelector))

        val nTest = dot(nVertex, planeNormal)
        if (nTest < -planeCoef) Intersecting else res
      }
    }
    
    var res = Inside
    res = testPlane(leftNormal, leftCoefficient, res); if (res != Outside) {
      res = testPlane(rightNormal, rightCoefficient, res); if (res != Outside) {
        res = testPlane(bottomNormal, bottomCoefficient, res); if (res != Outside) {
          res = testPlane(topNormal, topCoefficient, res); if (res != Outside) {
            res = testPlane(nearNormal, nearCoefficient, res); if (res != Outside) {
              res = testPlane(farNormal, farCoefficient, res)
            }
          }
        }
      }
    }

    res
  }

  
  final def intersectObb(min: inVec3, max: inVec3, worldTranformation: inMat4x3) :Int = {
    val m = Mat3(worldTranformation)
    val c3 = worldTranformation(3)

    def testPlane(frustumNormal: inVec3, frustumCoef: Double, res: Int): Int = {

      // Transform the plane equation as follows:
      // val planeEq = Vec4(normal(i), coefficient(i))
      // val transformed = planeEq*Mat4(modelMatrix)
      // val normalizedPlane = normalizePlane(transformed)
      // The code below does the same thing, but more efficiently.

      val planeNormal = frustumNormal*m
      val invLen = inversesqrt(dot(planeNormal, planeNormal))
      planeNormal *= invLen
      val planeCoef = (dot(frustumNormal, c3) + frustumCoef)*invLen

      // Continue like we have AABB.
      val pSelector = greaterThanEqual(planeNormal, Vec3.Zero)
      val pVertex = mix(min, max, pSelector)

      val pTest = dot(pVertex, planeNormal)
      if (pTest <= -planeCoef) Outside
      else {
        val nVertex = mix(min, max, not(pSelector))

        val nTest = dot(nVertex, planeNormal)
        if (nTest < -planeCoef) Intersecting else res
      }
    }
    
    var res = Inside
    res = testPlane(leftNormal, leftCoefficient, res); if (res != Outside) {
      res = testPlane(rightNormal, rightCoefficient, res); if (res != Outside) {
        res = testPlane(bottomNormal, bottomCoefficient, res); if (res != Outside) {
          res = testPlane(topNormal, topCoefficient, res); if (res != Outside) {
            res = testPlane(nearNormal, nearCoefficient, res); if (res != Outside) {
              res = testPlane(farNormal, farCoefficient, res)
            }
          }
        }
      }
    }

    res
  }

  override def toString() = {
    "Frustum(\n" +
    "  " + leftNormal + ", " + leftCoefficient + ",\n" +
    "  " + rightNormal + ", " + rightCoefficient + ",\n" +
    "  " + bottomNormal + ", " + bottomCoefficient + ",\n" +
    "  " + topNormal + ", " + topCoefficient + ",\n" +
    "  " + nearNormal + ", " + nearCoefficient + ",\n" +
    "  " + farNormal + ", " + farCoefficient + "\n" +
    ")"
  }
}

object Frustum {
  def apply(
    leftNormal: ConstVec3, leftCoefficient: Double,
    rightNormal: ConstVec3, rightCoefficient: Double,
    bottomNormal: ConstVec3, bottomCoefficient: Double,
    topNormal: ConstVec3, topCoefficient: Double,
    nearNormal: ConstVec3, nearCoefficient: Double,
    farNormal: ConstVec3, farCoefficient: Double
  ) = new Frustum(
    leftNormal, leftCoefficient,
    rightNormal, rightCoefficient,
    bottomNormal, bottomCoefficient,
    topNormal, topCoefficient,
    nearNormal, nearCoefficient,
    farNormal, farCoefficient
  )

  def apply(viewProjection: inMat4) :Frustum = {
    val rows = transpose(viewProjection)
    val rx = rows(0)
    val ry = rows(1)
    val rz = rows(2)
    val rw = rows(3)

    def normalizePlane(nd: inVec4) :Vec4 = {
      val len = length(nd.xyz)
      nd/len
    }

    val left = normalizePlane(rw + rx)
    val right = normalizePlane(rw - rx)
    val bottom = normalizePlane(rw + ry)
    val top = normalizePlane(rw - ry)
    val near = normalizePlane(rw + rz)
    val far = normalizePlane(rw - rz)

    new Frustum(
      left.xyz, left.w,
      right.xyz, right.w,
      bottom.xyz, bottom.w,
      top.xyz, top.w,
      near.xyz, near.w,
      far.xyz, far.w
    )
  }
}
