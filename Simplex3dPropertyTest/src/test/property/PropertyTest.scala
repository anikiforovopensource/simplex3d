/*
 * Simplex3d, PropertyTest package
 * Copyright (C) 2010, Simplex3d Team
 *
 * This file is part of Simplex3dPropertyTest.
 *
 * Simplex3dPropertyTest is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dPropertyTest is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package test.property

import org.scalatest._
import simplex3d.property._
import simplex3d.math._
import simplex3d.math.intm._
import simplex3d.math.floatm._
import simplex3d.math.doublem._


/**
 * @author Aleksey Nikiforov (lex)
 */
class PropertyTest extends FunSuite {

  class SimpleProperty[@specialized(Boolean, Int, Float, Double) T](
    init: PropertyValue[T]
  ) extends Property[T] {
    protected val value = init.clone()
  }

  test("Property") {
    {
      val p = new SimpleProperty(true)
      assert(p() == true)
      p := false
      assert(p() == false)
    }
    {
      val p = new SimpleProperty(2)
      assert(p() == 2)
      p := 3
      assert(p() == 3)
    }
    {
      val p = new SimpleProperty(2f)
      assert(p() == 2f)
      p := 3f
      assert(p() == 3f)
    }
    {
      val p = new SimpleProperty(2d)
      assert(p() == 2d)
      p := 3d
      assert(p() == 3d)
    }

    {
      val p = new SimpleProperty(Vec2b(true))
      assert(p() == Vec2b(true))
      p := Vec2b(false)
      assert(p() == Vec2b(false))
    }
    {
      val p = new SimpleProperty(Vec3b(true))
      assert(p() == Vec3b(true))
      p := Vec3b(false)
      assert(p() == Vec3b(false))
    }
    {
      val p = new SimpleProperty(Vec4b(true))
      assert(p() == Vec4b(true))
      p := Vec4b(false)
      assert(p() == Vec4b(false))
    }

    {
      val p = new SimpleProperty(Vec2i(2))
      assert(p() == Vec2i(2))
      p := Vec2i(3)
      assert(p() == Vec2i(3))
    }
    {
      val p = new SimpleProperty(Vec3i(2))
      assert(p() == Vec3i(2))
      p := Vec3i(3)
      assert(p() == Vec3i(3))
    }
    {
      val p = new SimpleProperty(Vec4i(2))
      assert(p() == Vec4i(2))
      p := Vec4i(3)
      assert(p() == Vec4i(3))
    }

    {
      val p = new SimpleProperty(Vec2f(2))
      assert(p() == Vec2f(2))
      p := Vec2f(3)
      assert(p() == Vec2f(3))
    }
    {
      val p = new SimpleProperty(Vec3f(2))
      assert(p() == Vec3f(2))
      p := Vec3f(3)
      assert(p() == Vec3f(3))
    }
    {
      val p = new SimpleProperty(Vec4f(2))
      assert(p() == Vec4f(2))
      p := Vec4f(3)
      assert(p() == Vec4f(3))
    }

    {
      val p = new SimpleProperty(Vec2d(2))
      assert(p() == Vec2d(2))
      p := Vec2d(3)
      assert(p() == Vec2d(3))
    }
    {
      val p = new SimpleProperty(Vec3d(2))
      assert(p() == Vec3d(2))
      p := Vec3d(3)
      assert(p() == Vec3d(3))
    }
    {
      val p = new SimpleProperty(Vec4d(2))
      assert(p() == Vec4d(2))
      p := Vec4d(3)
      assert(p() == Vec4d(3))
    }

    {
      val p = new SimpleProperty(Quat4f(2, 2, 2, 2))
      assert(p() == Quat4f(2, 2, 2, 2))
      p := Quat4f(3, 3, 3, 3)
      assert(p() == Quat4f(3, 3, 3, 3))
    }
    {
      val p = new SimpleProperty(Quat4d(2, 2, 2, 2))
      assert(p() == Quat4d(2, 2, 2, 2))
      p := Quat4d(3, 3, 3, 3)
      assert(p() == Quat4d(3, 3, 3, 3))
    }

    {
      val p = new SimpleProperty(Mat2x2f(2))
      assert(p() == Mat2x2f(2))
      p := Mat2x2f(3)
      assert(p() == Mat2x2f(3))
    }
    {
      val p = new SimpleProperty(Mat2x3f(2))
      assert(p() == Mat2x3f(2))
      p := Mat2x3f(3)
      assert(p() == Mat2x3f(3))
    }
    {
      val p = new SimpleProperty(Mat2x4f(2))
      assert(p() == Mat2x4f(2))
      p := Mat2x4f(3)
      assert(p() == Mat2x4f(3))
    }
    {
      val p = new SimpleProperty(Mat3x2f(2))
      assert(p() == Mat3x2f(2))
      p := Mat3x2f(3)
      assert(p() == Mat3x2f(3))
    }
    {
      val p = new SimpleProperty(Mat3x3f(2))
      assert(p() == Mat3x3f(2))
      p := Mat3x3f(3)
      assert(p() == Mat3x3f(3))
    }
    {
      val p = new SimpleProperty(Mat3x4f(2))
      assert(p() == Mat3x4f(2))
      p := Mat3x4f(3)
      assert(p() == Mat3x4f(3))
    }
    {
      val p = new SimpleProperty(Mat4x2f(2))
      assert(p() == Mat4x2f(2))
      p := Mat4x2f(3)
      assert(p() == Mat4x2f(3))
    }
    {
      val p = new SimpleProperty(Mat4x3f(2))
      assert(p() == Mat4x3f(2))
      p := Mat4x3f(3)
      assert(p() == Mat4x3f(3))
    }
    {
      val p = new SimpleProperty(Mat4x4f(2))
      assert(p() == Mat4x4f(2))
      p := Mat4x4f(3)
      assert(p() == Mat4x4f(3))
    }

    {
      val p = new SimpleProperty(Mat2x2d(2))
      assert(p() == Mat2x2d(2))
      p := Mat2x2d(3)
      assert(p() == Mat2x2d(3))
    }
    {
      val p = new SimpleProperty(Mat2x3d(2))
      assert(p() == Mat2x3d(2))
      p := Mat2x3d(3)
      assert(p() == Mat2x3d(3))
    }
    {
      val p = new SimpleProperty(Mat2x4d(2))
      assert(p() == Mat2x4d(2))
      p := Mat2x4d(3)
      assert(p() == Mat2x4d(3))
    }
    {
      val p = new SimpleProperty(Mat3x2d(2))
      assert(p() == Mat3x2d(2))
      p := Mat3x2d(3)
      assert(p() == Mat3x2d(3))
    }
    {
      val p = new SimpleProperty(Mat3x3d(2))
      assert(p() == Mat3x3d(2))
      p := Mat3x3d(3)
      assert(p() == Mat3x3d(3))
    }
    {
      val p = new SimpleProperty(Mat3x4d(2))
      assert(p() == Mat3x4d(2))
      p := Mat3x4d(3)
      assert(p() == Mat3x4d(3))
    }
    {
      val p = new SimpleProperty(Mat4x2d(2))
      assert(p() == Mat4x2d(2))
      p := Mat4x2d(3)
      assert(p() == Mat4x2d(3))
    }
    {
      val p = new SimpleProperty(Mat4x3d(2))
      assert(p() == Mat4x3d(2))
      p := Mat4x3d(3)
      assert(p() == Mat4x3d(3))
    }
    {
      val p = new SimpleProperty(Mat4x4d(2))
      assert(p() == Mat4x4d(2))
      p := Mat4x4d(3)
      assert(p() == Mat4x4d(3))
    }
  }
}
