/*
 * Simplex3d, MathTest package
 * Copyright (C) 2010, Simplex3d Team
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

package test.math

import org.scalatest._

import simplex3d.math._
import simplex3d.math.property._
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
    protected val value = init.copyAsMutable()
  }

  test("PrimitiveVal") {
    def mutable[T](v: PropertyValue[T]) = v.copyAsMutable()

    val b = mutable(true)
    assert(b.asReadInstance() == true)
    b := false
    assert(b.asReadInstance() == false)

    val i = mutable(1)
    assert(i.asReadInstance() == 1)
    i := 2
    assert(i.asReadInstance() == 2)

    val f = mutable(1f)
    assert(f.asReadInstance() == 1f)
    f := 2f
    assert(f.asReadInstance() == 2f)

    val d = mutable(1d)
    assert(d.asReadInstance() == 1d)
    d := 2d
    assert(d.asReadInstance() == 2d)
  }

  test("PropertyVal") {
    {
      val v = Vec2b(true)

      val m = v.copyAsMutable()
      assert(m ne v); assert(m == v)
      assert(m.isInstanceOf[Vec2b])

      val c = v.copyAsImmutable()
      assert(c ne v); assert(c == v)
      assert(c.isInstanceOf[ConstVec2b])
    }
    {
      val v = Vec3b(true)

      val m = v.copyAsMutable()
      assert(m ne v); assert(m == v)
      assert(m.isInstanceOf[Vec3b])

      val c = v.copyAsImmutable()
      assert(c ne v); assert(c == v)
      assert(c.isInstanceOf[ConstVec3b])
    }
    {
      val v = Vec4b(true)

      val m = v.copyAsMutable()
      assert(m ne v); assert(m == v)
      assert(m.isInstanceOf[Vec4b])

      val c = v.copyAsImmutable()
      assert(c ne v); assert(c == v)
      assert(c.isInstanceOf[ConstVec4b])
    }

    {
      val v = Vec2i(2)

      val m = v.copyAsMutable()
      assert(m ne v); assert(m == v)
      assert(m.isInstanceOf[Vec2i])

      val c = v.copyAsImmutable()
      assert(c ne v); assert(c == v)
      assert(c.isInstanceOf[ConstVec2i])
    }
    {
      val v = Vec3i(2)

      val m = v.copyAsMutable()
      assert(m ne v); assert(m == v)
      assert(m.isInstanceOf[Vec3i])

      val c = v.copyAsImmutable()
      assert(c ne v); assert(c == v)
      assert(c.isInstanceOf[ConstVec3i])
    }
    {
      val v = Vec4i(2)

      val m = v.copyAsMutable()
      assert(m ne v); assert(m == v)
      assert(m.isInstanceOf[Vec4i])

      val c = v.copyAsImmutable()
      assert(c ne v); assert(c == v)
      assert(c.isInstanceOf[ConstVec4i])
    }

    {
      val v = Vec2f(2)

      val m = v.copyAsMutable()
      assert(m ne v); assert(m == v)
      assert(m.isInstanceOf[Vec2f])

      val c = v.copyAsImmutable()
      assert(c ne v); assert(c == v)
      assert(c.isInstanceOf[ConstVec2f])
    }
    {
      val v = Vec3f(2)

      val m = v.copyAsMutable()
      assert(m ne v); assert(m == v)
      assert(m.isInstanceOf[Vec3f])

      val c = v.copyAsImmutable()
      assert(c ne v); assert(c == v)
      assert(c.isInstanceOf[ConstVec3f])
    }
    {
      val v = Vec4f(2)

      val m = v.copyAsMutable()
      assert(m ne v); assert(m == v)
      assert(m.isInstanceOf[Vec4f])

      val c = v.copyAsImmutable()
      assert(c ne v); assert(c == v)
      assert(c.isInstanceOf[ConstVec4f])
    }

    {
      val v = Vec2d(2)

      val m = v.copyAsMutable()
      assert(m ne v); assert(m == v)
      assert(m.isInstanceOf[Vec2d])

      val c = v.copyAsImmutable()
      assert(c ne v); assert(c == v)
      assert(c.isInstanceOf[ConstVec2d])
    }
    {
      val v = Vec3d(2)

      val m = v.copyAsMutable()
      assert(m ne v); assert(m == v)
      assert(m.isInstanceOf[Vec3d])

      val c = v.copyAsImmutable()
      assert(c ne v); assert(c == v)
      assert(c.isInstanceOf[ConstVec3d])
    }
    {
      val v = Vec4d(2)

      val m = v.copyAsMutable()
      assert(m ne v); assert(m == v)
      assert(m.isInstanceOf[Vec4d])

      val c = v.copyAsImmutable()
      assert(c ne v); assert(c == v)
      assert(c.isInstanceOf[ConstVec4d])
    }

    {
      val v = Quat4f(1, 2, 3, 4)

      val m = v.copyAsMutable()
      assert(m ne v); assert(m == v)
      assert(m.isInstanceOf[Quat4f])

      val c = v.copyAsImmutable()
      assert(c ne v); assert(c == v)
      assert(c.isInstanceOf[ConstQuat4f])
    }
    {
      val v = Quat4d(1, 2, 3, 4)

      val m = v.copyAsMutable()
      assert(m ne v); assert(m == v)
      assert(m.isInstanceOf[Quat4d])

      val c = v.copyAsImmutable()
      assert(c ne v); assert(c == v)
      assert(c.isInstanceOf[ConstQuat4d])
    }

    {
      val v = Mat2x2f(2)

      val m = v.copyAsMutable()
      assert(m ne v); assert(m == v)
      assert(m.isInstanceOf[Mat2x2f])

      val c = v.copyAsImmutable()
      assert(c ne v); assert(c == v)
      assert(c.isInstanceOf[ConstMat2x2f])
    }
    {
      val v = Mat2x3f(2)

      val m = v.copyAsMutable()
      assert(m ne v); assert(m == v)
      assert(m.isInstanceOf[Mat2x3f])

      val c = v.copyAsImmutable()
      assert(c ne v); assert(c == v)
      assert(c.isInstanceOf[ConstMat2x3f])
    }
    {
      val v = Mat2x4f(2)

      val m = v.copyAsMutable()
      assert(m ne v); assert(m == v)
      assert(m.isInstanceOf[Mat2x4f])

      val c = v.copyAsImmutable()
      assert(c ne v); assert(c == v)
      assert(c.isInstanceOf[ConstMat2x4f])
    }

    {
      val v = Mat3x2f(2)

      val m = v.copyAsMutable()
      assert(m ne v); assert(m == v)
      assert(m.isInstanceOf[Mat3x2f])

      val c = v.copyAsImmutable()
      assert(c ne v); assert(c == v)
      assert(c.isInstanceOf[ConstMat3x2f])
    }
    {
      val v = Mat3x3f(2)

      val m = v.copyAsMutable()
      assert(m ne v); assert(m == v)
      assert(m.isInstanceOf[Mat3x3f])

      val c = v.copyAsImmutable()
      assert(c ne v); assert(c == v)
      assert(c.isInstanceOf[ConstMat3x3f])
    }
    {
      val v = Mat3x4f(2)

      val m = v.copyAsMutable()
      assert(m ne v); assert(m == v)
      assert(m.isInstanceOf[Mat3x4f])

      val c = v.copyAsImmutable()
      assert(c ne v); assert(c == v)
      assert(c.isInstanceOf[ConstMat3x4f])
    }

    {
      val v = Mat4x2f(2)

      val m = v.copyAsMutable()
      assert(m ne v); assert(m == v)
      assert(m.isInstanceOf[Mat4x2f])

      val c = v.copyAsImmutable()
      assert(c ne v); assert(c == v)
      assert(c.isInstanceOf[ConstMat4x2f])
    }
    {
      val v = Mat4x3f(2)

      val m = v.copyAsMutable()
      assert(m ne v); assert(m == v)
      assert(m.isInstanceOf[Mat4x3f])

      val c = v.copyAsImmutable()
      assert(c ne v); assert(c == v)
      assert(c.isInstanceOf[ConstMat4x3f])
    }
    {
      val v = Mat4x4f(2)

      val m = v.copyAsMutable()
      assert(m ne v); assert(m == v)
      assert(m.isInstanceOf[Mat4x4f])

      val c = v.copyAsImmutable()
      assert(c ne v); assert(c == v)
      assert(c.isInstanceOf[ConstMat4x4f])
    }


    {
      val v = Mat2x2d(2)

      val m = v.copyAsMutable()
      assert(m ne v); assert(m == v)
      assert(m.isInstanceOf[Mat2x2d])

      val c = v.copyAsImmutable()
      assert(c ne v); assert(c == v)
      assert(c.isInstanceOf[ConstMat2x2d])
    }
    {
      val v = Mat2x3d(2)

      val m = v.copyAsMutable()
      assert(m ne v); assert(m == v)
      assert(m.isInstanceOf[Mat2x3d])

      val c = v.copyAsImmutable()
      assert(c ne v); assert(c == v)
      assert(c.isInstanceOf[ConstMat2x3d])
    }
    {
      val v = Mat2x4d(2)

      val m = v.copyAsMutable()
      assert(m ne v); assert(m == v)
      assert(m.isInstanceOf[Mat2x4d])

      val c = v.copyAsImmutable()
      assert(c ne v); assert(c == v)
      assert(c.isInstanceOf[ConstMat2x4d])
    }

    {
      val v = Mat3x2d(2)

      val m = v.copyAsMutable()
      assert(m ne v); assert(m == v)
      assert(m.isInstanceOf[Mat3x2d])

      val c = v.copyAsImmutable()
      assert(c ne v); assert(c == v)
      assert(c.isInstanceOf[ConstMat3x2d])
    }
    {
      val v = Mat3x3d(2)

      val m = v.copyAsMutable()
      assert(m ne v); assert(m == v)
      assert(m.isInstanceOf[Mat3x3d])

      val c = v.copyAsImmutable()
      assert(c ne v); assert(c == v)
      assert(c.isInstanceOf[ConstMat3x3d])
    }
    {
      val v = Mat3x4d(2)

      val m = v.copyAsMutable()
      assert(m ne v); assert(m == v)
      assert(m.isInstanceOf[Mat3x4d])

      val c = v.copyAsImmutable()
      assert(c ne v); assert(c == v)
      assert(c.isInstanceOf[ConstMat3x4d])
    }

    {
      val v = Mat4x2d(2)

      val m = v.copyAsMutable()
      assert(m ne v); assert(m == v)
      assert(m.isInstanceOf[Mat4x2d])

      val c = v.copyAsImmutable()
      assert(c ne v); assert(c == v)
      assert(c.isInstanceOf[ConstMat4x2d])
    }
    {
      val v = Mat4x3d(2)

      val m = v.copyAsMutable()
      assert(m ne v); assert(m == v)
      assert(m.isInstanceOf[Mat4x3d])

      val c = v.copyAsImmutable()
      assert(c ne v); assert(c == v)
      assert(c.isInstanceOf[ConstMat4x3d])
    }
    {
      val v = Mat4x4d(2)

      val m = v.copyAsMutable()
      assert(m ne v); assert(m == v)
      assert(m.isInstanceOf[Mat4x4d])

      val c = v.copyAsImmutable()
      assert(c ne v); assert(c == v)
      assert(c.isInstanceOf[ConstMat4x4d])
    }
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
      checkType(p())
    }
    {
      val p = new SimpleProperty(Vec3b(true))
      assert(p() == Vec3b(true))
      p := Vec3b(false)
      assert(p() == Vec3b(false))
      checkType(p())
    }
    {
      val p = new SimpleProperty(Vec4b(true))
      assert(p() == Vec4b(true))
      p := Vec4b(false)
      assert(p() == Vec4b(false))
      checkType(p())
    }

    {
      val p = new SimpleProperty(Vec2i(2))
      assert(p() == Vec2i(2))
      p := Vec2i(3)
      assert(p() == Vec2i(3))
      checkType(p())
    }
    {
      val p = new SimpleProperty(Vec3i(2))
      assert(p() == Vec3i(2))
      p := Vec3i(3)
      assert(p() == Vec3i(3))
      checkType(p())
    }
    {
      val p = new SimpleProperty(Vec4i(2))
      assert(p() == Vec4i(2))
      p := Vec4i(3)
      assert(p() == Vec4i(3))
      checkType(p())
    }

    {
      val p = new SimpleProperty(Vec2f(2))
      assert(p() == Vec2f(2))
      p := Vec2f(3)
      assert(p() == Vec2f(3))
      checkType(p())
    }
    {
      val p = new SimpleProperty(Vec3f(2))
      assert(p() == Vec3f(2))
      p := Vec3f(3)
      assert(p() == Vec3f(3))
      checkType(p())
    }
    {
      val p = new SimpleProperty(Vec4f(2))
      assert(p() == Vec4f(2))
      p := Vec4f(3)
      assert(p() == Vec4f(3))
      checkType(p())
    }

    {
      val p = new SimpleProperty(Vec2d(2))
      assert(p() == Vec2d(2))
      p := Vec2d(3)
      assert(p() == Vec2d(3))
      checkType(p())
    }
    {
      val p = new SimpleProperty(Vec3d(2))
      assert(p() == Vec3d(2))
      p := Vec3d(3)
      assert(p() == Vec3d(3))
      checkType(p())
    }
    {
      val p = new SimpleProperty(Vec4d(2))
      assert(p() == Vec4d(2))
      p := Vec4d(3)
      assert(p() == Vec4d(3))
      checkType(p())
    }

    {
      val p = new SimpleProperty(Quat4f(2, 2, 2, 2))
      assert(p() == Quat4f(2, 2, 2, 2))
      p := Quat4f(3, 3, 3, 3)
      assert(p() == Quat4f(3, 3, 3, 3))
      checkType(p())
    }
    {
      val p = new SimpleProperty(Quat4d(2, 2, 2, 2))
      assert(p() == Quat4d(2, 2, 2, 2))
      p := Quat4d(3, 3, 3, 3)
      assert(p() == Quat4d(3, 3, 3, 3))
      checkType(p())
    }

    {
      val p = new SimpleProperty(Mat2x2f(2))
      assert(p() == Mat2x2f(2))
      p := Mat2x2f(3)
      assert(p() == Mat2x2f(3))
      checkType(p())
    }
    {
      val p = new SimpleProperty(Mat2x3f(2))
      assert(p() == Mat2x3f(2))
      p := Mat2x3f(3)
      assert(p() == Mat2x3f(3))
      checkType(p())
    }
    {
      val p = new SimpleProperty(Mat2x4f(2))
      assert(p() == Mat2x4f(2))
      p := Mat2x4f(3)
      assert(p() == Mat2x4f(3))
      checkType(p())
    }
    {
      val p = new SimpleProperty(Mat3x2f(2))
      assert(p() == Mat3x2f(2))
      p := Mat3x2f(3)
      assert(p() == Mat3x2f(3))
      checkType(p())
    }
    {
      val p = new SimpleProperty(Mat3x3f(2))
      assert(p() == Mat3x3f(2))
      p := Mat3x3f(3)
      assert(p() == Mat3x3f(3))
      checkType(p())
    }
    {
      val p = new SimpleProperty(Mat3x4f(2))
      assert(p() == Mat3x4f(2))
      p := Mat3x4f(3)
      assert(p() == Mat3x4f(3))
      checkType(p())
    }
    {
      val p = new SimpleProperty(Mat4x2f(2))
      assert(p() == Mat4x2f(2))
      p := Mat4x2f(3)
      assert(p() == Mat4x2f(3))
      checkType(p())
    }
    {
      val p = new SimpleProperty(Mat4x3f(2))
      assert(p() == Mat4x3f(2))
      p := Mat4x3f(3)
      assert(p() == Mat4x3f(3))
      checkType(p())
    }
    {
      val p = new SimpleProperty(Mat4x4f(2))
      assert(p() == Mat4x4f(2))
      p := Mat4x4f(3)
      assert(p() == Mat4x4f(3))
      checkType(p())
    }

    {
      val p = new SimpleProperty(Mat2x2d(2))
      assert(p() == Mat2x2d(2))
      p := Mat2x2d(3)
      assert(p() == Mat2x2d(3))
      checkType(p())
    }
    {
      val p = new SimpleProperty(Mat2x3d(2))
      assert(p() == Mat2x3d(2))
      p := Mat2x3d(3)
      assert(p() == Mat2x3d(3))
      checkType(p())
    }
    {
      val p = new SimpleProperty(Mat2x4d(2))
      assert(p() == Mat2x4d(2))
      p := Mat2x4d(3)
      assert(p() == Mat2x4d(3))
      checkType(p())
    }
    {
      val p = new SimpleProperty(Mat3x2d(2))
      assert(p() == Mat3x2d(2))
      p := Mat3x2d(3)
      assert(p() == Mat3x2d(3))
      checkType(p())
    }
    {
      val p = new SimpleProperty(Mat3x3d(2))
      assert(p() == Mat3x3d(2))
      p := Mat3x3d(3)
      assert(p() == Mat3x3d(3))
      checkType(p())
    }
    {
      val p = new SimpleProperty(Mat3x4d(2))
      assert(p() == Mat3x4d(2))
      p := Mat3x4d(3)
      assert(p() == Mat3x4d(3))
      checkType(p())
    }
    {
      val p = new SimpleProperty(Mat4x2d(2))
      assert(p() == Mat4x2d(2))
      p := Mat4x2d(3)
      assert(p() == Mat4x2d(3))
      checkType(p())
    }
    {
      val p = new SimpleProperty(Mat4x3d(2))
      assert(p() == Mat4x3d(2))
      p := Mat4x3d(3)
      assert(p() == Mat4x3d(3))
      checkType(p())
    }
    {
      val p = new SimpleProperty(Mat4x4d(2))
      assert(p() == Mat4x4d(2))
      p := Mat4x4d(3)
      assert(p() == Mat4x4d(3))
      checkType(p())
    }
  }

  def checkType(value: ReadVec2b) {}
  def checkType(value: Vec2b) { throw new AssertionError }
  def checkType(value: ReadVec3b) {}
  def checkType(value: Vec3b) { throw new AssertionError }
  def checkType(value: ReadVec4b) {}
  def checkType(value: Vec4b) { throw new AssertionError }

  def checkType(value: ReadVec2i) {}
  def checkType(value: Vec2i) { throw new AssertionError }
  def checkType(value: ReadVec3i) {}
  def checkType(value: Vec3i) { throw new AssertionError }
  def checkType(value: ReadVec4i) {}
  def checkType(value: Vec4i) { throw new AssertionError }

  def checkType(value: ReadVec2f) {}
  def checkType(value: Vec2f) { throw new AssertionError }
  def checkType(value: ReadVec3f) {}
  def checkType(value: Vec3f) { throw new AssertionError }
  def checkType(value: ReadVec4f) {}
  def checkType(value: Vec4f) { throw new AssertionError }

  def checkType(value: ReadVec2d) {}
  def checkType(value: Vec2d) { throw new AssertionError }
  def checkType(value: ReadVec3d) {}
  def checkType(value: Vec3d) { throw new AssertionError }
  def checkType(value: ReadVec4d) {}
  def checkType(value: Vec4d) { throw new AssertionError }

  def checkType(value: ReadQuat4f) {}
  def checkType(value: Quat4f) { throw new AssertionError }
  def checkType(value: ReadQuat4d) {}
  def checkType(value: Quat4d) { throw new AssertionError }

  def checkType(value: ReadMat2x2f) {}
  def checkType(value: Mat2x2f) { throw new AssertionError }
  def checkType(value: ReadMat2x3f) {}
  def checkType(value: Mat2x3f) { throw new AssertionError }
  def checkType(value: ReadMat2x4f) {}
  def checkType(value: Mat2x4f) { throw new AssertionError }

  def checkType(value: ReadMat3x2f) {}
  def checkType(value: Mat3x2f) { throw new AssertionError }
  def checkType(value: ReadMat3x3f) {}
  def checkType(value: Mat3x3f) { throw new AssertionError }
  def checkType(value: ReadMat3x4f) {}
  def checkType(value: Mat3x4f) { throw new AssertionError }

  def checkType(value: ReadMat4x2f) {}
  def checkType(value: Mat4x2f) { throw new AssertionError }
  def checkType(value: ReadMat4x3f) {}
  def checkType(value: Mat4x3f) { throw new AssertionError }
  def checkType(value: ReadMat4x4f) {}
  def checkType(value: Mat4x4f) { throw new AssertionError }

  def checkType(value: ReadMat2x2d) {}
  def checkType(value: Mat2x2d) { throw new AssertionError }
  def checkType(value: ReadMat2x3d) {}
  def checkType(value: Mat2x3d) { throw new AssertionError }
  def checkType(value: ReadMat2x4d) {}
  def checkType(value: Mat2x4d) { throw new AssertionError }

  def checkType(value: ReadMat3x2d) {}
  def checkType(value: Mat3x2d) { throw new AssertionError }
  def checkType(value: ReadMat3x3d) {}
  def checkType(value: Mat3x3d) { throw new AssertionError }
  def checkType(value: ReadMat3x4d) {}
  def checkType(value: Mat3x4d) { throw new AssertionError }

  def checkType(value: ReadMat4x2d) {}
  def checkType(value: Mat4x2d) { throw new AssertionError }
  def checkType(value: ReadMat4x3d) {}
  def checkType(value: Mat4x3d) { throw new AssertionError }
  def checkType(value: ReadMat4x4d) {}
  def checkType(value: Mat4x4d) { throw new AssertionError }
}
