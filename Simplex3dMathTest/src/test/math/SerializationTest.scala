/*
 * Simplex3dMath - Test Package
 * Copyright (C) 2010-2011, Aleksey Nikiforov
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

import java.io._
import org.scalatest._

import simplex3d.math._
import simplex3d.math.floatx._
import simplex3d.math.doublex._


/**
 * @author Aleksey Nikiforov (lex)
 */
class SerializationTest extends FunSuite {

  test("Boolean Serialization") {
    val bytes = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bytes)

    BooleanCombinations.test { (x, y, z, w) =>
      out.writeObject(new BooleanRef(x))
      out.writeObject(Vec2b(x, y))
      out.writeObject(Vec3b(x, y, z))
      out.writeObject(Vec4b(x, y, z, w))
      out.writeObject(ConstVec2b(x, y))
      out.writeObject(ConstVec3b(x, y, z))
      out.writeObject(ConstVec4b(x, y, z, w))
    }

    out.close()
    val in = new ObjectInputStream(new ByteArrayInputStream(bytes.toByteArray))

    BooleanCombinations.test { (x, y, z, w) =>
      assert(new BooleanRef(x) == in.readObject())
      assert(Vec2b(x, y) == in.readObject())
      assert(Vec3b(x, y, z) == in.readObject())
      assert(Vec4b(x, y, z, w) == in.readObject())
      assert(Vec2b(x, y) == in.readObject())
      assert(Vec3b(x, y, z) == in.readObject())
      assert(Vec4b(x, y, z, w) == in.readObject())
    }

    in.close()
  }

  test("Serialization") {
    val m = Mat4f(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)

    val bytes = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bytes)

    out.writeObject(new IntRef(1))
    out.writeObject(Vec2i(1, 2))
    out.writeObject(Vec3i(1, 2, 3))
    out.writeObject(Vec4i(1, 2, 3, 4))
    out.writeObject(ConstVec2i(1, 2))
    out.writeObject(ConstVec3i(1, 2, 3))
    out.writeObject(ConstVec4i(1, 2, 3, 4))

    out.writeObject(new FloatRef(1))
    out.writeObject(Vec2f(1, 2))
    out.writeObject(Vec3f(1, 2, 3))
    out.writeObject(Vec4f(1, 2, 3, 4))
    out.writeObject(ConstVec2f(1, 2))
    out.writeObject(ConstVec3f(1, 2, 3))
    out.writeObject(ConstVec4f(1, 2, 3, 4))

    out.writeObject(new DoubleRef(1))
    out.writeObject(Vec2d(1, 2))
    out.writeObject(Vec3d(1, 2, 3))
    out.writeObject(Vec4d(1, 2, 3, 4))
    out.writeObject(ConstVec2d(1, 2))
    out.writeObject(ConstVec3d(1, 2, 3))
    out.writeObject(ConstVec4d(1, 2, 3, 4))

    out.writeObject(Quat4f(1, 2, 3, 4))
    out.writeObject(ConstQuat4f(1, 2, 3, 4))
    out.writeObject(Quat4d(1, 2, 3, 4))
    out.writeObject(ConstQuat4d(1, 2, 3, 4))

    out.writeObject(Mat2x2f(m))
    out.writeObject(Mat2x3f(m))
    out.writeObject(Mat2x4f(m))
    out.writeObject(Mat3x2f(m))
    out.writeObject(Mat3x3f(m))
    out.writeObject(Mat3x4f(m))
    out.writeObject(Mat4x2f(m))
    out.writeObject(Mat4x3f(m))
    out.writeObject(Mat4x4f(m))
    out.writeObject(ConstMat2x2f(m))
    out.writeObject(ConstMat2x3f(m))
    out.writeObject(ConstMat2x4f(m))
    out.writeObject(ConstMat3x2f(m))
    out.writeObject(ConstMat3x3f(m))
    out.writeObject(ConstMat3x4f(m))
    out.writeObject(ConstMat4x2f(m))
    out.writeObject(ConstMat4x3f(m))
    out.writeObject(ConstMat4x4f(m))

    out.writeObject(Mat2x2d(m))
    out.writeObject(Mat2x3d(m))
    out.writeObject(Mat2x4d(m))
    out.writeObject(Mat3x2d(m))
    out.writeObject(Mat3x3d(m))
    out.writeObject(Mat3x4d(m))
    out.writeObject(Mat4x2d(m))
    out.writeObject(Mat4x3d(m))
    out.writeObject(Mat4x4d(m))
    out.writeObject(ConstMat2x2d(m))
    out.writeObject(ConstMat2x3d(m))
    out.writeObject(ConstMat2x4d(m))
    out.writeObject(ConstMat3x2d(m))
    out.writeObject(ConstMat3x3d(m))
    out.writeObject(ConstMat3x4d(m))
    out.writeObject(ConstMat4x2d(m))
    out.writeObject(ConstMat4x3d(m))
    out.writeObject(ConstMat4x4d(m))

    out.close()
    val in = new ObjectInputStream(new ByteArrayInputStream(bytes.toByteArray))

    assert(new IntRef(1) == in.readObject())
    assert(Vec2i(1, 2) == in.readObject())
    assert(Vec3i(1, 2, 3) == in.readObject())
    assert(Vec4i(1, 2, 3, 4) == in.readObject())
    assert(Vec2i(1, 2) == in.readObject())
    assert(Vec3i(1, 2, 3) == in.readObject())
    assert(Vec4i(1, 2, 3, 4) == in.readObject())

    assert(new FloatRef(1) == in.readObject())
    assert(Vec2f(1, 2) == in.readObject())
    assert(Vec3f(1, 2, 3) == in.readObject())
    assert(Vec4f(1, 2, 3, 4) == in.readObject())
    assert(Vec2f(1, 2) == in.readObject())
    assert(Vec3f(1, 2, 3) == in.readObject())
    assert(Vec4f(1, 2, 3, 4) == in.readObject())

    assert(new DoubleRef(1) == in.readObject())
    assert(Vec2d(1, 2) == in.readObject())
    assert(Vec3d(1, 2, 3) == in.readObject())
    assert(Vec4d(1, 2, 3, 4) == in.readObject())
    assert(Vec2d(1, 2) == in.readObject())
    assert(Vec3d(1, 2, 3) == in.readObject())
    assert(Vec4d(1, 2, 3, 4) == in.readObject())

    assert(Quat4f(1, 2, 3, 4) == in.readObject())
    assert(Quat4f(1, 2, 3, 4) == in.readObject())
    assert(Quat4d(1, 2, 3, 4) == in.readObject())
    assert(Quat4d(1, 2, 3, 4) == in.readObject())

    assert(Mat2x2f(m) == in.readObject())
    assert(Mat2x3f(m) == in.readObject())
    assert(Mat2x4f(m) == in.readObject())
    assert(Mat3x2f(m) == in.readObject())
    assert(Mat3x3f(m) == in.readObject())
    assert(Mat3x4f(m) == in.readObject())
    assert(Mat4x2f(m) == in.readObject())
    assert(Mat4x3f(m) == in.readObject())
    assert(Mat4x4f(m) == in.readObject())
    assert(Mat2x2f(m) == in.readObject())
    assert(Mat2x3f(m) == in.readObject())
    assert(Mat2x4f(m) == in.readObject())
    assert(Mat3x2f(m) == in.readObject())
    assert(Mat3x3f(m) == in.readObject())
    assert(Mat3x4f(m) == in.readObject())
    assert(Mat4x2f(m) == in.readObject())
    assert(Mat4x3f(m) == in.readObject())
    assert(Mat4x4f(m) == in.readObject())

    assert(Mat2x2d(m) == in.readObject())
    assert(Mat2x3d(m) == in.readObject())
    assert(Mat2x4d(m) == in.readObject())
    assert(Mat3x2d(m) == in.readObject())
    assert(Mat3x3d(m) == in.readObject())
    assert(Mat3x4d(m) == in.readObject())
    assert(Mat4x2d(m) == in.readObject())
    assert(Mat4x3d(m) == in.readObject())
    assert(Mat4x4d(m) == in.readObject())
    assert(Mat2x2d(m) == in.readObject())
    assert(Mat2x3d(m) == in.readObject())
    assert(Mat2x4d(m) == in.readObject())
    assert(Mat3x2d(m) == in.readObject())
    assert(Mat3x3d(m) == in.readObject())
    assert(Mat3x4d(m) == in.readObject())
    assert(Mat4x2d(m) == in.readObject())
    assert(Mat4x3d(m) == in.readObject())
    assert(Mat4x4d(m) == in.readObject())

    in.close()
  }
}
