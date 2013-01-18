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

package simplex3d.test.math.doublex

import org.scalatest._
import simplex3d.math.double._


/**
 * @author Aleksey Nikiforov (lex)
 */
class ConstansTest extends FunSuite {

  test("Constants") {
    {
      val zero = Vec2.Zero
      expectResult(classOf[ConstVec2]) { zero.getClass }
      expectResult((0, 0)) { (zero.x, zero.y) }

      val unitx = Vec2.UnitX
      expectResult(classOf[ConstVec2]) { unitx.getClass }
      expectResult((1, 0)) { (unitx.x, unitx.y) }

      val unity = Vec2.UnitY
      expectResult(classOf[ConstVec2]) { unity.getClass }
      expectResult((0, 1)) { (unity.x, unity.y) }

      val one = Vec2.One
      expectResult(classOf[ConstVec2]) { one.getClass }
      expectResult((1, 1)) { (one.x, one.y) }
    }
    {
      val zero = Vec3.Zero
      expectResult(classOf[ConstVec3]) { zero.getClass }
      expectResult((0, 0, 0)) { (zero.x, zero.y, zero.z) }

      val unitx = Vec3.UnitX
      expectResult(classOf[ConstVec3]) { unitx.getClass }
      expectResult((1, 0, 0)) { (unitx.x, unitx.y, unitx.z) }

      val unity = Vec3.UnitY
      expectResult(classOf[ConstVec3]) { unity.getClass }
      expectResult((0, 1, 0)) { (unity.x, unity.y, unity.z) }

      val unitz = Vec3.UnitZ
      expectResult(classOf[ConstVec3]) { unitz.getClass }
      expectResult((0, 0, 1)) { (unitz.x, unitz.y, unitz.z) }

      val one = Vec3.One
      expectResult(classOf[ConstVec3]) { one.getClass }
      expectResult((1, 1, 1)) { (one.x, one.y, one.z) }
    }
    {
      val zero = Vec4.Zero
      expectResult(classOf[ConstVec4]) { zero.getClass }
      expectResult((0, 0, 0, 0)) { (zero.x, zero.y, zero.z, zero.w) }

      val unitx = Vec4.UnitX
      expectResult(classOf[ConstVec4]) { unitx.getClass }
      expectResult((1, 0, 0, 0)) { (unitx.x, unitx.y, unitx.z, unitx.w) }

      val unity = Vec4.UnitY
      expectResult(classOf[ConstVec4]) { unity.getClass }
      expectResult((0, 1, 0, 0)) { (unity.x, unity.y, unity.z, unity.w) }

      val unitz = Vec4.UnitZ
      expectResult(classOf[ConstVec4]) { unitz.getClass }
      expectResult((0, 0, 1, 0)) { (unitz.x, unitz.y, unitz.z, unitz.w) }

      val unitw = Vec4.UnitW
      expectResult(classOf[ConstVec4]) { unitw.getClass }
      expectResult((0, 0, 0, 1)) { (unitw.x, unitw.y, unitw.z, unitw.w) }

      val one = Vec4.One
      expectResult(classOf[ConstVec4]) { one.getClass }
      expectResult((1, 1, 1, 1)) { (one.x, one.y, one.z, one.w) }
    }
    {
      val identity = Mat2.Identity
      expectResult(classOf[ConstMat2]) { identity.getClass }
      expectResult(Vec2(1, 0)) { identity(0) }
      expectResult(Vec2(0, 1)) { identity(1) }

      val zero = Mat2.Zero
      expectResult(classOf[ConstMat2]) { zero.getClass }
      expectResult(Vec2(0, 0)) { zero(0) }
      expectResult(Vec2(0, 0)) { zero(1) }
    }
    {
      val identity = Mat3x2.Identity
      assert(identity.isInstanceOf[ConstMat3x2])
      expectResult(Vec2(1, 0)) { identity(0) }
      expectResult(Vec2(0, 1)) { identity(1) }
      expectResult(Vec2(0, 0)) { identity(2) }

      val zero = Mat3x2.Zero
      expectResult(classOf[ConstMat3x2]) { zero.getClass }
      expectResult(Vec2(0, 0)) { zero(0) }
      expectResult(Vec2(0, 0)) { zero(1) }
      expectResult(Vec2(0, 0)) { zero(2) }
    }
    {
      val identity = Mat4x2.Identity
      expectResult(classOf[ConstMat4x2]) { identity.getClass }
      expectResult(Vec2(1, 0)) { identity(0) }
      expectResult(Vec2(0, 1)) { identity(1) }
      expectResult(Vec2(0, 0)) { identity(2) }
      expectResult(Vec2(0, 0)) { identity(3) }

      val zero = Mat4x2.Zero
      expectResult(classOf[ConstMat4x2]) { zero.getClass }
      expectResult(Vec2(0, 0)) { zero(0) }
      expectResult(Vec2(0, 0)) { zero(1) }
      expectResult(Vec2(0, 0)) { zero(2) }
      expectResult(Vec2(0, 0)) { zero(3) }
    }
    {
      val identity = Mat2x3.Identity
      expectResult(classOf[ConstMat2x3]) { identity.getClass }
      expectResult(Vec3(1, 0, 0)) { identity(0) }
      expectResult(Vec3(0, 1, 0)) { identity(1) }

      val zero = Mat2x3.Zero
      expectResult(classOf[ConstMat2x3]) { zero.getClass }
      expectResult(Vec3(0, 0, 0)) { zero(0) }
      expectResult(Vec3(0, 0, 0)) { zero(1) }
    }
    {
      val identity = Mat3.Identity
      expectResult(classOf[ConstMat3]) { identity.getClass }
      expectResult(Vec3(1, 0, 0)) { identity(0) }
      expectResult(Vec3(0, 1, 0)) { identity(1) }
      expectResult(Vec3(0, 0, 1)) { identity(2) }

      val zero = Mat3.Zero
      expectResult(classOf[ConstMat3]) { zero.getClass }
      expectResult(Vec3(0, 0, 0)) { zero(0) }
      expectResult(Vec3(0, 0, 0)) { zero(1) }
      expectResult(Vec3(0, 0, 0)) { zero(2) }
    }
    {
      val identity = Mat4x3.Identity
      assert(identity.isInstanceOf[ConstMat4x3])
      expectResult(Vec3(1, 0, 0)) { identity(0) }
      expectResult(Vec3(0, 1, 0)) { identity(1) }
      expectResult(Vec3(0, 0, 1)) { identity(2) }
      expectResult(Vec3(0, 0, 0)) { identity(3) }

      val zero = Mat4x3.Zero
      expectResult(classOf[ConstMat4x3]) { zero.getClass }
      expectResult(Vec3(0, 0, 0)) { zero(0) }
      expectResult(Vec3(0, 0, 0)) { zero(1) }
      expectResult(Vec3(0, 0, 0)) { zero(2) }
      expectResult(Vec3(0, 0, 0)) { zero(3) }
    }
    {
      val identity = Mat2x4.Identity
      expectResult(classOf[ConstMat2x4]) { identity.getClass }
      expectResult(Vec4(1, 0, 0, 0)) { identity(0) }
      expectResult(Vec4(0, 1, 0, 0)) { identity(1) }

      val zero = Mat2x4.Zero
      expectResult(classOf[ConstMat2x4]) { zero.getClass }
      expectResult(Vec4(0, 0, 0, 0)) { zero(0) }
      expectResult(Vec4(0, 0, 0, 0)) { zero(1) }
    }
    {
      val identity = Mat3x4.Identity
      expectResult(classOf[ConstMat3x4]) { identity.getClass }
      expectResult(Vec4(1, 0, 0, 0)) { identity(0) }
      expectResult(Vec4(0, 1, 0, 0)) { identity(1) }
      expectResult(Vec4(0, 0, 1, 0)) { identity(2) }

      val zero = Mat3x4.Zero
      expectResult(classOf[ConstMat3x4]) { zero.getClass }
      expectResult(Vec4(0, 0, 0, 0)) { zero(0) }
      expectResult(Vec4(0, 0, 0, 0)) { zero(1) }
      expectResult(Vec4(0, 0, 0, 0)) { zero(2) }
    }
    {
      val identity = Mat4.Identity
      expectResult(classOf[ConstMat4]) { identity.getClass }
      expectResult(Vec4(1, 0, 0, 0)) { identity(0) }
      expectResult(Vec4(0, 1, 0, 0)) { identity(1) }
      expectResult(Vec4(0, 0, 1, 0)) { identity(2) }
      expectResult(Vec4(0, 0, 0, 1)) { identity(3) }

      val zero = Mat4.Zero
      expectResult(classOf[ConstMat4]) { zero.getClass }
      expectResult(Vec4(0, 0, 0, 0)) { zero(0) }
      expectResult(Vec4(0, 0, 0, 0)) { zero(1) }
      expectResult(Vec4(0, 0, 0, 0)) { zero(2) }
      expectResult(Vec4(0, 0, 0, 0)) { zero(3) }
    }
    {
      val ident = Quat4.Identity
      assert(ident.isInstanceOf[ConstQuat4])
      expectResult((1, 0, 0, 0)) { (ident.a, ident.b, ident.c, ident.d) }
    }
  }

  test("Tag") {
    import scala.reflect._

    assert(Vec2.Tag == classTag[Vec2])
    assert(Vec2.ConstTag == classTag[ConstVec2])
    assert(Vec2.ReadTag == classTag[ReadVec2])

    assert(Vec3.Tag == classTag[Vec3])
    assert(Vec3.ConstTag == classTag[ConstVec3])
    assert(Vec3.ReadTag == classTag[ReadVec3])

    assert(Vec4.Tag == classTag[Vec4])
    assert(Vec4.ConstTag == classTag[ConstVec4])
    assert(Vec4.ReadTag == classTag[ReadVec4])

    assert(Quat4.Tag == classTag[Quat4])
    assert(Quat4.ConstTag == classTag[ConstQuat4])
    assert(Quat4.ReadTag == classTag[ReadQuat4])

    assert(Mat2x2.Tag == classTag[Mat2x2])
    assert(Mat2x2.ConstTag == classTag[ConstMat2x2])
    assert(Mat2x2.ReadTag == classTag[ReadMat2x2])

    assert(Mat3x2.Tag == classTag[Mat3x2])
    assert(Mat3x2.ConstTag == classTag[ConstMat3x2])
    assert(Mat3x2.ReadTag == classTag[ReadMat3x2])

    assert(Mat4x2.Tag == classTag[Mat4x2])
    assert(Mat4x2.ConstTag == classTag[ConstMat4x2])
    assert(Mat4x2.ReadTag == classTag[ReadMat4x2])

    assert(Mat2x3.Tag == classTag[Mat2x3])
    assert(Mat2x3.ConstTag == classTag[ConstMat2x3])
    assert(Mat2x3.ReadTag == classTag[ReadMat2x3])

    assert(Mat3x3.Tag == classTag[Mat3x3])
    assert(Mat3x3.ConstTag == classTag[ConstMat3x3])
    assert(Mat3x3.ReadTag == classTag[ReadMat3x3])

    assert(Mat4x3.Tag == classTag[Mat4x3])
    assert(Mat4x3.ConstTag == classTag[ConstMat4x3])
    assert(Mat4x3.ReadTag == classTag[ReadMat4x3])

    assert(Mat2x4.Tag == classTag[Mat2x4])
    assert(Mat2x4.ConstTag == classTag[ConstMat2x4])
    assert(Mat2x4.ReadTag == classTag[ReadMat2x4])

    assert(Mat3x4.Tag == classTag[Mat3x4])
    assert(Mat3x4.ConstTag == classTag[ConstMat3x4])
    assert(Mat3x4.ReadTag == classTag[ReadMat3x4])

    assert(Mat4x4.Tag == classTag[Mat4x4])
    assert(Mat4x4.ConstTag == classTag[ConstMat4x4])
    assert(Mat4x4.ReadTag == classTag[ReadMat4x4])
  }
}
