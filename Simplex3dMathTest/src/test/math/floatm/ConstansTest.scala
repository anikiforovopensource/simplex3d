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

package test.math.floatm

import org.scalatest._

import simplex3d.math.floatm.renamed._


/**
 * @author Aleksey Nikiforov (lex)
 */
class ConstansTest extends FunSuite {

  test("Constants") {
    {
      val zero = Vec2.Zero
      expect(classOf[ConstVec2]) { zero.getClass }
      expect((0, 0)) { (zero.x, zero.y) }

      val unitx = Vec2.UnitX
      expect(classOf[ConstVec2]) { unitx.getClass }
      expect((1, 0)) { (unitx.x, unitx.y) }

      val unity = Vec2.UnitY
      expect(classOf[ConstVec2]) { unity.getClass }
      expect((0, 1)) { (unity.x, unity.y) }

      val one = Vec2.One
      expect(classOf[ConstVec2]) { one.getClass }
      expect((1, 1)) { (one.x, one.y) }
    }
    {
      val zero = Vec3.Zero
      expect(classOf[ConstVec3]) { zero.getClass }
      expect((0, 0, 0)) { (zero.x, zero.y, zero.z) }

      val unitx = Vec3.UnitX
      expect(classOf[ConstVec3]) { unitx.getClass }
      expect((1, 0, 0)) { (unitx.x, unitx.y, unitx.z) }

      val unity = Vec3.UnitY
      expect(classOf[ConstVec3]) { unity.getClass }
      expect((0, 1, 0)) { (unity.x, unity.y, unity.z) }

      val unitz = Vec3.UnitZ
      expect(classOf[ConstVec3]) { unitz.getClass }
      expect((0, 0, 1)) { (unitz.x, unitz.y, unitz.z) }

      val one = Vec3.One
      expect(classOf[ConstVec3]) { one.getClass }
      expect((1, 1, 1)) { (one.x, one.y, one.z) }
    }
    {
      val zero = Vec4.Zero
      expect(classOf[ConstVec4]) { zero.getClass }
      expect((0, 0, 0, 0)) { (zero.x, zero.y, zero.z, zero.w) }

      val unitx = Vec4.UnitX
      expect(classOf[ConstVec4]) { unitx.getClass }
      expect((1, 0, 0, 0)) { (unitx.x, unitx.y, unitx.z, unitx.w) }

      val unity = Vec4.UnitY
      expect(classOf[ConstVec4]) { unity.getClass }
      expect((0, 1, 0, 0)) { (unity.x, unity.y, unity.z, unity.w) }

      val unitz = Vec4.UnitZ
      expect(classOf[ConstVec4]) { unitz.getClass }
      expect((0, 0, 1, 0)) { (unitz.x, unitz.y, unitz.z, unitz.w) }

      val unitw = Vec4.UnitW
      expect(classOf[ConstVec4]) { unitw.getClass }
      expect((0, 0, 0, 1)) { (unitw.x, unitw.y, unitw.z, unitw.w) }

      val one = Vec4.One
      expect(classOf[ConstVec4]) { one.getClass }
      expect((1, 1, 1, 1)) { (one.x, one.y, one.z, one.w) }
    }
    {
      val identity = Mat2.Identity
      expect(classOf[ConstMat2]) { identity.getClass }
      expect(Vec2(1, 0)) { identity(0) }
      expect(Vec2(0, 1)) { identity(1) }

      val zero = Mat2.Zero
      expect(classOf[ConstMat2]) { zero.getClass }
      expect(Vec2(0, 0)) { zero(0) }
      expect(Vec2(0, 0)) { zero(1) }
    }
    {
      val identity = Mat2x3.Identity
      assert(identity.isInstanceOf[ConstMat2x3])
      expect(Vec2(1, 0)) { identity(0) }
      expect(Vec2(0, 1)) { identity(1) }
      expect(Vec2(0, 0)) { identity(2) }

      val zero = Mat2x3.Zero
      expect(classOf[ConstMat2x3]) { zero.getClass }
      expect(Vec2(0, 0)) { zero(0) }
      expect(Vec2(0, 0)) { zero(1) }
      expect(Vec2(0, 0)) { zero(2) }
    }
    {
      val identity = Mat2x4.Identity
      expect(classOf[ConstMat2x4]) { identity.getClass }
      expect(Vec2(1, 0)) { identity(0) }
      expect(Vec2(0, 1)) { identity(1) }
      expect(Vec2(0, 0)) { identity(2) }
      expect(Vec2(0, 0)) { identity(3) }

      val zero = Mat2x4.Zero
      expect(classOf[ConstMat2x4]) { zero.getClass }
      expect(Vec2(0, 0)) { zero(0) }
      expect(Vec2(0, 0)) { zero(1) }
      expect(Vec2(0, 0)) { zero(2) }
      expect(Vec2(0, 0)) { zero(3) }
    }
    {
      val identity = Mat3x2.Identity
      expect(classOf[ConstMat3x2]) { identity.getClass }
      expect(Vec3(1, 0, 0)) { identity(0) }
      expect(Vec3(0, 1, 0)) { identity(1) }

      val zero = Mat3x2.Zero
      expect(classOf[ConstMat3x2]) { zero.getClass }
      expect(Vec3(0, 0, 0)) { zero(0) }
      expect(Vec3(0, 0, 0)) { zero(1) }
    }
    {
      val identity = Mat3.Identity
      expect(classOf[ConstMat3]) { identity.getClass }
      expect(Vec3(1, 0, 0)) { identity(0) }
      expect(Vec3(0, 1, 0)) { identity(1) }
      expect(Vec3(0, 0, 1)) { identity(2) }

      val zero = Mat3.Zero
      expect(classOf[ConstMat3]) { zero.getClass }
      expect(Vec3(0, 0, 0)) { zero(0) }
      expect(Vec3(0, 0, 0)) { zero(1) }
      expect(Vec3(0, 0, 0)) { zero(2) }
    }
    {
      val identity = Mat3x4.Identity
      assert(identity.isInstanceOf[ConstMat3x4])
      expect(Vec3(1, 0, 0)) { identity(0) }
      expect(Vec3(0, 1, 0)) { identity(1) }
      expect(Vec3(0, 0, 1)) { identity(2) }
      expect(Vec3(0, 0, 0)) { identity(3) }

      val zero = Mat3x4.Zero
      expect(classOf[ConstMat3x4]) { zero.getClass }
      expect(Vec3(0, 0, 0)) { zero(0) }
      expect(Vec3(0, 0, 0)) { zero(1) }
      expect(Vec3(0, 0, 0)) { zero(2) }
      expect(Vec3(0, 0, 0)) { zero(3) }
    }
    {
      val identity = Mat4x2.Identity
      expect(classOf[ConstMat4x2]) { identity.getClass }
      expect(Vec4(1, 0, 0, 0)) { identity(0) }
      expect(Vec4(0, 1, 0, 0)) { identity(1) }

      val zero = Mat4x2.Zero
      expect(classOf[ConstMat4x2]) { zero.getClass }
      expect(Vec4(0, 0, 0, 0)) { zero(0) }
      expect(Vec4(0, 0, 0, 0)) { zero(1) }
    }
    {
      val identity = Mat4x3.Identity
      expect(classOf[ConstMat4x3]) { identity.getClass }
      expect(Vec4(1, 0, 0, 0)) { identity(0) }
      expect(Vec4(0, 1, 0, 0)) { identity(1) }
      expect(Vec4(0, 0, 1, 0)) { identity(2) }

      val zero = Mat4x3.Zero
      expect(classOf[ConstMat4x3]) { zero.getClass }
      expect(Vec4(0, 0, 0, 0)) { zero(0) }
      expect(Vec4(0, 0, 0, 0)) { zero(1) }
      expect(Vec4(0, 0, 0, 0)) { zero(2) }
    }
    {
      val identity = Mat4.Identity
      expect(classOf[ConstMat4]) { identity.getClass }
      expect(Vec4(1, 0, 0, 0)) { identity(0) }
      expect(Vec4(0, 1, 0, 0)) { identity(1) }
      expect(Vec4(0, 0, 1, 0)) { identity(2) }
      expect(Vec4(0, 0, 0, 1)) { identity(3) }

      val zero = Mat4.Zero
      expect(classOf[ConstMat4]) { zero.getClass }
      expect(Vec4(0, 0, 0, 0)) { zero(0) }
      expect(Vec4(0, 0, 0, 0)) { zero(1) }
      expect(Vec4(0, 0, 0, 0)) { zero(2) }
      expect(Vec4(0, 0, 0, 0)) { zero(3) }
    }
    {
      val ident = Quat4.Identity
      assert(ident.isInstanceOf[ConstQuat4])
      expect((1, 0, 0, 0)) { (ident.a, ident.b, ident.c, ident.d) }
    }
  }
}
