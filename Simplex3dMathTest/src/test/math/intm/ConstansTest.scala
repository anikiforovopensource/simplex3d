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

package test.math.intm

import org.scalatest._

import simplex3d.math.intm._


/**
 * @author Aleksey Nikiforov (lex)
 */
class ConstansTest extends FunSuite {

  test("Constants") {
    {
      val zero = Vec2i.Zero
      expect(classOf[ConstVec2i]) { zero.getClass }
      expect((0, 0)) { (zero.x, zero.y) }

      val unitx = Vec2i.UnitX
      expect(classOf[ConstVec2i]) { unitx.getClass }
      expect((1, 0)) { (unitx.x, unitx.y) }

      val unity = Vec2i.UnitY
      expect(classOf[ConstVec2i]) { unity.getClass }
      expect((0, 1)) { (unity.x, unity.y) }

      val one = Vec2i.One
      expect(classOf[ConstVec2i]) { one.getClass }
      expect((1, 1)) { (one.x, one.y) }
    }
    {
      val zero = Vec3i.Zero
      expect(classOf[ConstVec3i]) { zero.getClass }
      expect((0, 0, 0)) { (zero.x, zero.y, zero.z) }

      val unitx = Vec3i.UnitX
      expect(classOf[ConstVec3i]) { unitx.getClass }
      expect((1, 0, 0)) { (unitx.x, unitx.y, unitx.z) }

      val unity = Vec3i.UnitY
      expect(classOf[ConstVec3i]) { unity.getClass }
      expect((0, 1, 0)) { (unity.x, unity.y, unity.z) }

      val unitz = Vec3i.UnitZ
      expect(classOf[ConstVec3i]) { unitz.getClass }
      expect((0, 0, 1)) { (unitz.x, unitz.y, unitz.z) }

      val one = Vec3i.One
      expect(classOf[ConstVec3i]) { one.getClass }
      expect((1, 1, 1)) { (one.x, one.y, one.z) }
    }
    {
      val zero = Vec4i.Zero
      expect(classOf[ConstVec4i]) { zero.getClass }
      expect((0, 0, 0, 0)) { (zero.x, zero.y, zero.z, zero.w) }

      val unitx = Vec4i.UnitX
      expect(classOf[ConstVec4i]) { unitx.getClass }
      expect((1, 0, 0, 0)) { (unitx.x, unitx.y, unitx.z, unitx.w) }

      val unity = Vec4i.UnitY
      expect(classOf[ConstVec4i]) { unity.getClass }
      expect((0, 1, 0, 0)) { (unity.x, unity.y, unity.z, unity.w) }

      val unitz = Vec4i.UnitZ
      expect(classOf[ConstVec4i]) { unitz.getClass }
      expect((0, 0, 1, 0)) { (unitz.x, unitz.y, unitz.z, unitz.w) }

      val unitw = Vec4i.UnitW
      expect(classOf[ConstVec4i]) { unitw.getClass }
      expect((0, 0, 0, 1)) { (unitw.x, unitw.y, unitw.z, unitw.w) }

      val one = Vec4i.One
      expect(classOf[ConstVec4i]) { one.getClass }
      expect((1, 1, 1, 1)) { (one.x, one.y, one.z, one.w) }
    }
  }
}
