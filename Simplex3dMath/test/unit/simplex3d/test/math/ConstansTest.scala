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

package simplex3d.test.math

import org.scalatest._

import simplex3d.math._


/**
 * @author Aleksey Nikiforov (lex)
 */
class ConstansTest extends FunSuite {

  test("Constants") {
    {
      val f = Vec2b.False
      expectResult(classOf[ConstVec2b]) { f.getClass }
      expectResult((false, false)) { (f.x, f.y) }

      val t = Vec2b.True
      expectResult(classOf[ConstVec2b]) { t.getClass }
      expectResult((true, true)) { (t.x, t.y) }
    }
    {
      val f = Vec3b.False
      expectResult(classOf[ConstVec3b]) { f.getClass }
      expectResult((false, false, false)) { (f.x, f.y, f.z) }

      val t = Vec3b.True
      expectResult(classOf[ConstVec3b]) { t.getClass }
      expectResult((true, true, true)) { (t.x, t.y, t.z) }
    }
    {
      val f = Vec4b.False
      expectResult(classOf[ConstVec4b]) { f.getClass }
      expectResult((false, false, false, false)) { (f.x, f.y, f.z, f.w) }

      val t = Vec4b.True
      expectResult(classOf[ConstVec4b]) { t.getClass }
      expectResult((true, true, true, true)) { (t.x, t.y, t.z, t.w) }
    }


    {
      val zero = Vec2i.Zero
      expectResult(classOf[ConstVec2i]) { zero.getClass }
      expectResult((0, 0)) { (zero.x, zero.y) }

      val unitx = Vec2i.UnitX
      expectResult(classOf[ConstVec2i]) { unitx.getClass }
      expectResult((1, 0)) { (unitx.x, unitx.y) }

      val unity = Vec2i.UnitY
      expectResult(classOf[ConstVec2i]) { unity.getClass }
      expectResult((0, 1)) { (unity.x, unity.y) }

      val one = Vec2i.One
      expectResult(classOf[ConstVec2i]) { one.getClass }
      expectResult((1, 1)) { (one.x, one.y) }
    }
    {
      val zero = Vec3i.Zero
      expectResult(classOf[ConstVec3i]) { zero.getClass }
      expectResult((0, 0, 0)) { (zero.x, zero.y, zero.z) }

      val unitx = Vec3i.UnitX
      expectResult(classOf[ConstVec3i]) { unitx.getClass }
      expectResult((1, 0, 0)) { (unitx.x, unitx.y, unitx.z) }

      val unity = Vec3i.UnitY
      expectResult(classOf[ConstVec3i]) { unity.getClass }
      expectResult((0, 1, 0)) { (unity.x, unity.y, unity.z) }

      val unitz = Vec3i.UnitZ
      expectResult(classOf[ConstVec3i]) { unitz.getClass }
      expectResult((0, 0, 1)) { (unitz.x, unitz.y, unitz.z) }

      val one = Vec3i.One
      expectResult(classOf[ConstVec3i]) { one.getClass }
      expectResult((1, 1, 1)) { (one.x, one.y, one.z) }
    }
    {
      val zero = Vec4i.Zero
      expectResult(classOf[ConstVec4i]) { zero.getClass }
      expectResult((0, 0, 0, 0)) { (zero.x, zero.y, zero.z, zero.w) }

      val unitx = Vec4i.UnitX
      expectResult(classOf[ConstVec4i]) { unitx.getClass }
      expectResult((1, 0, 0, 0)) { (unitx.x, unitx.y, unitx.z, unitx.w) }

      val unity = Vec4i.UnitY
      expectResult(classOf[ConstVec4i]) { unity.getClass }
      expectResult((0, 1, 0, 0)) { (unity.x, unity.y, unity.z, unity.w) }

      val unitz = Vec4i.UnitZ
      expectResult(classOf[ConstVec4i]) { unitz.getClass }
      expectResult((0, 0, 1, 0)) { (unitz.x, unitz.y, unitz.z, unitz.w) }

      val unitw = Vec4i.UnitW
      expectResult(classOf[ConstVec4i]) { unitw.getClass }
      expectResult((0, 0, 0, 1)) { (unitw.x, unitw.y, unitw.z, unitw.w) }

      val one = Vec4i.One
      expectResult(classOf[ConstVec4i]) { one.getClass }
      expectResult((1, 1, 1, 1)) { (one.x, one.y, one.z, one.w) }
    }
  }

  test("Tag") {
    import scala.reflect._

    assert(Vec2b.Tag == classTag[Vec2b])
    assert(Vec2b.ConstTag == classTag[ConstVec2b])
    assert(Vec2b.ReadTag == classTag[ReadVec2b])

    assert(Vec3b.Tag == classTag[Vec3b])
    assert(Vec3b.ConstTag == classTag[ConstVec3b])
    assert(Vec3b.ReadTag == classTag[ReadVec3b])

    assert(Vec4b.Tag == classTag[Vec4b])
    assert(Vec4b.ConstTag == classTag[ConstVec4b])
    assert(Vec4b.ReadTag == classTag[ReadVec4b])


    assert(Vec2i.Tag == classTag[Vec2i])
    assert(Vec2i.ConstTag == classTag[ConstVec2i])
    assert(Vec2i.ReadTag == classTag[ReadVec2i])

    assert(Vec3i.Tag == classTag[Vec3i])
    assert(Vec3i.ConstTag == classTag[ConstVec3i])
    assert(Vec3i.ReadTag == classTag[ReadVec3i])

    assert(Vec4i.Tag == classTag[Vec4i])
    assert(Vec4i.ConstTag == classTag[ConstVec4i])
    assert(Vec4i.ReadTag == classTag[ReadVec4i])
  }
}
