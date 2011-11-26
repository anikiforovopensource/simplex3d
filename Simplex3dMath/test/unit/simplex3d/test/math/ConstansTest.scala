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
      expect(classOf[ConstVec2b]) { f.getClass }
      expect((false, false)) { (f.x, f.y) }

      val t = Vec2b.True
      expect(classOf[ConstVec2b]) { t.getClass }
      expect((true, true)) { (t.x, t.y) }
    }
    {
      val f = Vec3b.False
      expect(classOf[ConstVec3b]) { f.getClass }
      expect((false, false, false)) { (f.x, f.y, f.z) }

      val t = Vec3b.True
      expect(classOf[ConstVec3b]) { t.getClass }
      expect((true, true, true)) { (t.x, t.y, t.z) }
    }
    {
      val f = Vec4b.False
      expect(classOf[ConstVec4b]) { f.getClass }
      expect((false, false, false, false)) { (f.x, f.y, f.z, f.w) }

      val t = Vec4b.True
      expect(classOf[ConstVec4b]) { t.getClass }
      expect((true, true, true, true)) { (t.x, t.y, t.z, t.w) }
    }


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

  test("Manifest") {
    import scala.reflect.ClassManifest._

    assert(Vec2b.Manifest == classType[Vec2b](classOf[Vec2b]))
    assert(Vec2b.ConstManifest == classType[ConstVec2b](classOf[ConstVec2b]))
    assert(Vec2b.ReadManifest == classType[ReadVec2b](classOf[ReadVec2b]))

    assert(Vec3b.Manifest == classType[Vec3b](classOf[Vec3b]))
    assert(Vec3b.ConstManifest == classType[ConstVec3b](classOf[ConstVec3b]))
    assert(Vec3b.ReadManifest == classType[ReadVec3b](classOf[ReadVec3b]))

    assert(Vec4b.Manifest == classType[Vec4b](classOf[Vec4b]))
    assert(Vec4b.ConstManifest == classType[ConstVec4b](classOf[ConstVec4b]))
    assert(Vec4b.ReadManifest == classType[ReadVec4b](classOf[ReadVec4b]))


    assert(Vec2i.Manifest == classType[Vec2i](classOf[Vec2i]))
    assert(Vec2i.ConstManifest == classType[ConstVec2i](classOf[ConstVec2i]))
    assert(Vec2i.ReadManifest == classType[ReadVec2i](classOf[ReadVec2i]))

    assert(Vec3i.Manifest == classType[Vec3i](classOf[Vec3i]))
    assert(Vec3i.ConstManifest == classType[ConstVec3i](classOf[ConstVec3i]))
    assert(Vec3i.ReadManifest == classType[ReadVec3i](classOf[ReadVec3i]))

    assert(Vec4i.Manifest == classType[Vec4i](classOf[Vec4i]))
    assert(Vec4i.ConstManifest == classType[ConstVec4i](classOf[ConstVec4i]))
    assert(Vec4i.ReadManifest == classType[ReadVec4i](classOf[ReadVec4i]))
  }
}
