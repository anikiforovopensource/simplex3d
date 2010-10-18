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
  }

  test("Manifest") {
    import scala.reflect.Manifest._

    assert(Vec2b.Manifest == classType[Vec2b](classOf[Vec2b]))
    assert(Vec2b.ConstManifest == classType[ConstVec2b](classOf[ConstVec2b]))
    assert(Vec2b.ReadManifest == classType[ReadVec2b](classOf[ReadVec2b]))

    assert(Vec3b.Manifest == classType[Vec3b](classOf[Vec3b]))
    assert(Vec3b.ConstManifest == classType[ConstVec3b](classOf[ConstVec3b]))
    assert(Vec3b.ReadManifest == classType[ReadVec3b](classOf[ReadVec3b]))

    assert(Vec4b.Manifest == classType[Vec4b](classOf[Vec4b]))
    assert(Vec4b.ConstManifest == classType[ConstVec4b](classOf[ConstVec4b]))
    assert(Vec4b.ReadManifest == classType[ReadVec4b](classOf[ReadVec4b]))
  }
}
