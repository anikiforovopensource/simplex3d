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

package test.math.doublem

import org.scalatest._

import simplex3d.math.intm._
import simplex3d.math.floatm._


/**
 * @author Aleksey Nikiforov (lex)
 */
class PromotionsTest extends FunSuite {

    test("Promotions") {
        import simplex3d.math.doublem._

        val i2: Vec2d = Vec2i(1, 2)
        expect(classOf[Vec2d]) { i2.getClass }
        expect((1, 2)) { (i2.x, i2.y) }

        val i3: Vec3d = Vec3i(1, 2, 3)
        expect(classOf[Vec3d]) { i3.getClass }
        expect((1, 2, 3)) { (i3.x, i3.y, i3.z) }

        val i4: Vec4d = Vec4i(1, 2, 3, 4)
        expect(classOf[Vec4d]) { i4.getClass }
        expect((1, 2, 3, 4)) { (i4.x, i4.y, i4.z, i4.w) }

        val f2: Vec2d = Vec2f(1, 2)
        expect(classOf[Vec2d]) { f2.getClass }
        expect((1, 2)) { (f2.x, f2.y) }

        val f3: Vec3d = Vec3f(1, 2, 3)
        expect(classOf[Vec3d]) { f3.getClass }
        expect((1, 2, 3)) { (f3.x, f3.y, f3.z) }

        val f4: Vec4d = Vec4f(1, 2, 3, 4)
        expect(classOf[Vec4d]) { f4.getClass }
        expect((1, 2, 3, 4)) { (f4.x, f4.y, f4.z, f4.w) }

        val ci2: ConstVec2d = Vec2i(1, 2)
        expect(classOf[Vec2d]) { i2.getClass }
        expect((1, 2)) { (i2.x, i2.y) }

        val ci3: ConstVec3d = Vec3i(1, 2, 3)
        expect(classOf[Vec3d]) { i3.getClass }
        expect((1, 2, 3)) { (i3.x, i3.y, i3.z) }

        val ci4: ConstVec4d = Vec4i(1, 2, 3, 4)
        expect(classOf[Vec4d]) { i4.getClass }
        expect((1, 2, 3, 4)) { (i4.x, i4.y, i4.z, i4.w) }

        val cf2: ConstVec2d = Vec2f(1, 2)
        expect(classOf[Vec2d]) { f2.getClass }
        expect((1, 2)) { (f2.x, f2.y) }

        val cf3: ConstVec3d = Vec3f(1, 2, 3)
        expect(classOf[Vec3d]) { f3.getClass }
        expect((1, 2, 3)) { (f3.x, f3.y, f3.z) }

        val cf4: ConstVec4d = Vec4f(1, 2, 3, 4)
        expect(classOf[Vec4d]) { f4.getClass }
        expect((1, 2, 3, 4)) { (f4.x, f4.y, f4.z, f4.w) }
    }

    test("Renamed promotions") {
        import simplex3d.math.doublem.renamed._

        val i2: Vec2 = Vec2i(1, 2)
        expect(classOf[Vec2]) { i2.getClass }
        expect((1, 2)) { (i2.x, i2.y) }

        val i3: Vec3 = Vec3i(1, 2, 3)
        expect(classOf[Vec3]) { i3.getClass }
        expect((1, 2, 3)) { (i3.x, i3.y, i3.z) }

        val i4: Vec4 = Vec4i(1, 2, 3, 4)
        expect(classOf[Vec4]) { i4.getClass }
        expect((1, 2, 3, 4)) { (i4.x, i4.y, i4.z, i4.w) }

        val f2: Vec2 = Vec2f(1, 2)
        expect(classOf[Vec2]) { f2.getClass }
        expect((1, 2)) { (f2.x, f2.y) }

        val f3: Vec3 = Vec3f(1, 2, 3)
        expect(classOf[Vec3]) { f3.getClass }
        expect((1, 2, 3)) { (f3.x, f3.y, f3.z) }

        val f4: Vec4 = Vec4f(1, 2, 3, 4)
        expect(classOf[Vec4]) { f4.getClass }
        expect((1, 2, 3, 4)) { (f4.x, f4.y, f4.z, f4.w) }

        val ci2: ConstVec2 = Vec2i(1, 2)
        expect(classOf[Vec2]) { i2.getClass }
        expect((1, 2)) { (i2.x, i2.y) }

        val ci3: ConstVec3 = Vec3i(1, 2, 3)
        expect(classOf[Vec3]) { i3.getClass }
        expect((1, 2, 3)) { (i3.x, i3.y, i3.z) }

        val ci4: ConstVec4 = Vec4i(1, 2, 3, 4)
        expect(classOf[Vec4]) { i4.getClass }
        expect((1, 2, 3, 4)) { (i4.x, i4.y, i4.z, i4.w) }

        val cf2: ConstVec2 = Vec2f(1, 2)
        expect(classOf[Vec2]) { f2.getClass }
        expect((1, 2)) { (f2.x, f2.y) }

        val cf3: ConstVec3 = Vec3f(1, 2, 3)
        expect(classOf[Vec3]) { f3.getClass }
        expect((1, 2, 3)) { (f3.x, f3.y, f3.z) }

        val cf4: ConstVec4 = Vec4f(1, 2, 3, 4)
        expect(classOf[Vec4]) { f4.getClass }
        expect((1, 2, 3, 4)) { (f4.x, f4.y, f4.z, f4.w) }
    }
}
