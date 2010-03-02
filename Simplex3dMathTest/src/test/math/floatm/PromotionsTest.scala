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

package test.math.floatm

import org.scalatest._

import simplex3d.math.intm._


/**
 * @author Aleksey Nikiforov (lex)
 */
class PromotionsTest extends FunSuite {

    test("Promotions") {
        import simplex3d.math.floatm._

        val i2: Vec2f = Vec2i(1, 2)
        expect(classOf[Vec2f]) { i2.getClass }
        expect((1, 2)) { (i2.x, i2.y) }

        val i3: Vec3f = Vec3i(1, 2, 3)
        expect(classOf[Vec3f]) { i3.getClass }
        expect((1, 2, 3)) { (i3.x, i3.y, i3.z) }

        val i4: Vec4f = Vec4i(1, 2, 3, 4)
        expect(classOf[Vec4f]) { i4.getClass }
        expect((1, 2, 3, 4)) { (i4.x, i4.y, i4.z, i4.w) }

        val ci2: ConstVec2f = Vec2i(1, 2)
        expect(classOf[Vec2f]) { i2.getClass }
        expect((1, 2)) { (i2.x, i2.y) }

        val ci3: ConstVec3f = Vec3i(1, 2, 3)
        expect(classOf[Vec3f]) { i3.getClass }
        expect((1, 2, 3)) { (i3.x, i3.y, i3.z) }

        val ci4: ConstVec4f = Vec4i(1, 2, 3, 4)
        expect(classOf[Vec4f]) { i4.getClass }
        expect((1, 2, 3, 4)) { (i4.x, i4.y, i4.z, i4.w) }
    }

    test("Renamed promotions") {
        import simplex3d.math.floatm.renamed._

        val i2: Vec2 = Vec2i(1, 2)
        expect(classOf[Vec2]) { i2.getClass }
        expect((1, 2)) { (i2.x, i2.y) }

        val i3: Vec3 = Vec3i(1, 2, 3)
        expect(classOf[Vec3]) { i3.getClass }
        expect((1, 2, 3)) { (i3.x, i3.y, i3.z) }

        val i4: Vec4 = Vec4i(1, 2, 3, 4)
        expect(classOf[Vec4]) { i4.getClass }
        expect((1, 2, 3, 4)) { (i4.x, i4.y, i4.z, i4.w) }

        val ci2: ConstVec2 = Vec2i(1, 2)
        expect(classOf[Vec2]) { i2.getClass }
        expect((1, 2)) { (i2.x, i2.y) }

        val ci3: ConstVec3 = Vec3i(1, 2, 3)
        expect(classOf[Vec3]) { i3.getClass }
        expect((1, 2, 3)) { (i3.x, i3.y, i3.z) }

        val ci4: ConstVec4 = Vec4i(1, 2, 3, 4)
        expect(classOf[Vec4]) { i4.getClass }
        expect((1, 2, 3, 4)) { (i4.x, i4.y, i4.z, i4.w) }
    }
}
