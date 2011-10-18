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

package test.math.floatx

import org.scalatest._

import simplex3d.math._


/**
 * @author Aleksey Nikiforov (lex)
 */
class PromotionsTest extends FunSuite {

  test("Promotions") {
    import simplex3d.math.floatx._

    val ci2: ConstVec2f = Vec2i(1, 2)
    expect(classOf[ConstVec2f]) { ci2.getClass }
    expect((1, 2)) { (ci2.x, ci2.y) }

    val ci3: ConstVec3f = Vec3i(1, 2, 3)
    expect(classOf[ConstVec3f]) { ci3.getClass }
    expect((1, 2, 3)) { (ci3.x, ci3.y, ci3.z) }

    val ci4: ConstVec4f = Vec4i(1, 2, 3, 4)
    expect(classOf[ConstVec4f]) { ci4.getClass }
    expect((1, 2, 3, 4)) { (ci4.x, ci4.y, ci4.z, ci4.w) }
  }

  test("Renamed promotions") {
    import simplex3d.math.float._

    val ci2: ConstVec2 = Vec2i(1, 2)
    expect(classOf[ConstVec2]) { ci2.getClass }
    expect((1, 2)) { (ci2.x, ci2.y) }

    val ci3: ConstVec3 = Vec3i(1, 2, 3)
    expect(classOf[ConstVec3]) { ci3.getClass }
    expect((1, 2, 3)) { (ci3.x, ci3.y, ci3.z) }

    val ci4: ConstVec4 = Vec4i(1, 2, 3, 4)
    expect(classOf[ConstVec4]) { ci4.getClass }
    expect((1, 2, 3, 4)) { (ci4.x, ci4.y, ci4.z, ci4.w) }
  }
}
