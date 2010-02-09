/*
 * Simplex3d, MathTest package
 * Copyright (C) 2009-2010 Simplex3d Team
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


/**
 * @author Aleksey Nikiforov (lex)
 */
class PackageTest extends FunSuite {

    test("Package") {
        import simplex3d.math.intm._

        // implicits
        val i2 = 2*Vec2i(1)
        assert(i2.isInstanceOf[Vec2i])

        // the following passes if it compiles
        def abs(x: Int) = IntMath.abs(x)

        type av2 = AnyVec2i
        type cv2 = ConstVec2i
        val cv2 = ConstVec2i
        type v2 = Vec2i
        val v2 = Vec2i

        type av3 = AnyVec3i
        type cv3 = ConstVec3i
        val cv3 = ConstVec3i
        type v3 = Vec3i
        val v3 = Vec3i

        type av4 = AnyVec4i
        type cv4 = ConstVec4i
        val cv4 = ConstVec4i
        type v4 = Vec4i
        val v4 = Vec4i
    }
}
