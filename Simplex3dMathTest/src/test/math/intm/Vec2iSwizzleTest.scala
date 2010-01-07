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

import simplex3d.math.intm._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Vec2iSwizzleTest extends FunSuite {

    test("Swizzled read") {
        val x = 5
        val y = 6

        val u = ConstVec2i(x, y)

        expect(x) { u.x }
        expect(y) { u.y }

        expect(x) { u.r }
        expect(y) { u.g }

        assert(Vec2i(x, x) == u.xx)
        assert(Vec2i(x, y) == u.xy)
        assert(Vec2i(y, x) == u.yx)
        assert(Vec2i(y, y) == u.yy)

        assert(Vec3i(x, x, x) == u.xxx)
        assert(Vec3i(x, x, y) == u.xxy)
        assert(Vec3i(x, y, x) == u.xyx)
        assert(Vec3i(x, y, y) == u.xyy)
        assert(Vec3i(y, x, x) == u.yxx)
        assert(Vec3i(y, x, y) == u.yxy)
        assert(Vec3i(y, y, x) == u.yyx)
        assert(Vec3i(y, y, y) == u.yyy)

        assert(Vec4i(x, x, x, x) == u.xxxx)
        assert(Vec4i(x, x, x, y) == u.xxxy)
        assert(Vec4i(x, x, y, x) == u.xxyx)
        assert(Vec4i(x, x, y, y) == u.xxyy)
        assert(Vec4i(x, y, x, x) == u.xyxx)
        assert(Vec4i(x, y, x, y) == u.xyxy)
        assert(Vec4i(x, y, y, x) == u.xyyx)
        assert(Vec4i(x, y, y, y) == u.xyyy)
        assert(Vec4i(y, x, x, x) == u.yxxx)
        assert(Vec4i(y, x, x, y) == u.yxxy)
        assert(Vec4i(y, x, y, x) == u.yxyx)
        assert(Vec4i(y, x, y, y) == u.yxyy)
        assert(Vec4i(y, y, x, x) == u.yyxx)
        assert(Vec4i(y, y, x, y) == u.yyxy)
        assert(Vec4i(y, y, y, x) == u.yyyx)
        assert(Vec4i(y, y, y, y) == u.yyyy)

        assert(Vec2i(x, x) == u.rr)
        assert(Vec2i(x, y) == u.rg)
        assert(Vec2i(y, x) == u.gr)
        assert(Vec2i(y, y) == u.gg)

        assert(Vec3i(x, x, x) == u.rrr)
        assert(Vec3i(x, x, y) == u.rrg)
        assert(Vec3i(x, y, x) == u.rgr)
        assert(Vec3i(x, y, y) == u.rgg)
        assert(Vec3i(y, x, x) == u.grr)
        assert(Vec3i(y, x, y) == u.grg)
        assert(Vec3i(y, y, x) == u.ggr)
        assert(Vec3i(y, y, y) == u.ggg)

        assert(Vec4i(x, x, x, x) == u.rrrr)
        assert(Vec4i(x, x, x, y) == u.rrrg)
        assert(Vec4i(x, x, y, x) == u.rrgr)
        assert(Vec4i(x, x, y, y) == u.rrgg)
        assert(Vec4i(x, y, x, x) == u.rgrr)
        assert(Vec4i(x, y, x, y) == u.rgrg)
        assert(Vec4i(x, y, y, x) == u.rggr)
        assert(Vec4i(x, y, y, y) == u.rggg)
        assert(Vec4i(y, x, x, x) == u.grrr)
        assert(Vec4i(y, x, x, y) == u.grrg)
        assert(Vec4i(y, x, y, x) == u.grgr)
        assert(Vec4i(y, x, y, y) == u.grgg)
        assert(Vec4i(y, y, x, x) == u.ggrr)
        assert(Vec4i(y, y, x, y) == u.ggrg)
        assert(Vec4i(y, y, y, x) == u.gggr)
        assert(Vec4i(y, y, y, y) == u.gggg)
    }

    test("Swizzled write") {
        val x = 5
        val y = 6
        val t = 10

        var i = ConstVec2i(x, y)
        val u = Vec2i(1)

        u := i; u.x = t; assert(Vec2i(t, y) == u)
        u := i; u.y = t; assert(Vec2i(x, t) == u)

        u := i; u.r = t; assert(Vec2i(t, y) == u)
        u := i; u.g = t; assert(Vec2i(x, t) == u)

        i = Vec2i(t)

        u := i; u.xy = Vec2i(x, y); assert(Vec2i(x, y) == u)
        u := i; u.yx = Vec2i(x, y); assert(Vec2i(y, x) == u)

        u := i; u.rg = Vec2i(x, y); assert(Vec2i(x, y) == u)
        u := i; u.gr = Vec2i(x, y); assert(Vec2i(y, x) == u)
    }

    test("Swizzled self write") {
        val x = 5
        val y = 6

        val i = ConstVec2i(x, y)
        val u = Vec2i(1)

        u := i; u.xy = u; assert(Vec2i(x, y) == u)
        u := i; u.yx = u; assert(Vec2i(y, x) == u)
    }
}
