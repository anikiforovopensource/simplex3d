/*
 * Simplex3D, Math tests
 * Copyright (C) 2009 Simplex3D team
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package test.math

import org.scalatest._

import simplex3d.math.floatm.renamed._
import simplex3d.math.floatm.FloatMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Vec2fSwizzleTest extends FunSuite {

    test("Swizzled read") {
        val x = 5f
        val y = 6f

        val u = ConstVec2(x, y)

        expect(x) { u.x }
        expect(y) { u.y }

        expect(x) { u.r }
        expect(y) { u.g }

        assert(Vec2(x, x) == u.xx)
        assert(Vec2(x, y) == u.xy)
        assert(Vec2(y, x) == u.yx)
        assert(Vec2(y, y) == u.yy)

        assert(Vec3(x, x, x) == u.xxx)
        assert(Vec3(x, x, y) == u.xxy)
        assert(Vec3(x, y, x) == u.xyx)
        assert(Vec3(x, y, y) == u.xyy)
        assert(Vec3(y, x, x) == u.yxx)
        assert(Vec3(y, x, y) == u.yxy)
        assert(Vec3(y, y, x) == u.yyx)
        assert(Vec3(y, y, y) == u.yyy)

        assert(Vec4(x, x, x, x) == u.xxxx)
        assert(Vec4(x, x, x, y) == u.xxxy)
        assert(Vec4(x, x, y, x) == u.xxyx)
        assert(Vec4(x, x, y, y) == u.xxyy)
        assert(Vec4(x, y, x, x) == u.xyxx)
        assert(Vec4(x, y, x, y) == u.xyxy)
        assert(Vec4(x, y, y, x) == u.xyyx)
        assert(Vec4(x, y, y, y) == u.xyyy)
        assert(Vec4(y, x, x, x) == u.yxxx)
        assert(Vec4(y, x, x, y) == u.yxxy)
        assert(Vec4(y, x, y, x) == u.yxyx)
        assert(Vec4(y, x, y, y) == u.yxyy)
        assert(Vec4(y, y, x, x) == u.yyxx)
        assert(Vec4(y, y, x, y) == u.yyxy)
        assert(Vec4(y, y, y, x) == u.yyyx)
        assert(Vec4(y, y, y, y) == u.yyyy)

        assert(Vec2(x, x) == u.rr)
        assert(Vec2(x, y) == u.rg)
        assert(Vec2(y, x) == u.gr)
        assert(Vec2(y, y) == u.gg)

        assert(Vec3(x, x, x) == u.rrr)
        assert(Vec3(x, x, y) == u.rrg)
        assert(Vec3(x, y, x) == u.rgr)
        assert(Vec3(x, y, y) == u.rgg)
        assert(Vec3(y, x, x) == u.grr)
        assert(Vec3(y, x, y) == u.grg)
        assert(Vec3(y, y, x) == u.ggr)
        assert(Vec3(y, y, y) == u.ggg)

        assert(Vec4(x, x, x, x) == u.rrrr)
        assert(Vec4(x, x, x, y) == u.rrrg)
        assert(Vec4(x, x, y, x) == u.rrgr)
        assert(Vec4(x, x, y, y) == u.rrgg)
        assert(Vec4(x, y, x, x) == u.rgrr)
        assert(Vec4(x, y, x, y) == u.rgrg)
        assert(Vec4(x, y, y, x) == u.rggr)
        assert(Vec4(x, y, y, y) == u.rggg)
        assert(Vec4(y, x, x, x) == u.grrr)
        assert(Vec4(y, x, x, y) == u.grrg)
        assert(Vec4(y, x, y, x) == u.grgr)
        assert(Vec4(y, x, y, y) == u.grgg)
        assert(Vec4(y, y, x, x) == u.ggrr)
        assert(Vec4(y, y, x, y) == u.ggrg)
        assert(Vec4(y, y, y, x) == u.gggr)
        assert(Vec4(y, y, y, y) == u.gggg)
    }

    test("Swizzled write") {
        val x = 5f
        val y = 6f
        val t = 10f

        var i = ConstVec2(x, y)
        val u = Vec2(1)

        u := i; u.x = t; assert(Vec2(t, y) == u)
        u := i; u.y = t; assert(Vec2(x, t) == u)

        u := i; u.r = t; assert(Vec2(t, y) == u)
        u := i; u.g = t; assert(Vec2(x, t) == u)

        i = Vec2(t)

        u := i; u.xy = Vec2(x, y); assert(Vec2(x, y) == u)
        u := i; u.yx = Vec2(x, y); assert(Vec2(y, x) == u)

        u := i; u.rg = Vec2(x, y); assert(Vec2(x, y) == u)
        u := i; u.gr = Vec2(x, y); assert(Vec2(y, x) == u)
    }

    test("Swizzled self write") {
        val x = 5f
        val y = 6f

        val i = ConstVec2(x, y)
        val u = Vec2(1)

        u := i; u.xy = u.xx; assert(Vec2(x, x) == u)

        pending
    }
}
