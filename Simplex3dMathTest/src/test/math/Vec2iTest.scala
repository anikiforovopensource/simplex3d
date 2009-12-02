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

import simplex3d.math._
import simplex3d.math.VecMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Vec2iTest extends FunSuite {

    test("Const factories") {
        var u = ConstVec2i(5)
        expect(classOf[ConstVec2i]) { u.getClass }
        expect(5) { u.x }
        expect(5) { u.y }
        
        u = ConstVec2i(2, 3)
        expect(classOf[ConstVec2i]) { u.getClass }
        expect(2) { u.x }
        expect(3) { u.y }

        u = ConstVec2i(Vec2(4, 5))
        expect(classOf[ConstVec2i]) { u.getClass }
        expect(4) { u.x }
        expect(5) { u.y }

        u = ConstVec2i(Vec3(6, 7, 8))
        expect(classOf[ConstVec2i]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }

        u = ConstVec2i(Vec4(1, 2, 3, 4))
        expect(classOf[ConstVec2i]) { u.getClass }
        expect(1) { u.x }
        expect(2) { u.y }

        u = ConstVec2i(Vec2i(4, 5))
        expect(classOf[ConstVec2i]) { u.getClass }
        expect(4) { u.x }
        expect(5) { u.y }

        u = ConstVec2i(Vec3i(6, 7, 8))
        expect(classOf[ConstVec2i]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }

        u = ConstVec2i(Vec4i(1, 2, 3, 4))
        expect(classOf[ConstVec2i]) { u.getClass }
        expect(1) { u.x }
        expect(2) { u.y }
    }

    test("Mutable factories") {
        var u = Vec2i(5)
        expect(classOf[Vec2i]) { u.getClass }
        expect(5) { u.x }
        expect(5) { u.y }

        u = Vec2i(2, 3)
        expect(classOf[Vec2i]) { u.getClass }
        expect(2) { u.x }
        expect(3) { u.y }

        u = Vec2i(Vec2i(4, 5))
        expect(classOf[Vec2i]) { u.getClass }
        expect(4) { u.x }
        expect(5) { u.y }

        u = Vec2i(Vec3(6, 7, 8))
        expect(classOf[Vec2i]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }

        u = Vec2i(Vec4(1, 2, 3, 4))
        expect(classOf[Vec2i]) { u.getClass }
        expect(1) { u.x }
        expect(2) { u.y }

        u = Vec2i(Vec2i(4, 5))
        expect(classOf[Vec2i]) { u.getClass }
        expect(4) { u.x }
        expect(5) { u.y }

        u = Vec2i(Vec3i(6, 7, 8))
        expect(classOf[Vec2i]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }

        u = Vec2i(Vec4i(1, 2, 3, 4))
        expect(classOf[Vec2i]) { u.getClass }
        expect(1) { u.x }
        expect(2) { u.y }
    }

    test("Equality methods") {
        assert(Vec2i(4, 7) == ConstVec2i(4, 7))
        assert(ConstVec2i(4, 7) == Vec2i(4, 7))

        assert(Vec2i(1, 2) != Vec2i(9, 2))
        assert(Vec2i(1, 2) != Vec2i(1, 9))
    }

    test("Indexed read") {
        val u = ConstVec2i(3, 4)

        expect(3) { u(0) }
        expect(4) { u(1) }

        intercept[IndexOutOfBoundsException] {
            u(2)
        }
        intercept[IndexOutOfBoundsException] {
            u(-1)
        }
    }

    test("Indexed write") {
        val u = Vec2i(3, 4)

        u(0) = 5
        assert(Vec2i(5, 4) == u)

        u(1) = 6
        assert(Vec2i(5, 6) == u)

        intercept[IndexOutOfBoundsException] {
            u(2) = 1
        }
        intercept[IndexOutOfBoundsException] {
            u(-1) = 1
        }
    }

    test("Swizzled read") {
        val u = ConstVec2i(5, 6)

        expect(5) { u.x }
        expect(6) { u.y }

        expect(5) { u.r }
        expect(6) { u.g }

        expect(5) { u.s }
        expect(6) { u.t }

        assert(Vec2i(5, 5) == u.xx)
        assert(Vec2i(5, 6) == u.xy)
        assert(Vec2i(6, 5) == u.yx)
        assert(Vec2i(6, 6) == u.yy)

        assert(Vec3i(5, 5, 5) == u.xxx)
        assert(Vec3i(5, 5, 6) == u.xxy)
        assert(Vec3i(5, 6, 5) == u.xyx)
        assert(Vec3i(5, 6, 6) == u.xyy)
        assert(Vec3i(6, 5, 5) == u.yxx)
        assert(Vec3i(6, 5, 6) == u.yxy)
        assert(Vec3i(6, 6, 5) == u.yyx)
        assert(Vec3i(6, 6, 6) == u.yyy)

        assert(Vec4i(5, 5, 5, 5) == u.xxxx)
        assert(Vec4i(5, 5, 5, 6) == u.xxxy)
        assert(Vec4i(5, 5, 6, 5) == u.xxyx)
        assert(Vec4i(5, 5, 6, 6) == u.xxyy)
        assert(Vec4i(5, 6, 5, 5) == u.xyxx)
        assert(Vec4i(5, 6, 5, 6) == u.xyxy)
        assert(Vec4i(5, 6, 6, 5) == u.xyyx)
        assert(Vec4i(5, 6, 6, 6) == u.xyyy)
        assert(Vec4i(6, 5, 5, 5) == u.yxxx)
        assert(Vec4i(6, 5, 5, 6) == u.yxxy)
        assert(Vec4i(6, 5, 6, 5) == u.yxyx)
        assert(Vec4i(6, 5, 6, 6) == u.yxyy)
        assert(Vec4i(6, 6, 5, 5) == u.yyxx)
        assert(Vec4i(6, 6, 5, 6) == u.yyxy)
        assert(Vec4i(6, 6, 6, 5) == u.yyyx)
        assert(Vec4i(6, 6, 6, 6) == u.yyyy)

        assert(Vec2i(5, 5) == u.rr)
        assert(Vec2i(5, 6) == u.rg)
        assert(Vec2i(6, 5) == u.gr)
        assert(Vec2i(6, 6) == u.gg)

        assert(Vec3i(5, 5, 5) == u.rrr)
        assert(Vec3i(5, 5, 6) == u.rrg)
        assert(Vec3i(5, 6, 5) == u.rgr)
        assert(Vec3i(5, 6, 6) == u.rgg)
        assert(Vec3i(6, 5, 5) == u.grr)
        assert(Vec3i(6, 5, 6) == u.grg)
        assert(Vec3i(6, 6, 5) == u.ggr)
        assert(Vec3i(6, 6, 6) == u.ggg)

        assert(Vec4i(5, 5, 5, 5) == u.rrrr)
        assert(Vec4i(5, 5, 5, 6) == u.rrrg)
        assert(Vec4i(5, 5, 6, 5) == u.rrgr)
        assert(Vec4i(5, 5, 6, 6) == u.rrgg)
        assert(Vec4i(5, 6, 5, 5) == u.rgrr)
        assert(Vec4i(5, 6, 5, 6) == u.rgrg)
        assert(Vec4i(5, 6, 6, 5) == u.rggr)
        assert(Vec4i(5, 6, 6, 6) == u.rggg)
        assert(Vec4i(6, 5, 5, 5) == u.grrr)
        assert(Vec4i(6, 5, 5, 6) == u.grrg)
        assert(Vec4i(6, 5, 6, 5) == u.grgr)
        assert(Vec4i(6, 5, 6, 6) == u.grgg)
        assert(Vec4i(6, 6, 5, 5) == u.ggrr)
        assert(Vec4i(6, 6, 5, 6) == u.ggrg)
        assert(Vec4i(6, 6, 6, 5) == u.gggr)
        assert(Vec4i(6, 6, 6, 6) == u.gggg)

        assert(Vec2i(5, 5) == u.ss)
        assert(Vec2i(5, 6) == u.st)
        assert(Vec2i(6, 5) == u.ts)
        assert(Vec2i(6, 6) == u.tt)

        assert(Vec3i(5, 5, 5) == u.sss)
        assert(Vec3i(5, 5, 6) == u.sst)
        assert(Vec3i(5, 6, 5) == u.sts)
        assert(Vec3i(5, 6, 6) == u.stt)
        assert(Vec3i(6, 5, 5) == u.tss)
        assert(Vec3i(6, 5, 6) == u.tst)
        assert(Vec3i(6, 6, 5) == u.tts)
        assert(Vec3i(6, 6, 6) == u.ttt)

        assert(Vec4i(5, 5, 5, 5) == u.ssss)
        assert(Vec4i(5, 5, 5, 6) == u.ssst)
        assert(Vec4i(5, 5, 6, 5) == u.ssts)
        assert(Vec4i(5, 5, 6, 6) == u.sstt)
        assert(Vec4i(5, 6, 5, 5) == u.stss)
        assert(Vec4i(5, 6, 5, 6) == u.stst)
        assert(Vec4i(5, 6, 6, 5) == u.stts)
        assert(Vec4i(5, 6, 6, 6) == u.sttt)
        assert(Vec4i(6, 5, 5, 5) == u.tsss)
        assert(Vec4i(6, 5, 5, 6) == u.tsst)
        assert(Vec4i(6, 5, 6, 5) == u.tsts)
        assert(Vec4i(6, 5, 6, 6) == u.tstt)
        assert(Vec4i(6, 6, 5, 5) == u.ttss)
        assert(Vec4i(6, 6, 5, 6) == u.ttst)
        assert(Vec4i(6, 6, 6, 5) == u.ttts)
        assert(Vec4i(6, 6, 6, 6) == u.tttt)
    }

    test("Swizzled write") {
        var u = Vec2i(1)

        u = Vec2i(1); u.x = 5; assert(Vec2i(5, 1) == u)
        u = Vec2i(1); u.y = 5; assert(Vec2i(1, 5) == u)

        u = Vec2i(1); u.r = 5; assert(Vec2i(5, 1) == u)
        u = Vec2i(1); u.g = 5; assert(Vec2i(1, 5) == u)

        u = Vec2i(1); u.s = 5; assert(Vec2i(5, 1) == u)
        u = Vec2i(1); u.t = 5; assert(Vec2i(1, 5) == u)

        u = Vec2i(1); u.xy = Vec2i(5, 6); assert(Vec2i(5, 6) == u)
        u = Vec2i(1); u.yx = Vec2i(7, 8); assert(Vec2i(8, 7) == u)

        u = Vec2i(1); u.rg = Vec2i(5, 6); assert(Vec2i(5, 6) == u)
        u = Vec2i(1); u.gr = Vec2i(7, 8); assert(Vec2i(8, 7) == u)

        u = Vec2i(1); u.st = Vec2i(5, 6); assert(Vec2i(5, 6) == u)
        u = Vec2i(1); u.ts = Vec2i(7, 8); assert(Vec2i(8, 7) == u)
    }

    test("Const math") {
        pending
    }

    test("Mutable math") {
        pending
    }
}
