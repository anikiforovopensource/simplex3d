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
class Vec2Test extends FunSuite {

    test("Const factories") {
        var u = ConstVec2(5)
        expect(classOf[ConstVec2]) { u.getClass }
        expect(5) { u.x }
        expect(5) { u.y }
        
        u = ConstVec2(2, 3)
        expect(classOf[ConstVec2]) { u.getClass }
        expect(2) { u.x }
        expect(3) { u.y }

        u = ConstVec2(Vec2(4, 5))
        expect(classOf[ConstVec2]) { u.getClass }
        expect(4) { u.x }
        expect(5) { u.y }

        u = ConstVec2(Vec3(6, 7, 8))
        expect(classOf[ConstVec2]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }

        u = ConstVec2(Vec4(1, 2, 3, 4))
        expect(classOf[ConstVec2]) { u.getClass }
        expect(1) { u.x }
        expect(2) { u.y }

        u = ConstVec2(Vec2i(4, 5))
        expect(classOf[ConstVec2]) { u.getClass }
        expect(4) { u.x }
        expect(5) { u.y }

        u = ConstVec2(Vec3i(6, 7, 8))
        expect(classOf[ConstVec2]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }

        u = ConstVec2(Vec4i(1, 2, 3, 4))
        expect(classOf[ConstVec2]) { u.getClass }
        expect(1) { u.x }
        expect(2) { u.y }
    }

    test("Mutable factories") {
        var u = Vec2(5)
        expect(classOf[Vec2]) { u.getClass }
        expect(5) { u.x }
        expect(5) { u.y }

        u = Vec2(2, 3)
        expect(classOf[Vec2]) { u.getClass }
        expect(2) { u.x }
        expect(3) { u.y }

        u = Vec2(Vec2(4, 5))
        expect(classOf[Vec2]) { u.getClass }
        expect(4) { u.x }
        expect(5) { u.y }

        u = Vec2(Vec3(6, 7, 8))
        expect(classOf[Vec2]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }

        u = Vec2(Vec4(1, 2, 3, 4))
        expect(classOf[Vec2]) { u.getClass }
        expect(1) { u.x }
        expect(2) { u.y }

        u = Vec2(Vec2i(4, 5))
        expect(classOf[Vec2]) { u.getClass }
        expect(4) { u.x }
        expect(5) { u.y }

        u = Vec2(Vec3i(6, 7, 8))
        expect(classOf[Vec2]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }

        u = Vec2(Vec4i(1, 2, 3, 4))
        expect(classOf[Vec2]) { u.getClass }
        expect(1) { u.x }
        expect(2) { u.y }
    }

    test("Equality methods") {
        assert(Vec2(4, 7) == ConstVec2(4, 7))
        assert(ConstVec2(4, 7) == Vec2(4, 7))

        assert(Vec2(1, 2) != Vec2(9, 2))
        assert(Vec2(1, 2) != Vec2(1, 9))
    }

    test("Indexed read") {
        val u = ConstVec2(3, 4)

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
        val u = Vec2(3, 4)

        u(0) = 5
        assert(Vec2(5, 4) == u)

        u(1) = 6
        assert(Vec2(5, 6) == u)

        intercept[IndexOutOfBoundsException] {
            u(2) = 1
        }
        intercept[IndexOutOfBoundsException] {
            u(-1) = 1
        }
    }

    test("Swizzled read") {
        val u = ConstVec2(5, 6)

        expect(5) { u.x }
        expect(6) { u.y }

        expect(5) { u.r }
        expect(6) { u.g }

        expect(5) { u.s }
        expect(6) { u.t }

        assert(Vec2(5, 5) == u.xx)
        assert(Vec2(5, 6) == u.xy)
        assert(Vec2(6, 5) == u.yx)
        assert(Vec2(6, 6) == u.yy)

        assert(Vec3(5, 5, 5) == u.xxx)
        assert(Vec3(5, 5, 6) == u.xxy)
        assert(Vec3(5, 6, 5) == u.xyx)
        assert(Vec3(5, 6, 6) == u.xyy)
        assert(Vec3(6, 5, 5) == u.yxx)
        assert(Vec3(6, 5, 6) == u.yxy)
        assert(Vec3(6, 6, 5) == u.yyx)
        assert(Vec3(6, 6, 6) == u.yyy)

        assert(Vec4(5, 5, 5, 5) == u.xxxx)
        assert(Vec4(5, 5, 5, 6) == u.xxxy)
        assert(Vec4(5, 5, 6, 5) == u.xxyx)
        assert(Vec4(5, 5, 6, 6) == u.xxyy)
        assert(Vec4(5, 6, 5, 5) == u.xyxx)
        assert(Vec4(5, 6, 5, 6) == u.xyxy)
        assert(Vec4(5, 6, 6, 5) == u.xyyx)
        assert(Vec4(5, 6, 6, 6) == u.xyyy)
        assert(Vec4(6, 5, 5, 5) == u.yxxx)
        assert(Vec4(6, 5, 5, 6) == u.yxxy)
        assert(Vec4(6, 5, 6, 5) == u.yxyx)
        assert(Vec4(6, 5, 6, 6) == u.yxyy)
        assert(Vec4(6, 6, 5, 5) == u.yyxx)
        assert(Vec4(6, 6, 5, 6) == u.yyxy)
        assert(Vec4(6, 6, 6, 5) == u.yyyx)
        assert(Vec4(6, 6, 6, 6) == u.yyyy)

        assert(Vec2(5, 5) == u.rr)
        assert(Vec2(5, 6) == u.rg)
        assert(Vec2(6, 5) == u.gr)
        assert(Vec2(6, 6) == u.gg)

        assert(Vec3(5, 5, 5) == u.rrr)
        assert(Vec3(5, 5, 6) == u.rrg)
        assert(Vec3(5, 6, 5) == u.rgr)
        assert(Vec3(5, 6, 6) == u.rgg)
        assert(Vec3(6, 5, 5) == u.grr)
        assert(Vec3(6, 5, 6) == u.grg)
        assert(Vec3(6, 6, 5) == u.ggr)
        assert(Vec3(6, 6, 6) == u.ggg)

        assert(Vec4(5, 5, 5, 5) == u.rrrr)
        assert(Vec4(5, 5, 5, 6) == u.rrrg)
        assert(Vec4(5, 5, 6, 5) == u.rrgr)
        assert(Vec4(5, 5, 6, 6) == u.rrgg)
        assert(Vec4(5, 6, 5, 5) == u.rgrr)
        assert(Vec4(5, 6, 5, 6) == u.rgrg)
        assert(Vec4(5, 6, 6, 5) == u.rggr)
        assert(Vec4(5, 6, 6, 6) == u.rggg)
        assert(Vec4(6, 5, 5, 5) == u.grrr)
        assert(Vec4(6, 5, 5, 6) == u.grrg)
        assert(Vec4(6, 5, 6, 5) == u.grgr)
        assert(Vec4(6, 5, 6, 6) == u.grgg)
        assert(Vec4(6, 6, 5, 5) == u.ggrr)
        assert(Vec4(6, 6, 5, 6) == u.ggrg)
        assert(Vec4(6, 6, 6, 5) == u.gggr)
        assert(Vec4(6, 6, 6, 6) == u.gggg)

        assert(Vec2(5, 5) == u.ss)
        assert(Vec2(5, 6) == u.st)
        assert(Vec2(6, 5) == u.ts)
        assert(Vec2(6, 6) == u.tt)

        assert(Vec3(5, 5, 5) == u.sss)
        assert(Vec3(5, 5, 6) == u.sst)
        assert(Vec3(5, 6, 5) == u.sts)
        assert(Vec3(5, 6, 6) == u.stt)
        assert(Vec3(6, 5, 5) == u.tss)
        assert(Vec3(6, 5, 6) == u.tst)
        assert(Vec3(6, 6, 5) == u.tts)
        assert(Vec3(6, 6, 6) == u.ttt)

        assert(Vec4(5, 5, 5, 5) == u.ssss)
        assert(Vec4(5, 5, 5, 6) == u.ssst)
        assert(Vec4(5, 5, 6, 5) == u.ssts)
        assert(Vec4(5, 5, 6, 6) == u.sstt)
        assert(Vec4(5, 6, 5, 5) == u.stss)
        assert(Vec4(5, 6, 5, 6) == u.stst)
        assert(Vec4(5, 6, 6, 5) == u.stts)
        assert(Vec4(5, 6, 6, 6) == u.sttt)
        assert(Vec4(6, 5, 5, 5) == u.tsss)
        assert(Vec4(6, 5, 5, 6) == u.tsst)
        assert(Vec4(6, 5, 6, 5) == u.tsts)
        assert(Vec4(6, 5, 6, 6) == u.tstt)
        assert(Vec4(6, 6, 5, 5) == u.ttss)
        assert(Vec4(6, 6, 5, 6) == u.ttst)
        assert(Vec4(6, 6, 6, 5) == u.ttts)
        assert(Vec4(6, 6, 6, 6) == u.tttt)
    }

    test("Swizzled write") {
        var u = Vec2(1)

        u = Vec2(1); u.x = 5; assert(Vec2(5, 1) == u)
        u = Vec2(1); u.y = 5; assert(Vec2(1, 5) == u)

        u = Vec2(1); u.r = 5; assert(Vec2(5, 1) == u)
        u = Vec2(1); u.g = 5; assert(Vec2(1, 5) == u)

        u = Vec2(1); u.s = 5; assert(Vec2(5, 1) == u)
        u = Vec2(1); u.t = 5; assert(Vec2(1, 5) == u)

        u = Vec2(1); u.xy = Vec2(5, 6); assert(Vec2(5, 6) == u)
        u = Vec2(1); u.yx = Vec2(7, 8); assert(Vec2(8, 7) == u)

        u = Vec2(1); u.rg = Vec2(5, 6); assert(Vec2(5, 6) == u)
        u = Vec2(1); u.gr = Vec2(7, 8); assert(Vec2(8, 7) == u)

        u = Vec2(1); u.st = Vec2(5, 6); assert(Vec2(5, 6) == u)
        u = Vec2(1); u.ts = Vec2(7, 8); assert(Vec2(8, 7) == u)
    }

    test("Const math") {
        val u = ConstVec2(7, 8)

        assert(Vec2(-7, -8) == -u)

        assert(Vec2(14, 16) == u*2)
        assert(Vec2(14, 16) == 2*u)
        assert(Vec2(3.5f, 4) == u/2)
        assert(Vec2(1, 7/8f) == 7/u)

        val v = ConstVec2(2, 4)

        assert(Vec2(9, 12) == u + v)
        assert(Vec2(5, 4) == u - v)
        assert(Vec2(14, 32) == u*v)
        assert(Vec2(3.5f, 2) == u/v)

        val m2 = ConstMat2(2, 4, 3, 5)
        assert(Vec2(46, 61) == u*m2)

        val m2x3 = ConstMat2x3(2, 4, 3, 5, 6, 7)
        assert(Vec3(46, 61, 98) == u*m2x3)

        val m2x4 = ConstMat2x4(2, 4, 3, 5, 6, 7, 8, 9)
        assert(Vec4(46, 61, 98, 128) == u*m2x4)
    }

    test("Mutable math") {
        var u = Vec2(2, 3)

        u = Vec2(2, 3); u *= 2; assert(Vec2(4, 6) == u)
        u = Vec2(2, 3); u /= 2; assert(Vec2(1, 1.5f) == u)

        u = Vec2(2, 3); u += Vec2(3, 4); assert(Vec2(5, 7) == u)
        u = Vec2(2, 3); u += u; assert(Vec2(4, 6) == u)
        u = Vec2(2, 3); u -= Vec2(2, 3); assert(Vec2(0, 0) == u)
        u = Vec2(2, 3); u -= u; assert(Vec2(0, 0) == u)

        u = Vec2(2, 3); u *= Vec2(5, 10); assert(Vec2(10, 30) == u)
        u = Vec2(2, 3); u *= u; assert(Vec2(4, 9) == u)
        u = Vec2(2, 3); u /= Vec2(2, 6); assert(Vec2(1, 0.5f) == u)
        u = Vec2(2, 3); u /= u; assert(Vec2(1, 1) == u)

        u = Vec2(2, 3); u := Vec2(11, 12); assert(Vec2(11, 12) == u)
        u = Vec2(2, 3); u.set(22, 33); assert(Vec2(22, 33) == u)

        u = Vec2(7, 8)
        u *= ConstMat2(2, 4, 3, 5)
        assert(Vec2(46, 61) == u)
    }
}
