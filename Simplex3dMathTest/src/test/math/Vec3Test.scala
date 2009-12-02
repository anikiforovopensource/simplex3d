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
class Vec3Test extends FunSuite {

    test("Const factories") {
        var u = ConstVec3(5)
        expect(classOf[ConstVec3]) { u.getClass }
        expect(5) { u.x }
        expect(5) { u.y }
        expect(5) { u.z }
        
        u = ConstVec3(2, 3, 4)
        expect(classOf[ConstVec3]) { u.getClass }
        expect(2) { u.x }
        expect(3) { u.y }
        expect(4) { u.z }

        u = ConstVec3(6, Vec2(7, 8))
        expect(classOf[ConstVec3]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }
        expect(8) { u.z }

        u = ConstVec3(Vec2(6, 7), 8)
        expect(classOf[ConstVec3]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }
        expect(8) { u.z }

        u = ConstVec3(Vec3(4, 5, 6))
        expect(classOf[ConstVec3]) { u.getClass }
        expect(4) { u.x }
        expect(5) { u.y }
        expect(6) { u.z }

        u = ConstVec3(Vec4(1, 2, 3, 4))
        expect(classOf[ConstVec3]) { u.getClass }
        expect(1) { u.x }
        expect(2) { u.y }
        expect(3) { u.z }

        u = ConstVec3(Vec2i(4, 5), 6)
        expect(classOf[ConstVec3]) { u.getClass }
        expect(4) { u.x }
        expect(5) { u.y }
        expect(6) { u.z }

        u = ConstVec3(4, Vec2i(5, 6))
        expect(classOf[ConstVec3]) { u.getClass }
        expect(4) { u.x }
        expect(5) { u.y }
        expect(6) { u.z }

        u = ConstVec3(Vec3i(6, 7, 8))
        expect(classOf[ConstVec3]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }
        expect(8) { u.z }

        u = ConstVec3(Vec4i(1, 2, 3, 4))
        expect(classOf[ConstVec3]) { u.getClass }
        expect(1) { u.x }
        expect(2) { u.y }
        expect(3) { u.z }
    }

    test("Mutable factories") {
        var u = Vec3(5)
        expect(classOf[Vec3]) { u.getClass }
        expect(5) { u.x }
        expect(5) { u.y }
        expect(5) { u.z }

        u = Vec3(2, 3, 4)
        expect(classOf[Vec3]) { u.getClass }
        expect(2) { u.x }
        expect(3) { u.y }
        expect(4) { u.z }

        u = Vec3(Vec3(4, 5, 6))
        expect(classOf[Vec3]) { u.getClass }
        expect(4) { u.x }
        expect(5) { u.y }
        expect(6) { u.z }

        u = Vec3(6, Vec2(7, 8))
        expect(classOf[Vec3]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }
        expect(8) { u.z }

        u = Vec3(Vec2(6, 7), 8)
        expect(classOf[Vec3]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }
        expect(8) { u.z }

        u = Vec3(Vec4(1, 2, 3, 4))
        expect(classOf[Vec3]) { u.getClass }
        expect(1) { u.x }
        expect(2) { u.y }
        expect(3) { u.z }

        u = Vec3(Vec2i(4, 5), 6)
        expect(classOf[Vec3]) { u.getClass }
        expect(4) { u.x }
        expect(5) { u.y }
        expect(6) { u.z }

        u = Vec3(4, Vec2i(5, 6))
        expect(classOf[Vec3]) { u.getClass }
        expect(4) { u.x }
        expect(5) { u.y }
        expect(6) { u.z }

        u = Vec3(Vec3i(6, 7, 8))
        expect(classOf[Vec3]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }
        expect(8) { u.z }

        u = Vec3(Vec4i(1, 2, 3, 4))
        expect(classOf[Vec3]) { u.getClass }
        expect(1) { u.x }
        expect(2) { u.y }
        expect(3) { u.z }
    }

    test("Equality methods") {
        assert(Vec3(4, 7, 9) == ConstVec3(4, 7, 9))
        assert(ConstVec3(4, 7, 9) == Vec3(4, 7, 9))

        assert(Vec3(1, 2, 3) != Vec3(9, 2, 3))
        assert(Vec3(1, 2, 3) != Vec3(1, 9, 3))
        assert(Vec3(1, 2, 3) != Vec3(1, 2, 9))
    }

    test("Indexed read") {
        val u = ConstVec3(3, 4, 5)

        expect(3) { u(0) }
        expect(4) { u(1) }
        expect(5) { u(2) }

        intercept[IndexOutOfBoundsException] {
            u(3)
        }
        intercept[IndexOutOfBoundsException] {
            u(-1)
        }
    }

    test("Indexed write") {
        val u = Vec3(3, 4, 5)

        u(0) = 5
        assert(Vec3(5, 4, 5) == u)

        u(1) = 6
        assert(Vec3(5, 6, 5) == u)

        u(2) = 7
        assert(Vec3(5, 6, 7) == u)

        intercept[IndexOutOfBoundsException] {
            u(3) = 1
        }
        intercept[IndexOutOfBoundsException] {
            u(-1) = 1
        }
    }

    test("Swizzled read") {
        val u = ConstVec3(5, 6, 7)

        expect(5) { u.x }
        expect(6) { u.y }
        expect(7) { u.z }

        expect(5) { u.r }
        expect(6) { u.g }
        expect(7) { u.b }

        expect(5) { u.s }
        expect(6) { u.t }
        expect(7) { u.p }

        assert(Vec2(5, 5) == u.xx)
        assert(Vec2(5, 6) == u.xy)
        assert(Vec2(5, 7) == u.xz)
        assert(Vec2(6, 5) == u.yx)
        assert(Vec2(6, 6) == u.yy)
        assert(Vec2(6, 7) == u.yz)
        assert(Vec2(7, 5) == u.zx)
        assert(Vec2(7, 6) == u.zy)
        assert(Vec2(7, 7) == u.zz)
        assert(Vec3(5, 5, 5) == u.xxx)
        assert(Vec3(5, 5, 6) == u.xxy)
        assert(Vec3(5, 5, 7) == u.xxz)
        assert(Vec3(5, 6, 5) == u.xyx)
        assert(Vec3(5, 6, 6) == u.xyy)
        assert(Vec3(5, 6, 7) == u.xyz)
        assert(Vec3(5, 7, 5) == u.xzx)
        assert(Vec3(5, 7, 6) == u.xzy)
        assert(Vec3(5, 7, 7) == u.xzz)
        assert(Vec3(6, 5, 5) == u.yxx)
        assert(Vec3(6, 5, 6) == u.yxy)
        assert(Vec3(6, 5, 7) == u.yxz)
        assert(Vec3(6, 6, 5) == u.yyx)
        assert(Vec3(6, 6, 6) == u.yyy)
        assert(Vec3(6, 6, 7) == u.yyz)
        assert(Vec3(6, 7, 5) == u.yzx)
        assert(Vec3(6, 7, 6) == u.yzy)
        assert(Vec3(6, 7, 7) == u.yzz)
        assert(Vec3(7, 5, 5) == u.zxx)
        assert(Vec3(7, 5, 6) == u.zxy)
        assert(Vec3(7, 5, 7) == u.zxz)
        assert(Vec3(7, 6, 5) == u.zyx)
        assert(Vec3(7, 6, 6) == u.zyy)
        assert(Vec3(7, 6, 7) == u.zyz)
        assert(Vec3(7, 7, 5) == u.zzx)
        assert(Vec3(7, 7, 6) == u.zzy)
        assert(Vec3(7, 7, 7) == u.zzz)
        assert(Vec4(5, 5, 5, 5) == u.xxxx)
        assert(Vec4(5, 5, 5, 6) == u.xxxy)
        assert(Vec4(5, 5, 5, 7) == u.xxxz)
        assert(Vec4(5, 5, 6, 5) == u.xxyx)
        assert(Vec4(5, 5, 6, 6) == u.xxyy)
        assert(Vec4(5, 5, 6, 7) == u.xxyz)
        assert(Vec4(5, 5, 7, 5) == u.xxzx)
        assert(Vec4(5, 5, 7, 6) == u.xxzy)
        assert(Vec4(5, 5, 7, 7) == u.xxzz)
        assert(Vec4(5, 6, 5, 5) == u.xyxx)
        assert(Vec4(5, 6, 5, 6) == u.xyxy)
        assert(Vec4(5, 6, 5, 7) == u.xyxz)
        assert(Vec4(5, 6, 6, 5) == u.xyyx)
        assert(Vec4(5, 6, 6, 6) == u.xyyy)
        assert(Vec4(5, 6, 6, 7) == u.xyyz)
        assert(Vec4(5, 6, 7, 5) == u.xyzx)
        assert(Vec4(5, 6, 7, 6) == u.xyzy)
        assert(Vec4(5, 6, 7, 7) == u.xyzz)
        assert(Vec4(5, 7, 5, 5) == u.xzxx)
        assert(Vec4(5, 7, 5, 6) == u.xzxy)
        assert(Vec4(5, 7, 5, 7) == u.xzxz)
        assert(Vec4(5, 7, 6, 5) == u.xzyx)
        assert(Vec4(5, 7, 6, 6) == u.xzyy)
        assert(Vec4(5, 7, 6, 7) == u.xzyz)
        assert(Vec4(5, 7, 7, 5) == u.xzzx)
        assert(Vec4(5, 7, 7, 6) == u.xzzy)
        assert(Vec4(5, 7, 7, 7) == u.xzzz)
        assert(Vec4(6, 5, 5, 5) == u.yxxx)
        assert(Vec4(6, 5, 5, 6) == u.yxxy)
        assert(Vec4(6, 5, 5, 7) == u.yxxz)
        assert(Vec4(6, 5, 6, 5) == u.yxyx)
        assert(Vec4(6, 5, 6, 6) == u.yxyy)
        assert(Vec4(6, 5, 6, 7) == u.yxyz)
        assert(Vec4(6, 5, 7, 5) == u.yxzx)
        assert(Vec4(6, 5, 7, 6) == u.yxzy)
        assert(Vec4(6, 5, 7, 7) == u.yxzz)
        assert(Vec4(6, 6, 5, 5) == u.yyxx)
        assert(Vec4(6, 6, 5, 6) == u.yyxy)
        assert(Vec4(6, 6, 5, 7) == u.yyxz)
        assert(Vec4(6, 6, 6, 5) == u.yyyx)
        assert(Vec4(6, 6, 6, 6) == u.yyyy)
        assert(Vec4(6, 6, 6, 7) == u.yyyz)
        assert(Vec4(6, 6, 7, 5) == u.yyzx)
        assert(Vec4(6, 6, 7, 6) == u.yyzy)
        assert(Vec4(6, 6, 7, 7) == u.yyzz)
        assert(Vec4(6, 7, 5, 5) == u.yzxx)
        assert(Vec4(6, 7, 5, 6) == u.yzxy)
        assert(Vec4(6, 7, 5, 7) == u.yzxz)
        assert(Vec4(6, 7, 6, 5) == u.yzyx)
        assert(Vec4(6, 7, 6, 6) == u.yzyy)
        assert(Vec4(6, 7, 6, 7) == u.yzyz)
        assert(Vec4(6, 7, 7, 5) == u.yzzx)
        assert(Vec4(6, 7, 7, 6) == u.yzzy)
        assert(Vec4(6, 7, 7, 7) == u.yzzz)
        assert(Vec4(7, 5, 5, 5) == u.zxxx)
        assert(Vec4(7, 5, 5, 6) == u.zxxy)
        assert(Vec4(7, 5, 5, 7) == u.zxxz)
        assert(Vec4(7, 5, 6, 5) == u.zxyx)
        assert(Vec4(7, 5, 6, 6) == u.zxyy)
        assert(Vec4(7, 5, 6, 7) == u.zxyz)
        assert(Vec4(7, 5, 7, 5) == u.zxzx)
        assert(Vec4(7, 5, 7, 6) == u.zxzy)
        assert(Vec4(7, 5, 7, 7) == u.zxzz)
        assert(Vec4(7, 6, 5, 5) == u.zyxx)
        assert(Vec4(7, 6, 5, 6) == u.zyxy)
        assert(Vec4(7, 6, 5, 7) == u.zyxz)
        assert(Vec4(7, 6, 6, 5) == u.zyyx)
        assert(Vec4(7, 6, 6, 6) == u.zyyy)
        assert(Vec4(7, 6, 6, 7) == u.zyyz)
        assert(Vec4(7, 6, 7, 5) == u.zyzx)
        assert(Vec4(7, 6, 7, 6) == u.zyzy)
        assert(Vec4(7, 6, 7, 7) == u.zyzz)
        assert(Vec4(7, 7, 5, 5) == u.zzxx)
        assert(Vec4(7, 7, 5, 6) == u.zzxy)
        assert(Vec4(7, 7, 5, 7) == u.zzxz)
        assert(Vec4(7, 7, 6, 5) == u.zzyx)
        assert(Vec4(7, 7, 6, 6) == u.zzyy)
        assert(Vec4(7, 7, 6, 7) == u.zzyz)
        assert(Vec4(7, 7, 7, 5) == u.zzzx)
        assert(Vec4(7, 7, 7, 6) == u.zzzy)
        assert(Vec4(7, 7, 7, 7) == u.zzzz)
        assert(Vec2(5, 5) == u.rr)
        assert(Vec2(5, 6) == u.rg)
        assert(Vec2(5, 7) == u.rb)
        assert(Vec2(6, 5) == u.gr)
        assert(Vec2(6, 6) == u.gg)
        assert(Vec2(6, 7) == u.gb)
        assert(Vec2(7, 5) == u.br)
        assert(Vec2(7, 6) == u.bg)
        assert(Vec2(7, 7) == u.bb)
        assert(Vec3(5, 5, 5) == u.rrr)
        assert(Vec3(5, 5, 6) == u.rrg)
        assert(Vec3(5, 5, 7) == u.rrb)
        assert(Vec3(5, 6, 5) == u.rgr)
        assert(Vec3(5, 6, 6) == u.rgg)
        assert(Vec3(5, 6, 7) == u.rgb)
        assert(Vec3(5, 7, 5) == u.rbr)
        assert(Vec3(5, 7, 6) == u.rbg)
        assert(Vec3(5, 7, 7) == u.rbb)
        assert(Vec3(6, 5, 5) == u.grr)
        assert(Vec3(6, 5, 6) == u.grg)
        assert(Vec3(6, 5, 7) == u.grb)
        assert(Vec3(6, 6, 5) == u.ggr)
        assert(Vec3(6, 6, 6) == u.ggg)
        assert(Vec3(6, 6, 7) == u.ggb)
        assert(Vec3(6, 7, 5) == u.gbr)
        assert(Vec3(6, 7, 6) == u.gbg)
        assert(Vec3(6, 7, 7) == u.gbb)
        assert(Vec3(7, 5, 5) == u.brr)
        assert(Vec3(7, 5, 6) == u.brg)
        assert(Vec3(7, 5, 7) == u.brb)
        assert(Vec3(7, 6, 5) == u.bgr)
        assert(Vec3(7, 6, 6) == u.bgg)
        assert(Vec3(7, 6, 7) == u.bgb)
        assert(Vec3(7, 7, 5) == u.bbr)
        assert(Vec3(7, 7, 6) == u.bbg)
        assert(Vec3(7, 7, 7) == u.bbb)
        assert(Vec4(5, 5, 5, 5) == u.rrrr)
        assert(Vec4(5, 5, 5, 6) == u.rrrg)
        assert(Vec4(5, 5, 5, 7) == u.rrrb)
        assert(Vec4(5, 5, 6, 5) == u.rrgr)
        assert(Vec4(5, 5, 6, 6) == u.rrgg)
        assert(Vec4(5, 5, 6, 7) == u.rrgb)
        assert(Vec4(5, 5, 7, 5) == u.rrbr)
        assert(Vec4(5, 5, 7, 6) == u.rrbg)
        assert(Vec4(5, 5, 7, 7) == u.rrbb)
        assert(Vec4(5, 6, 5, 5) == u.rgrr)
        assert(Vec4(5, 6, 5, 6) == u.rgrg)
        assert(Vec4(5, 6, 5, 7) == u.rgrb)
        assert(Vec4(5, 6, 6, 5) == u.rggr)
        assert(Vec4(5, 6, 6, 6) == u.rggg)
        assert(Vec4(5, 6, 6, 7) == u.rggb)
        assert(Vec4(5, 6, 7, 5) == u.rgbr)
        assert(Vec4(5, 6, 7, 6) == u.rgbg)
        assert(Vec4(5, 6, 7, 7) == u.rgbb)
        assert(Vec4(5, 7, 5, 5) == u.rbrr)
        assert(Vec4(5, 7, 5, 6) == u.rbrg)
        assert(Vec4(5, 7, 5, 7) == u.rbrb)
        assert(Vec4(5, 7, 6, 5) == u.rbgr)
        assert(Vec4(5, 7, 6, 6) == u.rbgg)
        assert(Vec4(5, 7, 6, 7) == u.rbgb)
        assert(Vec4(5, 7, 7, 5) == u.rbbr)
        assert(Vec4(5, 7, 7, 6) == u.rbbg)
        assert(Vec4(5, 7, 7, 7) == u.rbbb)
        assert(Vec4(6, 5, 5, 5) == u.grrr)
        assert(Vec4(6, 5, 5, 6) == u.grrg)
        assert(Vec4(6, 5, 5, 7) == u.grrb)
        assert(Vec4(6, 5, 6, 5) == u.grgr)
        assert(Vec4(6, 5, 6, 6) == u.grgg)
        assert(Vec4(6, 5, 6, 7) == u.grgb)
        assert(Vec4(6, 5, 7, 5) == u.grbr)
        assert(Vec4(6, 5, 7, 6) == u.grbg)
        assert(Vec4(6, 5, 7, 7) == u.grbb)
        assert(Vec4(6, 6, 5, 5) == u.ggrr)
        assert(Vec4(6, 6, 5, 6) == u.ggrg)
        assert(Vec4(6, 6, 5, 7) == u.ggrb)
        assert(Vec4(6, 6, 6, 5) == u.gggr)
        assert(Vec4(6, 6, 6, 6) == u.gggg)
        assert(Vec4(6, 6, 6, 7) == u.gggb)
        assert(Vec4(6, 6, 7, 5) == u.ggbr)
        assert(Vec4(6, 6, 7, 6) == u.ggbg)
        assert(Vec4(6, 6, 7, 7) == u.ggbb)
        assert(Vec4(6, 7, 5, 5) == u.gbrr)
        assert(Vec4(6, 7, 5, 6) == u.gbrg)
        assert(Vec4(6, 7, 5, 7) == u.gbrb)
        assert(Vec4(6, 7, 6, 5) == u.gbgr)
        assert(Vec4(6, 7, 6, 6) == u.gbgg)
        assert(Vec4(6, 7, 6, 7) == u.gbgb)
        assert(Vec4(6, 7, 7, 5) == u.gbbr)
        assert(Vec4(6, 7, 7, 6) == u.gbbg)
        assert(Vec4(6, 7, 7, 7) == u.gbbb)
        assert(Vec4(7, 5, 5, 5) == u.brrr)
        assert(Vec4(7, 5, 5, 6) == u.brrg)
        assert(Vec4(7, 5, 5, 7) == u.brrb)
        assert(Vec4(7, 5, 6, 5) == u.brgr)
        assert(Vec4(7, 5, 6, 6) == u.brgg)
        assert(Vec4(7, 5, 6, 7) == u.brgb)
        assert(Vec4(7, 5, 7, 5) == u.brbr)
        assert(Vec4(7, 5, 7, 6) == u.brbg)
        assert(Vec4(7, 5, 7, 7) == u.brbb)
        assert(Vec4(7, 6, 5, 5) == u.bgrr)
        assert(Vec4(7, 6, 5, 6) == u.bgrg)
        assert(Vec4(7, 6, 5, 7) == u.bgrb)
        assert(Vec4(7, 6, 6, 5) == u.bggr)
        assert(Vec4(7, 6, 6, 6) == u.bggg)
        assert(Vec4(7, 6, 6, 7) == u.bggb)
        assert(Vec4(7, 6, 7, 5) == u.bgbr)
        assert(Vec4(7, 6, 7, 6) == u.bgbg)
        assert(Vec4(7, 6, 7, 7) == u.bgbb)
        assert(Vec4(7, 7, 5, 5) == u.bbrr)
        assert(Vec4(7, 7, 5, 6) == u.bbrg)
        assert(Vec4(7, 7, 5, 7) == u.bbrb)
        assert(Vec4(7, 7, 6, 5) == u.bbgr)
        assert(Vec4(7, 7, 6, 6) == u.bbgg)
        assert(Vec4(7, 7, 6, 7) == u.bbgb)
        assert(Vec4(7, 7, 7, 5) == u.bbbr)
        assert(Vec4(7, 7, 7, 6) == u.bbbg)
        assert(Vec4(7, 7, 7, 7) == u.bbbb)
        assert(Vec2(5, 5) == u.ss)
        assert(Vec2(5, 6) == u.st)
        assert(Vec2(5, 7) == u.sp)
        assert(Vec2(6, 5) == u.ts)
        assert(Vec2(6, 6) == u.tt)
        assert(Vec2(6, 7) == u.tp)
        assert(Vec2(7, 5) == u.ps)
        assert(Vec2(7, 6) == u.pt)
        assert(Vec2(7, 7) == u.pp)
        assert(Vec3(5, 5, 5) == u.sss)
        assert(Vec3(5, 5, 6) == u.sst)
        assert(Vec3(5, 5, 7) == u.ssp)
        assert(Vec3(5, 6, 5) == u.sts)
        assert(Vec3(5, 6, 6) == u.stt)
        assert(Vec3(5, 6, 7) == u.stp)
        assert(Vec3(5, 7, 5) == u.sps)
        assert(Vec3(5, 7, 6) == u.spt)
        assert(Vec3(5, 7, 7) == u.spp)
        assert(Vec3(6, 5, 5) == u.tss)
        assert(Vec3(6, 5, 6) == u.tst)
        assert(Vec3(6, 5, 7) == u.tsp)
        assert(Vec3(6, 6, 5) == u.tts)
        assert(Vec3(6, 6, 6) == u.ttt)
        assert(Vec3(6, 6, 7) == u.ttp)
        assert(Vec3(6, 7, 5) == u.tps)
        assert(Vec3(6, 7, 6) == u.tpt)
        assert(Vec3(6, 7, 7) == u.tpp)
        assert(Vec3(7, 5, 5) == u.pss)
        assert(Vec3(7, 5, 6) == u.pst)
        assert(Vec3(7, 5, 7) == u.psp)
        assert(Vec3(7, 6, 5) == u.pts)
        assert(Vec3(7, 6, 6) == u.ptt)
        assert(Vec3(7, 6, 7) == u.ptp)
        assert(Vec3(7, 7, 5) == u.pps)
        assert(Vec3(7, 7, 6) == u.ppt)
        assert(Vec3(7, 7, 7) == u.ppp)
        assert(Vec4(5, 5, 5, 5) == u.ssss)
        assert(Vec4(5, 5, 5, 6) == u.ssst)
        assert(Vec4(5, 5, 5, 7) == u.sssp)
        assert(Vec4(5, 5, 6, 5) == u.ssts)
        assert(Vec4(5, 5, 6, 6) == u.sstt)
        assert(Vec4(5, 5, 6, 7) == u.sstp)
        assert(Vec4(5, 5, 7, 5) == u.ssps)
        assert(Vec4(5, 5, 7, 6) == u.sspt)
        assert(Vec4(5, 5, 7, 7) == u.sspp)
        assert(Vec4(5, 6, 5, 5) == u.stss)
        assert(Vec4(5, 6, 5, 6) == u.stst)
        assert(Vec4(5, 6, 5, 7) == u.stsp)
        assert(Vec4(5, 6, 6, 5) == u.stts)
        assert(Vec4(5, 6, 6, 6) == u.sttt)
        assert(Vec4(5, 6, 6, 7) == u.sttp)
        assert(Vec4(5, 6, 7, 5) == u.stps)
        assert(Vec4(5, 6, 7, 6) == u.stpt)
        assert(Vec4(5, 6, 7, 7) == u.stpp)
        assert(Vec4(5, 7, 5, 5) == u.spss)
        assert(Vec4(5, 7, 5, 6) == u.spst)
        assert(Vec4(5, 7, 5, 7) == u.spsp)
        assert(Vec4(5, 7, 6, 5) == u.spts)
        assert(Vec4(5, 7, 6, 6) == u.sptt)
        assert(Vec4(5, 7, 6, 7) == u.sptp)
        assert(Vec4(5, 7, 7, 5) == u.spps)
        assert(Vec4(5, 7, 7, 6) == u.sppt)
        assert(Vec4(5, 7, 7, 7) == u.sppp)
        assert(Vec4(6, 5, 5, 5) == u.tsss)
        assert(Vec4(6, 5, 5, 6) == u.tsst)
        assert(Vec4(6, 5, 5, 7) == u.tssp)
        assert(Vec4(6, 5, 6, 5) == u.tsts)
        assert(Vec4(6, 5, 6, 6) == u.tstt)
        assert(Vec4(6, 5, 6, 7) == u.tstp)
        assert(Vec4(6, 5, 7, 5) == u.tsps)
        assert(Vec4(6, 5, 7, 6) == u.tspt)
        assert(Vec4(6, 5, 7, 7) == u.tspp)
        assert(Vec4(6, 6, 5, 5) == u.ttss)
        assert(Vec4(6, 6, 5, 6) == u.ttst)
        assert(Vec4(6, 6, 5, 7) == u.ttsp)
        assert(Vec4(6, 6, 6, 5) == u.ttts)
        assert(Vec4(6, 6, 6, 6) == u.tttt)
        assert(Vec4(6, 6, 6, 7) == u.tttp)
        assert(Vec4(6, 6, 7, 5) == u.ttps)
        assert(Vec4(6, 6, 7, 6) == u.ttpt)
        assert(Vec4(6, 6, 7, 7) == u.ttpp)
        assert(Vec4(6, 7, 5, 5) == u.tpss)
        assert(Vec4(6, 7, 5, 6) == u.tpst)
        assert(Vec4(6, 7, 5, 7) == u.tpsp)
        assert(Vec4(6, 7, 6, 5) == u.tpts)
        assert(Vec4(6, 7, 6, 6) == u.tptt)
        assert(Vec4(6, 7, 6, 7) == u.tptp)
        assert(Vec4(6, 7, 7, 5) == u.tpps)
        assert(Vec4(6, 7, 7, 6) == u.tppt)
        assert(Vec4(6, 7, 7, 7) == u.tppp)
        assert(Vec4(7, 5, 5, 5) == u.psss)
        assert(Vec4(7, 5, 5, 6) == u.psst)
        assert(Vec4(7, 5, 5, 7) == u.pssp)
        assert(Vec4(7, 5, 6, 5) == u.psts)
        assert(Vec4(7, 5, 6, 6) == u.pstt)
        assert(Vec4(7, 5, 6, 7) == u.pstp)
        assert(Vec4(7, 5, 7, 5) == u.psps)
        assert(Vec4(7, 5, 7, 6) == u.pspt)
        assert(Vec4(7, 5, 7, 7) == u.pspp)
        assert(Vec4(7, 6, 5, 5) == u.ptss)
        assert(Vec4(7, 6, 5, 6) == u.ptst)
        assert(Vec4(7, 6, 5, 7) == u.ptsp)
        assert(Vec4(7, 6, 6, 5) == u.ptts)
        assert(Vec4(7, 6, 6, 6) == u.pttt)
        assert(Vec4(7, 6, 6, 7) == u.pttp)
        assert(Vec4(7, 6, 7, 5) == u.ptps)
        assert(Vec4(7, 6, 7, 6) == u.ptpt)
        assert(Vec4(7, 6, 7, 7) == u.ptpp)
        assert(Vec4(7, 7, 5, 5) == u.ppss)
        assert(Vec4(7, 7, 5, 6) == u.ppst)
        assert(Vec4(7, 7, 5, 7) == u.ppsp)
        assert(Vec4(7, 7, 6, 5) == u.ppts)
        assert(Vec4(7, 7, 6, 6) == u.pptt)
        assert(Vec4(7, 7, 6, 7) == u.pptp)
        assert(Vec4(7, 7, 7, 5) == u.ppps)
        assert(Vec4(7, 7, 7, 6) == u.pppt)
        assert(Vec4(7, 7, 7, 7) == u.pppp)
    }

    test("Swizzled write") {
        var u = Vec3(1)

        u = Vec3(1); u.x = 5; assert(Vec3(5, 1, 1) == u)
        u = Vec3(1); u.y = 5; assert(Vec3(1, 5, 1) == u)
        u = Vec3(1); u.z = 5; assert(Vec3(1, 1, 5) == u)

        u = Vec3(1); u.r = 5; assert(Vec3(5, 1, 1) == u)
        u = Vec3(1); u.g = 5; assert(Vec3(1, 5, 1) == u)
        u = Vec3(1); u.b = 5; assert(Vec3(1, 1, 5) == u)

        u = Vec3(1); u.s = 5; assert(Vec3(5, 1, 1) == u)
        u = Vec3(1); u.t = 5; assert(Vec3(1, 5, 1) == u)
        u = Vec3(1); u.p = 5; assert(Vec3(1, 1, 5) == u)

        u = Vec3(1); u.xy = Vec2(5, 6); assert(Vec3(5, 6, 1) == u)
        u = Vec3(1); u.xz = Vec2(5, 7); assert(Vec3(5, 1, 7) == u)
        u = Vec3(1); u.yx = Vec2(6, 5); assert(Vec3(5, 6, 1) == u)
        u = Vec3(1); u.yz = Vec2(6, 7); assert(Vec3(1, 6, 7) == u)
        u = Vec3(1); u.zx = Vec2(7, 5); assert(Vec3(5, 1, 7) == u)
        u = Vec3(1); u.zy = Vec2(7, 6); assert(Vec3(1, 6, 7) == u)
        u = Vec3(1); u.xyz = Vec3(5, 6, 7); assert(Vec3(5, 6, 7) == u)
        u = Vec3(1); u.xzy = Vec3(5, 7, 6); assert(Vec3(5, 6, 7) == u)
        u = Vec3(1); u.yxz = Vec3(6, 5, 7); assert(Vec3(5, 6, 7) == u)
        u = Vec3(1); u.yzx = Vec3(6, 7, 5); assert(Vec3(5, 6, 7) == u)
        u = Vec3(1); u.zxy = Vec3(7, 5, 6); assert(Vec3(5, 6, 7) == u)
        u = Vec3(1); u.zyx = Vec3(7, 6, 5); assert(Vec3(5, 6, 7) == u)
        u = Vec3(1); u.rg = Vec2(5, 6); assert(Vec3(5, 6, 1) == u)
        u = Vec3(1); u.rb = Vec2(5, 7); assert(Vec3(5, 1, 7) == u)
        u = Vec3(1); u.gr = Vec2(6, 5); assert(Vec3(5, 6, 1) == u)
        u = Vec3(1); u.gb = Vec2(6, 7); assert(Vec3(1, 6, 7) == u)
        u = Vec3(1); u.br = Vec2(7, 5); assert(Vec3(5, 1, 7) == u)
        u = Vec3(1); u.bg = Vec2(7, 6); assert(Vec3(1, 6, 7) == u)
        u = Vec3(1); u.rgb = Vec3(5, 6, 7); assert(Vec3(5, 6, 7) == u)
        u = Vec3(1); u.rbg = Vec3(5, 7, 6); assert(Vec3(5, 6, 7) == u)
        u = Vec3(1); u.grb = Vec3(6, 5, 7); assert(Vec3(5, 6, 7) == u)
        u = Vec3(1); u.gbr = Vec3(6, 7, 5); assert(Vec3(5, 6, 7) == u)
        u = Vec3(1); u.brg = Vec3(7, 5, 6); assert(Vec3(5, 6, 7) == u)
        u = Vec3(1); u.bgr = Vec3(7, 6, 5); assert(Vec3(5, 6, 7) == u)
        u = Vec3(1); u.st = Vec2(5, 6); assert(Vec3(5, 6, 1) == u)
        u = Vec3(1); u.sp = Vec2(5, 7); assert(Vec3(5, 1, 7) == u)
        u = Vec3(1); u.ts = Vec2(6, 5); assert(Vec3(5, 6, 1) == u)
        u = Vec3(1); u.tp = Vec2(6, 7); assert(Vec3(1, 6, 7) == u)
        u = Vec3(1); u.ps = Vec2(7, 5); assert(Vec3(5, 1, 7) == u)
        u = Vec3(1); u.pt = Vec2(7, 6); assert(Vec3(1, 6, 7) == u)
        u = Vec3(1); u.stp = Vec3(5, 6, 7); assert(Vec3(5, 6, 7) == u)
        u = Vec3(1); u.spt = Vec3(5, 7, 6); assert(Vec3(5, 6, 7) == u)
        u = Vec3(1); u.tsp = Vec3(6, 5, 7); assert(Vec3(5, 6, 7) == u)
        u = Vec3(1); u.tps = Vec3(6, 7, 5); assert(Vec3(5, 6, 7) == u)
        u = Vec3(1); u.pst = Vec3(7, 5, 6); assert(Vec3(5, 6, 7) == u)
        u = Vec3(1); u.pts = Vec3(7, 6, 5); assert(Vec3(5, 6, 7) == u)
    }

    test("Const math") {
        val u = ConstVec3(7, 8, 9)

        assert(Vec3(-7, -8, -9) == -u)

        assert(Vec3(14, 16, 18) == u*2)
        assert(Vec3(14, 16, 18) == 2*u)
        assert(Vec3(3.5f, 4, 4.5f) == u/2)
        assert(Vec3(1, 7/8f, 7/9f) == 7/u)

        val v = ConstVec3(2, 4, 3)

        assert(Vec3(9, 12, 12) == u + v)
        assert(Vec3(5, 4, 6) == u - v)
        assert(Vec3(14, 32, 27) == u*v)
        assert(Vec3(3.5f, 2, 3) == u/v)

        val t = ConstVec3(2, 3, 4)

        val m3x2 = ConstMat3x2(
            2, 5, 4,
            3, 4, 8
        )
        assert(Vec2(35, 50) == t*m3x2)

        val m3 = ConstMat3(
            2, 5, 4,
            3, 4, 8,
            7, 4, 2
        )
        assert(Vec3(35, 50, 34) == t*m3)

        val m3x4 = ConstMat3x4(
            2, 5, 4,
            3, 4, 8,
            7, 4, 2,
            5, 9, 2
        )
        assert(Vec4(35, 50, 34, 45) == t*m3x4)
    }

    test("Mutable math") {
        var u = Vec3(2, 3, 4)

        u = Vec3(2, 3, 4); u *= 2; assert(Vec3(4, 6, 8) == u)
        u = Vec3(2, 3, 4); u /= 2; assert(Vec3(1, 1.5f, 2) == u)

        u = Vec3(2, 3, 4); u += Vec3(3, 4, 5); assert(Vec3(5, 7, 9) == u)
        u = Vec3(2, 3, 4); u += u; assert(Vec3(4, 6, 8) == u)
        u = Vec3(2, 3, 4); u -= Vec3(2, 3, 4); assert(Vec3(0, 0, 0) == u)
        u = Vec3(2, 3, 4); u -= u; assert(Vec3(0, 0, 0) == u)

        u = Vec3(2, 3, 4); u *= Vec3(5, 10, 15); assert(Vec3(10, 30, 60) == u)
        u = Vec3(2, 3, 4); u *= u; assert(Vec3(4, 9, 16) == u)
        u = Vec3(2, 3, 4); u /= Vec3(2, 6, 2); assert(Vec3(1, 0.5f, 2) == u)
        u = Vec3(2, 3, 4); u /= u; assert(Vec3(1, 1, 1) == u)

        u = Vec3(2, 3, 4); u := Vec3(11, 12, 13); assert(Vec3(11, 12, 13) == u)
        u = Vec3(2, 3, 4); u.set(22, 33, 44); assert(Vec3(22, 33, 44) == u)

        u = Vec3(2, 3, 4)
        val m3 = ConstMat3(
            2, 5, 4,
            3, 4, 8,
            7, 4, 2
        )
        u *= m3
        assert(Vec3(35, 50, 34) == u)
    }
}
