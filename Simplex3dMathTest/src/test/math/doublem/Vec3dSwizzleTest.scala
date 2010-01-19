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

package test.math.doublem

import org.scalatest._

import simplex3d.math.doublem.renamed._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Vec3dSwizzleTest extends FunSuite {

    test("Swizzled read") {
        val x = 5d
        val y = 6d
        val z = 7d

        val u = ConstVec3(x, y, z)

        expect(x) { u.x }
        expect(y) { u.y }
        expect(z) { u.z }

        expect(x) { u.r }
        expect(y) { u.g }
        expect(z) { u.b }

        expect(x) { u.s }
        expect(y) { u.t }
        expect(z) { u.p }

        assert(Vec2(x, x) == u.xx)
        assert(Vec2(x, y) == u.xy)
        assert(Vec2(x, z) == u.xz)
        assert(Vec2(y, x) == u.yx)
        assert(Vec2(y, y) == u.yy)
        assert(Vec2(y, z) == u.yz)
        assert(Vec2(z, x) == u.zx)
        assert(Vec2(z, y) == u.zy)
        assert(Vec2(z, z) == u.zz)
        assert(Vec3(x, x, x) == u.xxx)
        assert(Vec3(x, x, y) == u.xxy)
        assert(Vec3(x, x, z) == u.xxz)
        assert(Vec3(x, y, x) == u.xyx)
        assert(Vec3(x, y, y) == u.xyy)
        assert(Vec3(x, y, z) == u.xyz)
        assert(Vec3(x, z, x) == u.xzx)
        assert(Vec3(x, z, y) == u.xzy)
        assert(Vec3(x, z, z) == u.xzz)
        assert(Vec3(y, x, x) == u.yxx)
        assert(Vec3(y, x, y) == u.yxy)
        assert(Vec3(y, x, z) == u.yxz)
        assert(Vec3(y, y, x) == u.yyx)
        assert(Vec3(y, y, y) == u.yyy)
        assert(Vec3(y, y, z) == u.yyz)
        assert(Vec3(y, z, x) == u.yzx)
        assert(Vec3(y, z, y) == u.yzy)
        assert(Vec3(y, z, z) == u.yzz)
        assert(Vec3(z, x, x) == u.zxx)
        assert(Vec3(z, x, y) == u.zxy)
        assert(Vec3(z, x, z) == u.zxz)
        assert(Vec3(z, y, x) == u.zyx)
        assert(Vec3(z, y, y) == u.zyy)
        assert(Vec3(z, y, z) == u.zyz)
        assert(Vec3(z, z, x) == u.zzx)
        assert(Vec3(z, z, y) == u.zzy)
        assert(Vec3(z, z, z) == u.zzz)
        assert(Vec4(x, x, x, x) == u.xxxx)
        assert(Vec4(x, x, x, y) == u.xxxy)
        assert(Vec4(x, x, x, z) == u.xxxz)
        assert(Vec4(x, x, y, x) == u.xxyx)
        assert(Vec4(x, x, y, y) == u.xxyy)
        assert(Vec4(x, x, y, z) == u.xxyz)
        assert(Vec4(x, x, z, x) == u.xxzx)
        assert(Vec4(x, x, z, y) == u.xxzy)
        assert(Vec4(x, x, z, z) == u.xxzz)
        assert(Vec4(x, y, x, x) == u.xyxx)
        assert(Vec4(x, y, x, y) == u.xyxy)
        assert(Vec4(x, y, x, z) == u.xyxz)
        assert(Vec4(x, y, y, x) == u.xyyx)
        assert(Vec4(x, y, y, y) == u.xyyy)
        assert(Vec4(x, y, y, z) == u.xyyz)
        assert(Vec4(x, y, z, x) == u.xyzx)
        assert(Vec4(x, y, z, y) == u.xyzy)
        assert(Vec4(x, y, z, z) == u.xyzz)
        assert(Vec4(x, z, x, x) == u.xzxx)
        assert(Vec4(x, z, x, y) == u.xzxy)
        assert(Vec4(x, z, x, z) == u.xzxz)
        assert(Vec4(x, z, y, x) == u.xzyx)
        assert(Vec4(x, z, y, y) == u.xzyy)
        assert(Vec4(x, z, y, z) == u.xzyz)
        assert(Vec4(x, z, z, x) == u.xzzx)
        assert(Vec4(x, z, z, y) == u.xzzy)
        assert(Vec4(x, z, z, z) == u.xzzz)
        assert(Vec4(y, x, x, x) == u.yxxx)
        assert(Vec4(y, x, x, y) == u.yxxy)
        assert(Vec4(y, x, x, z) == u.yxxz)
        assert(Vec4(y, x, y, x) == u.yxyx)
        assert(Vec4(y, x, y, y) == u.yxyy)
        assert(Vec4(y, x, y, z) == u.yxyz)
        assert(Vec4(y, x, z, x) == u.yxzx)
        assert(Vec4(y, x, z, y) == u.yxzy)
        assert(Vec4(y, x, z, z) == u.yxzz)
        assert(Vec4(y, y, x, x) == u.yyxx)
        assert(Vec4(y, y, x, y) == u.yyxy)
        assert(Vec4(y, y, x, z) == u.yyxz)
        assert(Vec4(y, y, y, x) == u.yyyx)
        assert(Vec4(y, y, y, y) == u.yyyy)
        assert(Vec4(y, y, y, z) == u.yyyz)
        assert(Vec4(y, y, z, x) == u.yyzx)
        assert(Vec4(y, y, z, y) == u.yyzy)
        assert(Vec4(y, y, z, z) == u.yyzz)
        assert(Vec4(y, z, x, x) == u.yzxx)
        assert(Vec4(y, z, x, y) == u.yzxy)
        assert(Vec4(y, z, x, z) == u.yzxz)
        assert(Vec4(y, z, y, x) == u.yzyx)
        assert(Vec4(y, z, y, y) == u.yzyy)
        assert(Vec4(y, z, y, z) == u.yzyz)
        assert(Vec4(y, z, z, x) == u.yzzx)
        assert(Vec4(y, z, z, y) == u.yzzy)
        assert(Vec4(y, z, z, z) == u.yzzz)
        assert(Vec4(z, x, x, x) == u.zxxx)
        assert(Vec4(z, x, x, y) == u.zxxy)
        assert(Vec4(z, x, x, z) == u.zxxz)
        assert(Vec4(z, x, y, x) == u.zxyx)
        assert(Vec4(z, x, y, y) == u.zxyy)
        assert(Vec4(z, x, y, z) == u.zxyz)
        assert(Vec4(z, x, z, x) == u.zxzx)
        assert(Vec4(z, x, z, y) == u.zxzy)
        assert(Vec4(z, x, z, z) == u.zxzz)
        assert(Vec4(z, y, x, x) == u.zyxx)
        assert(Vec4(z, y, x, y) == u.zyxy)
        assert(Vec4(z, y, x, z) == u.zyxz)
        assert(Vec4(z, y, y, x) == u.zyyx)
        assert(Vec4(z, y, y, y) == u.zyyy)
        assert(Vec4(z, y, y, z) == u.zyyz)
        assert(Vec4(z, y, z, x) == u.zyzx)
        assert(Vec4(z, y, z, y) == u.zyzy)
        assert(Vec4(z, y, z, z) == u.zyzz)
        assert(Vec4(z, z, x, x) == u.zzxx)
        assert(Vec4(z, z, x, y) == u.zzxy)
        assert(Vec4(z, z, x, z) == u.zzxz)
        assert(Vec4(z, z, y, x) == u.zzyx)
        assert(Vec4(z, z, y, y) == u.zzyy)
        assert(Vec4(z, z, y, z) == u.zzyz)
        assert(Vec4(z, z, z, x) == u.zzzx)
        assert(Vec4(z, z, z, y) == u.zzzy)
        assert(Vec4(z, z, z, z) == u.zzzz)

        assert(Vec2(x, x) == u.rr)
        assert(Vec2(x, y) == u.rg)
        assert(Vec2(x, z) == u.rb)
        assert(Vec2(y, x) == u.gr)
        assert(Vec2(y, y) == u.gg)
        assert(Vec2(y, z) == u.gb)
        assert(Vec2(z, x) == u.br)
        assert(Vec2(z, y) == u.bg)
        assert(Vec2(z, z) == u.bb)
        assert(Vec3(x, x, x) == u.rrr)
        assert(Vec3(x, x, y) == u.rrg)
        assert(Vec3(x, x, z) == u.rrb)
        assert(Vec3(x, y, x) == u.rgr)
        assert(Vec3(x, y, y) == u.rgg)
        assert(Vec3(x, y, z) == u.rgb)
        assert(Vec3(x, z, x) == u.rbr)
        assert(Vec3(x, z, y) == u.rbg)
        assert(Vec3(x, z, z) == u.rbb)
        assert(Vec3(y, x, x) == u.grr)
        assert(Vec3(y, x, y) == u.grg)
        assert(Vec3(y, x, z) == u.grb)
        assert(Vec3(y, y, x) == u.ggr)
        assert(Vec3(y, y, y) == u.ggg)
        assert(Vec3(y, y, z) == u.ggb)
        assert(Vec3(y, z, x) == u.gbr)
        assert(Vec3(y, z, y) == u.gbg)
        assert(Vec3(y, z, z) == u.gbb)
        assert(Vec3(z, x, x) == u.brr)
        assert(Vec3(z, x, y) == u.brg)
        assert(Vec3(z, x, z) == u.brb)
        assert(Vec3(z, y, x) == u.bgr)
        assert(Vec3(z, y, y) == u.bgg)
        assert(Vec3(z, y, z) == u.bgb)
        assert(Vec3(z, z, x) == u.bbr)
        assert(Vec3(z, z, y) == u.bbg)
        assert(Vec3(z, z, z) == u.bbb)
        assert(Vec4(x, x, x, x) == u.rrrr)
        assert(Vec4(x, x, x, y) == u.rrrg)
        assert(Vec4(x, x, x, z) == u.rrrb)
        assert(Vec4(x, x, y, x) == u.rrgr)
        assert(Vec4(x, x, y, y) == u.rrgg)
        assert(Vec4(x, x, y, z) == u.rrgb)
        assert(Vec4(x, x, z, x) == u.rrbr)
        assert(Vec4(x, x, z, y) == u.rrbg)
        assert(Vec4(x, x, z, z) == u.rrbb)
        assert(Vec4(x, y, x, x) == u.rgrr)
        assert(Vec4(x, y, x, y) == u.rgrg)
        assert(Vec4(x, y, x, z) == u.rgrb)
        assert(Vec4(x, y, y, x) == u.rggr)
        assert(Vec4(x, y, y, y) == u.rggg)
        assert(Vec4(x, y, y, z) == u.rggb)
        assert(Vec4(x, y, z, x) == u.rgbr)
        assert(Vec4(x, y, z, y) == u.rgbg)
        assert(Vec4(x, y, z, z) == u.rgbb)
        assert(Vec4(x, z, x, x) == u.rbrr)
        assert(Vec4(x, z, x, y) == u.rbrg)
        assert(Vec4(x, z, x, z) == u.rbrb)
        assert(Vec4(x, z, y, x) == u.rbgr)
        assert(Vec4(x, z, y, y) == u.rbgg)
        assert(Vec4(x, z, y, z) == u.rbgb)
        assert(Vec4(x, z, z, x) == u.rbbr)
        assert(Vec4(x, z, z, y) == u.rbbg)
        assert(Vec4(x, z, z, z) == u.rbbb)
        assert(Vec4(y, x, x, x) == u.grrr)
        assert(Vec4(y, x, x, y) == u.grrg)
        assert(Vec4(y, x, x, z) == u.grrb)
        assert(Vec4(y, x, y, x) == u.grgr)
        assert(Vec4(y, x, y, y) == u.grgg)
        assert(Vec4(y, x, y, z) == u.grgb)
        assert(Vec4(y, x, z, x) == u.grbr)
        assert(Vec4(y, x, z, y) == u.grbg)
        assert(Vec4(y, x, z, z) == u.grbb)
        assert(Vec4(y, y, x, x) == u.ggrr)
        assert(Vec4(y, y, x, y) == u.ggrg)
        assert(Vec4(y, y, x, z) == u.ggrb)
        assert(Vec4(y, y, y, x) == u.gggr)
        assert(Vec4(y, y, y, y) == u.gggg)
        assert(Vec4(y, y, y, z) == u.gggb)
        assert(Vec4(y, y, z, x) == u.ggbr)
        assert(Vec4(y, y, z, y) == u.ggbg)
        assert(Vec4(y, y, z, z) == u.ggbb)
        assert(Vec4(y, z, x, x) == u.gbrr)
        assert(Vec4(y, z, x, y) == u.gbrg)
        assert(Vec4(y, z, x, z) == u.gbrb)
        assert(Vec4(y, z, y, x) == u.gbgr)
        assert(Vec4(y, z, y, y) == u.gbgg)
        assert(Vec4(y, z, y, z) == u.gbgb)
        assert(Vec4(y, z, z, x) == u.gbbr)
        assert(Vec4(y, z, z, y) == u.gbbg)
        assert(Vec4(y, z, z, z) == u.gbbb)
        assert(Vec4(z, x, x, x) == u.brrr)
        assert(Vec4(z, x, x, y) == u.brrg)
        assert(Vec4(z, x, x, z) == u.brrb)
        assert(Vec4(z, x, y, x) == u.brgr)
        assert(Vec4(z, x, y, y) == u.brgg)
        assert(Vec4(z, x, y, z) == u.brgb)
        assert(Vec4(z, x, z, x) == u.brbr)
        assert(Vec4(z, x, z, y) == u.brbg)
        assert(Vec4(z, x, z, z) == u.brbb)
        assert(Vec4(z, y, x, x) == u.bgrr)
        assert(Vec4(z, y, x, y) == u.bgrg)
        assert(Vec4(z, y, x, z) == u.bgrb)
        assert(Vec4(z, y, y, x) == u.bggr)
        assert(Vec4(z, y, y, y) == u.bggg)
        assert(Vec4(z, y, y, z) == u.bggb)
        assert(Vec4(z, y, z, x) == u.bgbr)
        assert(Vec4(z, y, z, y) == u.bgbg)
        assert(Vec4(z, y, z, z) == u.bgbb)
        assert(Vec4(z, z, x, x) == u.bbrr)
        assert(Vec4(z, z, x, y) == u.bbrg)
        assert(Vec4(z, z, x, z) == u.bbrb)
        assert(Vec4(z, z, y, x) == u.bbgr)
        assert(Vec4(z, z, y, y) == u.bbgg)
        assert(Vec4(z, z, y, z) == u.bbgb)
        assert(Vec4(z, z, z, x) == u.bbbr)
        assert(Vec4(z, z, z, y) == u.bbbg)
        assert(Vec4(z, z, z, z) == u.bbbb)

        assert(Vec2(x, x) == u.ss)
        assert(Vec2(x, y) == u.st)
        assert(Vec2(x, z) == u.sp)
        assert(Vec2(y, x) == u.ts)
        assert(Vec2(y, y) == u.tt)
        assert(Vec2(y, z) == u.tp)
        assert(Vec2(z, x) == u.ps)
        assert(Vec2(z, y) == u.pt)
        assert(Vec2(z, z) == u.pp)
        assert(Vec3(x, x, x) == u.sss)
        assert(Vec3(x, x, y) == u.sst)
        assert(Vec3(x, x, z) == u.ssp)
        assert(Vec3(x, y, x) == u.sts)
        assert(Vec3(x, y, y) == u.stt)
        assert(Vec3(x, y, z) == u.stp)
        assert(Vec3(x, z, x) == u.sps)
        assert(Vec3(x, z, y) == u.spt)
        assert(Vec3(x, z, z) == u.spp)
        assert(Vec3(y, x, x) == u.tss)
        assert(Vec3(y, x, y) == u.tst)
        assert(Vec3(y, x, z) == u.tsp)
        assert(Vec3(y, y, x) == u.tts)
        assert(Vec3(y, y, y) == u.ttt)
        assert(Vec3(y, y, z) == u.ttp)
        assert(Vec3(y, z, x) == u.tps)
        assert(Vec3(y, z, y) == u.tpt)
        assert(Vec3(y, z, z) == u.tpp)
        assert(Vec3(z, x, x) == u.pss)
        assert(Vec3(z, x, y) == u.pst)
        assert(Vec3(z, x, z) == u.psp)
        assert(Vec3(z, y, x) == u.pts)
        assert(Vec3(z, y, y) == u.ptt)
        assert(Vec3(z, y, z) == u.ptp)
        assert(Vec3(z, z, x) == u.pps)
        assert(Vec3(z, z, y) == u.ppt)
        assert(Vec3(z, z, z) == u.ppp)
        assert(Vec4(x, x, x, x) == u.ssss)
        assert(Vec4(x, x, x, y) == u.ssst)
        assert(Vec4(x, x, x, z) == u.sssp)
        assert(Vec4(x, x, y, x) == u.ssts)
        assert(Vec4(x, x, y, y) == u.sstt)
        assert(Vec4(x, x, y, z) == u.sstp)
        assert(Vec4(x, x, z, x) == u.ssps)
        assert(Vec4(x, x, z, y) == u.sspt)
        assert(Vec4(x, x, z, z) == u.sspp)
        assert(Vec4(x, y, x, x) == u.stss)
        assert(Vec4(x, y, x, y) == u.stst)
        assert(Vec4(x, y, x, z) == u.stsp)
        assert(Vec4(x, y, y, x) == u.stts)
        assert(Vec4(x, y, y, y) == u.sttt)
        assert(Vec4(x, y, y, z) == u.sttp)
        assert(Vec4(x, y, z, x) == u.stps)
        assert(Vec4(x, y, z, y) == u.stpt)
        assert(Vec4(x, y, z, z) == u.stpp)
        assert(Vec4(x, z, x, x) == u.spss)
        assert(Vec4(x, z, x, y) == u.spst)
        assert(Vec4(x, z, x, z) == u.spsp)
        assert(Vec4(x, z, y, x) == u.spts)
        assert(Vec4(x, z, y, y) == u.sptt)
        assert(Vec4(x, z, y, z) == u.sptp)
        assert(Vec4(x, z, z, x) == u.spps)
        assert(Vec4(x, z, z, y) == u.sppt)
        assert(Vec4(x, z, z, z) == u.sppp)
        assert(Vec4(y, x, x, x) == u.tsss)
        assert(Vec4(y, x, x, y) == u.tsst)
        assert(Vec4(y, x, x, z) == u.tssp)
        assert(Vec4(y, x, y, x) == u.tsts)
        assert(Vec4(y, x, y, y) == u.tstt)
        assert(Vec4(y, x, y, z) == u.tstp)
        assert(Vec4(y, x, z, x) == u.tsps)
        assert(Vec4(y, x, z, y) == u.tspt)
        assert(Vec4(y, x, z, z) == u.tspp)
        assert(Vec4(y, y, x, x) == u.ttss)
        assert(Vec4(y, y, x, y) == u.ttst)
        assert(Vec4(y, y, x, z) == u.ttsp)
        assert(Vec4(y, y, y, x) == u.ttts)
        assert(Vec4(y, y, y, y) == u.tttt)
        assert(Vec4(y, y, y, z) == u.tttp)
        assert(Vec4(y, y, z, x) == u.ttps)
        assert(Vec4(y, y, z, y) == u.ttpt)
        assert(Vec4(y, y, z, z) == u.ttpp)
        assert(Vec4(y, z, x, x) == u.tpss)
        assert(Vec4(y, z, x, y) == u.tpst)
        assert(Vec4(y, z, x, z) == u.tpsp)
        assert(Vec4(y, z, y, x) == u.tpts)
        assert(Vec4(y, z, y, y) == u.tptt)
        assert(Vec4(y, z, y, z) == u.tptp)
        assert(Vec4(y, z, z, x) == u.tpps)
        assert(Vec4(y, z, z, y) == u.tppt)
        assert(Vec4(y, z, z, z) == u.tppp)
        assert(Vec4(z, x, x, x) == u.psss)
        assert(Vec4(z, x, x, y) == u.psst)
        assert(Vec4(z, x, x, z) == u.pssp)
        assert(Vec4(z, x, y, x) == u.psts)
        assert(Vec4(z, x, y, y) == u.pstt)
        assert(Vec4(z, x, y, z) == u.pstp)
        assert(Vec4(z, x, z, x) == u.psps)
        assert(Vec4(z, x, z, y) == u.pspt)
        assert(Vec4(z, x, z, z) == u.pspp)
        assert(Vec4(z, y, x, x) == u.ptss)
        assert(Vec4(z, y, x, y) == u.ptst)
        assert(Vec4(z, y, x, z) == u.ptsp)
        assert(Vec4(z, y, y, x) == u.ptts)
        assert(Vec4(z, y, y, y) == u.pttt)
        assert(Vec4(z, y, y, z) == u.pttp)
        assert(Vec4(z, y, z, x) == u.ptps)
        assert(Vec4(z, y, z, y) == u.ptpt)
        assert(Vec4(z, y, z, z) == u.ptpp)
        assert(Vec4(z, z, x, x) == u.ppss)
        assert(Vec4(z, z, x, y) == u.ppst)
        assert(Vec4(z, z, x, z) == u.ppsp)
        assert(Vec4(z, z, y, x) == u.ppts)
        assert(Vec4(z, z, y, y) == u.pptt)
        assert(Vec4(z, z, y, z) == u.pptp)
        assert(Vec4(z, z, z, x) == u.ppps)
        assert(Vec4(z, z, z, y) == u.pppt)
        assert(Vec4(z, z, z, z) == u.pppp)
    }

    test("Swizzled write") {
        val x = 5d
        val y = 6d
        val z = 7d
        val t = 10d

        var i = ConstVec3(x, y, z)
        val u = Vec3(1)

        u := i; u.x = t; assert(Vec3(t, y, z) == u)
        u := i; u.y = t; assert(Vec3(x, t, z) == u)
        u := i; u.z = t; assert(Vec3(x, y, t) == u)

        u := i; u.r = t; assert(Vec3(t, y, z) == u)
        u := i; u.g = t; assert(Vec3(x, t, z) == u)
        u := i; u.b = t; assert(Vec3(x, y, t) == u)

        u := i; u.s = t; assert(Vec3(t, y, z) == u)
        u := i; u.t = t; assert(Vec3(x, t, z) == u)
        u := i; u.p = t; assert(Vec3(x, y, t) == u)

        i = Vec3(t)

        u := i; u.xy = Vec2(x, y); assert(Vec3(x, y, t) == u)
        u := i; u.xz = Vec2(x, z); assert(Vec3(x, t, z) == u)
        u := i; u.yx = Vec2(y, x); assert(Vec3(x, y, t) == u)
        u := i; u.yz = Vec2(y, z); assert(Vec3(t, y, z) == u)
        u := i; u.zx = Vec2(z, x); assert(Vec3(x, t, z) == u)
        u := i; u.zy = Vec2(z, y); assert(Vec3(t, y, z) == u)
        u := i; u.xyz = Vec3(x, y, z); assert(Vec3(x, y, z) == u)
        u := i; u.xzy = Vec3(x, z, y); assert(Vec3(x, y, z) == u)
        u := i; u.yxz = Vec3(y, x, z); assert(Vec3(x, y, z) == u)
        u := i; u.yzx = Vec3(y, z, x); assert(Vec3(x, y, z) == u)
        u := i; u.zxy = Vec3(z, x, y); assert(Vec3(x, y, z) == u)
        u := i; u.zyx = Vec3(z, y, x); assert(Vec3(x, y, z) == u)

        u := i; u.rg = Vec2(x, y); assert(Vec3(x, y, t) == u)
        u := i; u.rb = Vec2(x, z); assert(Vec3(x, t, z) == u)
        u := i; u.gr = Vec2(y, x); assert(Vec3(x, y, t) == u)
        u := i; u.gb = Vec2(y, z); assert(Vec3(t, y, z) == u)
        u := i; u.br = Vec2(z, x); assert(Vec3(x, t, z) == u)
        u := i; u.bg = Vec2(z, y); assert(Vec3(t, y, z) == u)
        u := i; u.rgb = Vec3(x, y, z); assert(Vec3(x, y, z) == u)
        u := i; u.rbg = Vec3(x, z, y); assert(Vec3(x, y, z) == u)
        u := i; u.grb = Vec3(y, x, z); assert(Vec3(x, y, z) == u)
        u := i; u.gbr = Vec3(y, z, x); assert(Vec3(x, y, z) == u)
        u := i; u.brg = Vec3(z, x, y); assert(Vec3(x, y, z) == u)
        u := i; u.bgr = Vec3(z, y, x); assert(Vec3(x, y, z) == u)

        u := i; u.st = Vec2(x, y); assert(Vec3(x, y, t) == u)
        u := i; u.sp = Vec2(x, z); assert(Vec3(x, t, z) == u)
        u := i; u.ts = Vec2(y, x); assert(Vec3(x, y, t) == u)
        u := i; u.tp = Vec2(y, z); assert(Vec3(t, y, z) == u)
        u := i; u.ps = Vec2(z, x); assert(Vec3(x, t, z) == u)
        u := i; u.pt = Vec2(z, y); assert(Vec3(t, y, z) == u)
        u := i; u.stp = Vec3(x, y, z); assert(Vec3(x, y, z) == u)
        u := i; u.spt = Vec3(x, z, y); assert(Vec3(x, y, z) == u)
        u := i; u.tsp = Vec3(y, x, z); assert(Vec3(x, y, z) == u)
        u := i; u.tps = Vec3(y, z, x); assert(Vec3(x, y, z) == u)
        u := i; u.pst = Vec3(z, x, y); assert(Vec3(x, y, z) == u)
        u := i; u.pts = Vec3(z, y, x); assert(Vec3(x, y, z) == u)
    }

    test("Swizzled self write") {
        val x = 5d
        val y = 6d
        val z = 7d

        val i = ConstVec3(x, y, z)
        val u = Vec3(0)

        u := i; u.xyz = u; assert(Vec3(x, y, z) == u)
        u := i; u.xzy = u; assert(Vec3(x, z, y) == u)
        u := i; u.yxz = u; assert(Vec3(y, x, z) == u)
        u := i; u.yzx = u; assert(Vec3(z, x, y) == u)
        u := i; u.zxy = u; assert(Vec3(y, z, x) == u)
        u := i; u.zyx = u; assert(Vec3(z, y, x) == u)

        u := i; u.rgb = u; assert(Vec3(x, y, z) == u)
        u := i; u.rbg = u; assert(Vec3(x, z, y) == u)
        u := i; u.grb = u; assert(Vec3(y, x, z) == u)
        u := i; u.gbr = u; assert(Vec3(z, x, y) == u)
        u := i; u.brg = u; assert(Vec3(y, z, x) == u)
        u := i; u.bgr = u; assert(Vec3(z, y, x) == u)

        u := i; u.stp = u; assert(Vec3(x, y, z) == u)
        u := i; u.spt = u; assert(Vec3(x, z, y) == u)
        u := i; u.tsp = u; assert(Vec3(y, x, z) == u)
        u := i; u.tps = u; assert(Vec3(z, x, y) == u)
        u := i; u.pst = u; assert(Vec3(y, z, x) == u)
        u := i; u.pts = u; assert(Vec3(z, y, x) == u)
    }

    // This test passes if it compiles
    test("Swizzle as property") {
        val u = Vec3(0)
        val i = Vec2(1)
        val j = Vec3(1)

        u.x += 1
        u.y += 1
        u.z += 1

        u.r += 1
        u.g += 1
        u.b += 1

        u.s += 1
        u.t += 1
        u.p += 1

        u.xy += i
        u.xz += i
        u.yx += i
        u.yz += i
        u.zx += i
        u.zy += i
        u.xyz += j
        u.xzy += j
        u.yxz += j
        u.yzx += j
        u.zxy += j
        u.zyx += j

        u.rg += i
        u.rb += i
        u.gr += i
        u.gb += i
        u.br += i
        u.bg += i
        u.rgb += j
        u.rbg += j
        u.grb += j
        u.gbr += j
        u.brg += j
        u.bgr += j

        u.st += i
        u.sp += i
        u.ts += i
        u.tp += i
        u.ps += i
        u.pt += i
        u.stp += j
        u.spt += j
        u.tsp += j
        u.tps += j
        u.pst += j
        u.pts += j
    }
}
