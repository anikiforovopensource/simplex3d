/*
 * Simplex3d, MathTest package
 * Copyright (C) 2009-2011, Aleksey Nikiforov
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

package test.math

import org.scalatest._

import simplex3d.math._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Vec2bSwizzleTest extends FunSuite {

  test("Swizzled read") {
  BooleanCombinations.test { (x, y, z, w) =>
    val u = ConstVec2b(x, y)

    expect(x) { u.x }
    expect(y) { u.y }

    expect(x) { u.r }
    expect(y) { u.g }

    expect(x) { u.s }
    expect(y) { u.t }

    assert(u.x.isInstanceOf[Boolean])
    assert(u.y.isInstanceOf[Boolean])

    assert(u.r.isInstanceOf[Boolean])
    assert(u.g.isInstanceOf[Boolean])

    assert(u.s.isInstanceOf[Boolean])
    assert(u.t.isInstanceOf[Boolean])

    assert(Vec2b(x, x) == u.xx)
    assert(Vec2b(x, y) == u.xy)
    assert(Vec2b(y, x) == u.yx)
    assert(Vec2b(y, y) == u.yy)

    assert(Vec3b(x, x, x) == u.xxx)
    assert(Vec3b(x, x, y) == u.xxy)
    assert(Vec3b(x, y, x) == u.xyx)
    assert(Vec3b(x, y, y) == u.xyy)
    assert(Vec3b(y, x, x) == u.yxx)
    assert(Vec3b(y, x, y) == u.yxy)
    assert(Vec3b(y, y, x) == u.yyx)
    assert(Vec3b(y, y, y) == u.yyy)

    assert(Vec4b(x, x, x, x) == u.xxxx)
    assert(Vec4b(x, x, x, y) == u.xxxy)
    assert(Vec4b(x, x, y, x) == u.xxyx)
    assert(Vec4b(x, x, y, y) == u.xxyy)
    assert(Vec4b(x, y, x, x) == u.xyxx)
    assert(Vec4b(x, y, x, y) == u.xyxy)
    assert(Vec4b(x, y, y, x) == u.xyyx)
    assert(Vec4b(x, y, y, y) == u.xyyy)
    assert(Vec4b(y, x, x, x) == u.yxxx)
    assert(Vec4b(y, x, x, y) == u.yxxy)
    assert(Vec4b(y, x, y, x) == u.yxyx)
    assert(Vec4b(y, x, y, y) == u.yxyy)
    assert(Vec4b(y, y, x, x) == u.yyxx)
    assert(Vec4b(y, y, x, y) == u.yyxy)
    assert(Vec4b(y, y, y, x) == u.yyyx)
    assert(Vec4b(y, y, y, y) == u.yyyy)

    assert(Vec2b(x, x) == u.rr)
    assert(Vec2b(x, y) == u.rg)
    assert(Vec2b(y, x) == u.gr)
    assert(Vec2b(y, y) == u.gg)

    assert(Vec3b(x, x, x) == u.rrr)
    assert(Vec3b(x, x, y) == u.rrg)
    assert(Vec3b(x, y, x) == u.rgr)
    assert(Vec3b(x, y, y) == u.rgg)
    assert(Vec3b(y, x, x) == u.grr)
    assert(Vec3b(y, x, y) == u.grg)
    assert(Vec3b(y, y, x) == u.ggr)
    assert(Vec3b(y, y, y) == u.ggg)

    assert(Vec4b(x, x, x, x) == u.rrrr)
    assert(Vec4b(x, x, x, y) == u.rrrg)
    assert(Vec4b(x, x, y, x) == u.rrgr)
    assert(Vec4b(x, x, y, y) == u.rrgg)
    assert(Vec4b(x, y, x, x) == u.rgrr)
    assert(Vec4b(x, y, x, y) == u.rgrg)
    assert(Vec4b(x, y, y, x) == u.rggr)
    assert(Vec4b(x, y, y, y) == u.rggg)
    assert(Vec4b(y, x, x, x) == u.grrr)
    assert(Vec4b(y, x, x, y) == u.grrg)
    assert(Vec4b(y, x, y, x) == u.grgr)
    assert(Vec4b(y, x, y, y) == u.grgg)
    assert(Vec4b(y, y, x, x) == u.ggrr)
    assert(Vec4b(y, y, x, y) == u.ggrg)
    assert(Vec4b(y, y, y, x) == u.gggr)
    assert(Vec4b(y, y, y, y) == u.gggg)

    assert(Vec2b(x, x) == u.ss)
    assert(Vec2b(x, y) == u.st)
    assert(Vec2b(y, x) == u.ts)
    assert(Vec2b(y, y) == u.tt)

    assert(Vec3b(x, x, x) == u.sss)
    assert(Vec3b(x, x, y) == u.sst)
    assert(Vec3b(x, y, x) == u.sts)
    assert(Vec3b(x, y, y) == u.stt)
    assert(Vec3b(y, x, x) == u.tss)
    assert(Vec3b(y, x, y) == u.tst)
    assert(Vec3b(y, y, x) == u.tts)
    assert(Vec3b(y, y, y) == u.ttt)

    assert(Vec4b(x, x, x, x) == u.ssss)
    assert(Vec4b(x, x, x, y) == u.ssst)
    assert(Vec4b(x, x, y, x) == u.ssts)
    assert(Vec4b(x, x, y, y) == u.sstt)
    assert(Vec4b(x, y, x, x) == u.stss)
    assert(Vec4b(x, y, x, y) == u.stst)
    assert(Vec4b(x, y, y, x) == u.stts)
    assert(Vec4b(x, y, y, y) == u.sttt)
    assert(Vec4b(y, x, x, x) == u.tsss)
    assert(Vec4b(y, x, x, y) == u.tsst)
    assert(Vec4b(y, x, y, x) == u.tsts)
    assert(Vec4b(y, x, y, y) == u.tstt)
    assert(Vec4b(y, y, x, x) == u.ttss)
    assert(Vec4b(y, y, x, y) == u.ttst)
    assert(Vec4b(y, y, y, x) == u.ttts)
    assert(Vec4b(y, y, y, y) == u.tttt)
  }}

  test("Swizzled write") {
  BooleanCombinations.test { (x, y, z, w) =>
    val t = !x

    var i = ConstVec2b(x, y)
    val u = Vec2b(false)

    u := i; u.x = t; assert(Vec2b(t, y) == u)
    u := i; u.y = t; assert(Vec2b(x, t) == u)

    u := i; u.r = t; assert(Vec2b(t, y) == u)
    u := i; u.g = t; assert(Vec2b(x, t) == u)

    u := i; u.s = t; assert(Vec2b(t, y) == u)
    u := i; u.t = t; assert(Vec2b(x, t) == u)

    i = Vec2b(t)

    u := i; u.xy = Vec2b(x, y); assert(Vec2b(x, y) == u)
    u := i; u.yx = Vec2b(x, y); assert(Vec2b(y, x) == u)

    u := i; u.rg = Vec2b(x, y); assert(Vec2b(x, y) == u)
    u := i; u.gr = Vec2b(x, y); assert(Vec2b(y, x) == u)

    u := i; u.st = Vec2b(x, y); assert(Vec2b(x, y) == u)
    u := i; u.ts = Vec2b(x, y); assert(Vec2b(y, x) == u)
  }}

  test("Swizzled self write") {
  BooleanCombinations.test { (x, y, z, w) =>
    val i = ConstVec2b(x, y)
    val u = Vec2b(false)

    u := i; u.xy = u; assert(Vec2b(x, y) == u)
    u := i; u.yx = u; assert(Vec2b(y, x) == u)

    u := i; u.rg = u; assert(Vec2b(x, y) == u)
    u := i; u.gr = u; assert(Vec2b(y, x) == u)

    u := i; u.st = u; assert(Vec2b(x, y) == u)
    u := i; u.ts = u; assert(Vec2b(y, x) == u)
  }}

  // This test passes if it compiles
  test("Swizzle as property") {
    val u = Vec2b(false)

    u.x ||= true
    u.y ||= true

    u.r ||= true
    u.g ||= true

    u.s ||= true
    u.t ||= true
  }
}
