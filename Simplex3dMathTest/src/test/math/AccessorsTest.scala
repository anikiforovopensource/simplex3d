/*
 * Simplex3d, MathTest package
 * Copyright (C) 2011, Aleksey Nikiforov
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
import simplex3d.math.floatx._
import simplex3d.math.doublex._
import simplex3d.math.doublex.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
class AccessorsTest extends FunSuite {

  val random = new java.util.Random(1)
  def rb = random.nextBoolean
  def ri = random.nextInt
  def rf = random.nextFloat
  def rd = random.nextDouble


  test("Vector Accessors") {
    for (i <- 0 until 100) {
      val vb1 = new BooleanRef(rb)

      assert(Accessors.bx(vb1) == toBoolean(vb1.toConst))
      assert(Accessors.ix(vb1) == toInt(vb1.toConst))
      assert(Accessors.fx(vb1) == toFloat(vb1.toConst))
      assert(Accessors.dx(vb1) == toDouble(vb1.toConst))

      val vb2 = Vec2b(rb, rb)

      assert(Accessors.bx(vb2) == toBoolean(vb2.x))
      assert(Accessors.by(vb2) == toBoolean(vb2.y))

      assert(Accessors.ix(vb2) == toInt(vb2.x))
      assert(Accessors.iy(vb2) == toInt(vb2.y))

      assert(Accessors.fx(vb2) == toFloat(vb2.x))
      assert(Accessors.fy(vb2) == toFloat(vb2.y))

      assert(Accessors.dx(vb2) == toDouble(vb2.x))
      assert(Accessors.dy(vb2) == toDouble(vb2.y))

      val vb3 = Vec3b(rb, rb, rb)

      assert(Accessors.bx(vb3) == toBoolean(vb3.x))
      assert(Accessors.by(vb3) == toBoolean(vb3.y))
      assert(Accessors.bz(vb3) == toBoolean(vb3.z))

      assert(Accessors.ix(vb3) == toInt(vb3.x))
      assert(Accessors.iy(vb3) == toInt(vb3.y))
      assert(Accessors.iz(vb3) == toInt(vb3.z))

      assert(Accessors.fx(vb3) == toFloat(vb3.x))
      assert(Accessors.fy(vb3) == toFloat(vb3.y))
      assert(Accessors.fz(vb3) == toFloat(vb3.z))

      assert(Accessors.dx(vb3) == toDouble(vb3.x))
      assert(Accessors.dy(vb3) == toDouble(vb3.y))
      assert(Accessors.dz(vb3) == toDouble(vb3.z))

      val vb4 = Vec4b(rb, rb, rb, rb)

      assert(Accessors.bx(vb4) == toBoolean(vb4.x))
      assert(Accessors.by(vb4) == toBoolean(vb4.y))
      assert(Accessors.bz(vb4) == toBoolean(vb4.z))
      assert(Accessors.bw(vb4) == toBoolean(vb4.w))

      assert(Accessors.ix(vb4) == toInt(vb4.x))
      assert(Accessors.iy(vb4) == toInt(vb4.y))
      assert(Accessors.iz(vb4) == toInt(vb4.z))
      assert(Accessors.iw(vb4) == toInt(vb4.w))

      assert(Accessors.fx(vb4) == toFloat(vb4.x))
      assert(Accessors.fy(vb4) == toFloat(vb4.y))
      assert(Accessors.fz(vb4) == toFloat(vb4.z))
      assert(Accessors.fw(vb4) == toFloat(vb4.w))

      assert(Accessors.dx(vb4) == toDouble(vb4.x))
      assert(Accessors.dy(vb4) == toDouble(vb4.y))
      assert(Accessors.dz(vb4) == toDouble(vb4.z))
      assert(Accessors.dw(vb4) == toDouble(vb4.w))


      val vi1 = new IntRef(ri)

      assert(Accessors.bx(vi1) == toBoolean(vi1.toConst))
      assert(Accessors.ix(vi1) == toInt(vi1.toConst))
      assert(Accessors.fx(vi1) == toFloat(vi1.toConst))
      assert(Accessors.dx(vi1) == toDouble(vi1.toConst))

      val vi2 = Vec2i(ri, ri)

      assert(Accessors.bx(vi2) == toBoolean(vi2.x))
      assert(Accessors.by(vi2) == toBoolean(vi2.y))

      assert(Accessors.ix(vi2) == toInt(vi2.x))
      assert(Accessors.iy(vi2) == toInt(vi2.y))

      assert(Accessors.fx(vi2) == toFloat(vi2.x))
      assert(Accessors.fy(vi2) == toFloat(vi2.y))

      assert(Accessors.dx(vi2) == toDouble(vi2.x))
      assert(Accessors.dy(vi2) == toDouble(vi2.y))

      val vi3 = Vec3i(ri, ri, ri)

      assert(Accessors.bx(vi3) == toBoolean(vi3.x))
      assert(Accessors.by(vi3) == toBoolean(vi3.y))
      assert(Accessors.bz(vi3) == toBoolean(vi3.z))

      assert(Accessors.ix(vi3) == toInt(vi3.x))
      assert(Accessors.iy(vi3) == toInt(vi3.y))
      assert(Accessors.iz(vi3) == toInt(vi3.z))

      assert(Accessors.fx(vi3) == toFloat(vi3.x))
      assert(Accessors.fy(vi3) == toFloat(vi3.y))
      assert(Accessors.fz(vi3) == toFloat(vi3.z))

      assert(Accessors.dx(vi3) == toDouble(vi3.x))
      assert(Accessors.dy(vi3) == toDouble(vi3.y))
      assert(Accessors.dz(vi3) == toDouble(vi3.z))

      val vi4 = Vec4i(ri, ri, ri, ri)

      assert(Accessors.bx(vi4) == toBoolean(vi4.x))
      assert(Accessors.by(vi4) == toBoolean(vi4.y))
      assert(Accessors.bz(vi4) == toBoolean(vi4.z))
      assert(Accessors.bw(vi4) == toBoolean(vi4.w))

      assert(Accessors.ix(vi4) == toInt(vi4.x))
      assert(Accessors.iy(vi4) == toInt(vi4.y))
      assert(Accessors.iz(vi4) == toInt(vi4.z))
      assert(Accessors.iw(vi4) == toInt(vi4.w))

      assert(Accessors.fx(vi4) == toFloat(vi4.x))
      assert(Accessors.fy(vi4) == toFloat(vi4.y))
      assert(Accessors.fz(vi4) == toFloat(vi4.z))
      assert(Accessors.fw(vi4) == toFloat(vi4.w))

      assert(Accessors.dx(vi4) == toDouble(vi4.x))
      assert(Accessors.dy(vi4) == toDouble(vi4.y))
      assert(Accessors.dz(vi4) == toDouble(vi4.z))
      assert(Accessors.dw(vi4) == toDouble(vi4.w))


      val vf1 = new FloatRef(rf)

      assert(Accessors.bx(vf1) == toBoolean(vf1.toConst))
      assert(Accessors.ix(vf1) == toInt(vf1.toConst))
      assert(Accessors.fx(vf1) == toFloat(vf1.toConst))
      assert(Accessors.dx(vf1) == toDouble(vf1.toConst))

      val vf2 = Vec2f(rf, rf)

      assert(Accessors.bx(vf2) == toBoolean(vf2.x))
      assert(Accessors.by(vf2) == toBoolean(vf2.y))

      assert(Accessors.ix(vf2) == toInt(vf2.x))
      assert(Accessors.iy(vf2) == toInt(vf2.y))

      assert(Accessors.fx(vf2) == toFloat(vf2.x))
      assert(Accessors.fy(vf2) == toFloat(vf2.y))

      assert(Accessors.dx(vf2) == toDouble(vf2.x))
      assert(Accessors.dy(vf2) == toDouble(vf2.y))

      val vf3 = Vec3f(rf, rf, rf)

      assert(Accessors.bx(vf3) == toBoolean(vf3.x))
      assert(Accessors.by(vf3) == toBoolean(vf3.y))
      assert(Accessors.bz(vf3) == toBoolean(vf3.z))

      assert(Accessors.ix(vf3) == toInt(vf3.x))
      assert(Accessors.iy(vf3) == toInt(vf3.y))
      assert(Accessors.iz(vf3) == toInt(vf3.z))

      assert(Accessors.fx(vf3) == toFloat(vf3.x))
      assert(Accessors.fy(vf3) == toFloat(vf3.y))
      assert(Accessors.fz(vf3) == toFloat(vf3.z))

      assert(Accessors.dx(vf3) == toDouble(vf3.x))
      assert(Accessors.dy(vf3) == toDouble(vf3.y))
      assert(Accessors.dz(vf3) == toDouble(vf3.z))

      val vf4 = Vec4f(rf, rf, rf, rf)

      assert(Accessors.bx(vf4) == toBoolean(vf4.x))
      assert(Accessors.by(vf4) == toBoolean(vf4.y))
      assert(Accessors.bz(vf4) == toBoolean(vf4.z))
      assert(Accessors.bw(vf4) == toBoolean(vf4.w))

      assert(Accessors.ix(vf4) == toInt(vf4.x))
      assert(Accessors.iy(vf4) == toInt(vf4.y))
      assert(Accessors.iz(vf4) == toInt(vf4.z))
      assert(Accessors.iw(vf4) == toInt(vf4.w))

      assert(Accessors.fx(vf4) == toFloat(vf4.x))
      assert(Accessors.fy(vf4) == toFloat(vf4.y))
      assert(Accessors.fz(vf4) == toFloat(vf4.z))
      assert(Accessors.fw(vf4) == toFloat(vf4.w))

      assert(Accessors.dx(vf4) == toDouble(vf4.x))
      assert(Accessors.dy(vf4) == toDouble(vf4.y))
      assert(Accessors.dz(vf4) == toDouble(vf4.z))
      assert(Accessors.dw(vf4) == toDouble(vf4.w))


      val vd1 = new DoubleRef(rd)

      assert(Accessors.bx(vd1) == toBoolean(vd1.toConst))
      assert(Accessors.ix(vd1) == toInt(vd1.toConst))
      assert(Accessors.fx(vd1) == toFloat(vd1.toConst))
      assert(Accessors.dx(vd1) == toDouble(vd1.toConst))

      val vd2 = Vec2d(rd, rd)

      assert(Accessors.bx(vd2) == toBoolean(vd2.x))
      assert(Accessors.by(vd2) == toBoolean(vd2.y))

      assert(Accessors.ix(vd2) == toInt(vd2.x))
      assert(Accessors.iy(vd2) == toInt(vd2.y))

      assert(Accessors.fx(vd2) == toFloat(vd2.x))
      assert(Accessors.fy(vd2) == toFloat(vd2.y))

      assert(Accessors.dx(vd2) == toDouble(vd2.x))
      assert(Accessors.dy(vd2) == toDouble(vd2.y))

      val vd3 = Vec3d(rd, rd, rd)

      assert(Accessors.bx(vd3) == toBoolean(vd3.x))
      assert(Accessors.by(vd3) == toBoolean(vd3.y))
      assert(Accessors.bz(vd3) == toBoolean(vd3.z))

      assert(Accessors.ix(vd3) == toInt(vd3.x))
      assert(Accessors.iy(vd3) == toInt(vd3.y))
      assert(Accessors.iz(vd3) == toInt(vd3.z))

      assert(Accessors.fx(vd3) == toFloat(vd3.x))
      assert(Accessors.fy(vd3) == toFloat(vd3.y))
      assert(Accessors.fz(vd3) == toFloat(vd3.z))

      assert(Accessors.dx(vd3) == toDouble(vd3.x))
      assert(Accessors.dy(vd3) == toDouble(vd3.y))
      assert(Accessors.dz(vd3) == toDouble(vd3.z))

      val vd4 = Vec4d(rd, rd, rd, rd)

      assert(Accessors.bx(vd4) == toBoolean(vd4.x))
      assert(Accessors.by(vd4) == toBoolean(vd4.y))
      assert(Accessors.bz(vd4) == toBoolean(vd4.z))
      assert(Accessors.bw(vd4) == toBoolean(vd4.w))

      assert(Accessors.ix(vd4) == toInt(vd4.x))
      assert(Accessors.iy(vd4) == toInt(vd4.y))
      assert(Accessors.iz(vd4) == toInt(vd4.z))
      assert(Accessors.iw(vd4) == toInt(vd4.w))

      assert(Accessors.fx(vd4) == toFloat(vd4.x))
      assert(Accessors.fy(vd4) == toFloat(vd4.y))
      assert(Accessors.fz(vd4) == toFloat(vd4.z))
      assert(Accessors.fw(vd4) == toFloat(vd4.w))

      assert(Accessors.dx(vd4) == toDouble(vd4.x))
      assert(Accessors.dy(vd4) == toDouble(vd4.y))
      assert(Accessors.dz(vd4) == toDouble(vd4.z))
      assert(Accessors.dw(vd4) == toDouble(vd4.w))
    }
  }

  test("Quaternion Accessors") {
    for (i <- 0 until 100) {
      val qf = Quat4f(rf, rf, rf, rf)

      assert(Accessors.fa(qf) == toFloat(qf.a))
      assert(Accessors.fb(qf) == toFloat(qf.b))
      assert(Accessors.fc(qf) == toFloat(qf.c))
      assert(Accessors.fd(qf) == toFloat(qf.d))

      assert(Accessors.da(qf) == toDouble(qf.a))
      assert(Accessors.db(qf) == toDouble(qf.b))
      assert(Accessors.dc(qf) == toDouble(qf.c))
      assert(Accessors.dd(qf) == toDouble(qf.d))


      val qd = Quat4d(rd, rd, rd, rd)

      assert(Accessors.fa(qd) == toFloat(qd.a))
      assert(Accessors.fb(qd) == toFloat(qd.b))
      assert(Accessors.fc(qd) == toFloat(qd.c))
      assert(Accessors.fd(qd) == toFloat(qd.d))

      assert(Accessors.da(qd) == toDouble(qd.a))
      assert(Accessors.db(qd) == toDouble(qd.b))
      assert(Accessors.dc(qd) == toDouble(qd.c))
      assert(Accessors.dd(qd) == toDouble(qd.d))
    }
  }

  test("Matrix Accessors") {
    for (i <- 0 until 100) {
      val mf = Mat4x4f(
        rf, rf, rf, rf,
        rf, rf, rf, rf,
        rf, rf, rf, rf,
        rf, rf, rf, rf
      )

      val m22 = Mat2x2f(mf)
      val a22 = Mat4x4f(
        Accessors.f00(m22), Accessors.f10(m22), Accessors.f20(m22), Accessors.f30(m22),
        Accessors.f01(m22), Accessors.f11(m22), Accessors.f21(m22), Accessors.f31(m22),
        Accessors.f02(m22), Accessors.f12(m22), Accessors.f22(m22), Accessors.f32(m22),
        Accessors.f03(m22), Accessors.f13(m22), Accessors.f23(m22), Accessors.f33(m22)
      )
      assert(a22 == Mat4x4f(m22))
      val b22 = Mat4x4d(
        Accessors.d00(m22), Accessors.d10(m22), Accessors.d20(m22), Accessors.d30(m22),
        Accessors.d01(m22), Accessors.d11(m22), Accessors.d21(m22), Accessors.d31(m22),
        Accessors.d02(m22), Accessors.d12(m22), Accessors.d22(m22), Accessors.d32(m22),
        Accessors.d03(m22), Accessors.d13(m22), Accessors.d23(m22), Accessors.d33(m22)
      )
      assert(b22 == Mat4x4d(m22))


      val m23 = Mat2x3f(mf)
      val a23 = Mat4x4f(
        Accessors.f00(m23), Accessors.f10(m23), Accessors.f20(m23), Accessors.f30(m23),
        Accessors.f01(m23), Accessors.f11(m23), Accessors.f21(m23), Accessors.f31(m23),
        Accessors.f02(m23), Accessors.f12(m23), Accessors.f22(m23), Accessors.f32(m23),
        Accessors.f03(m23), Accessors.f13(m23), Accessors.f23(m23), Accessors.f33(m23)
      )
      assert(a23 == Mat4x4f(m23))
      val b23 = Mat4x4d(
        Accessors.d00(m23), Accessors.d10(m23), Accessors.d20(m23), Accessors.d30(m23),
        Accessors.d01(m23), Accessors.d11(m23), Accessors.d21(m23), Accessors.d31(m23),
        Accessors.d02(m23), Accessors.d12(m23), Accessors.d22(m23), Accessors.d32(m23),
        Accessors.d03(m23), Accessors.d13(m23), Accessors.d23(m23), Accessors.d33(m23)
      )
      assert(b23 == Mat4x4d(m23))


      val m24 = Mat2x4f(mf)
      val a24 = Mat4x4f(
        Accessors.f00(m24), Accessors.f10(m24), Accessors.f20(m24), Accessors.f30(m24),
        Accessors.f01(m24), Accessors.f11(m24), Accessors.f21(m24), Accessors.f31(m24),
        Accessors.f02(m24), Accessors.f12(m24), Accessors.f22(m24), Accessors.f32(m24),
        Accessors.f03(m24), Accessors.f13(m24), Accessors.f23(m24), Accessors.f33(m24)
      )
      assert(a24 == Mat4x4f(m24))
      val b24 = Mat4x4d(
        Accessors.d00(m24), Accessors.d10(m24), Accessors.d20(m24), Accessors.d30(m24),
        Accessors.d01(m24), Accessors.d11(m24), Accessors.d21(m24), Accessors.d31(m24),
        Accessors.d02(m24), Accessors.d12(m24), Accessors.d22(m24), Accessors.d32(m24),
        Accessors.d03(m24), Accessors.d13(m24), Accessors.d23(m24), Accessors.d33(m24)
      )
      assert(b24 == Mat4x4d(m24))


      val m32 = Mat3x2f(mf)
      val a32 = Mat4x4f(
        Accessors.f00(m32), Accessors.f10(m32), Accessors.f20(m32), Accessors.f30(m32),
        Accessors.f01(m32), Accessors.f11(m32), Accessors.f21(m32), Accessors.f31(m32),
        Accessors.f02(m32), Accessors.f12(m32), Accessors.f22(m32), Accessors.f32(m32),
        Accessors.f03(m32), Accessors.f13(m32), Accessors.f23(m32), Accessors.f33(m32)
      )
      assert(a32 == Mat4x4f(m32))
      val b32 = Mat4x4d(
        Accessors.d00(m32), Accessors.d10(m32), Accessors.d20(m32), Accessors.d30(m32),
        Accessors.d01(m32), Accessors.d11(m32), Accessors.d21(m32), Accessors.d31(m32),
        Accessors.d02(m32), Accessors.d12(m32), Accessors.d22(m32), Accessors.d32(m32),
        Accessors.d03(m32), Accessors.d13(m32), Accessors.d23(m32), Accessors.d33(m32)
      )
      assert(b32 == Mat4x4d(m32))


      val m33 = Mat3x3f(mf)
      val a33 = Mat4x4f(
        Accessors.f00(m33), Accessors.f10(m33), Accessors.f20(m33), Accessors.f30(m33),
        Accessors.f01(m33), Accessors.f11(m33), Accessors.f21(m33), Accessors.f31(m33),
        Accessors.f02(m33), Accessors.f12(m33), Accessors.f22(m33), Accessors.f32(m33),
        Accessors.f03(m33), Accessors.f13(m33), Accessors.f23(m33), Accessors.f33(m33)
      )
      assert(a33 == Mat4x4f(m33))
      val b33 = Mat4x4d(
        Accessors.d00(m33), Accessors.d10(m33), Accessors.d20(m33), Accessors.d30(m33),
        Accessors.d01(m33), Accessors.d11(m33), Accessors.d21(m33), Accessors.d31(m33),
        Accessors.d02(m33), Accessors.d12(m33), Accessors.d22(m33), Accessors.d32(m33),
        Accessors.d03(m33), Accessors.d13(m33), Accessors.d23(m33), Accessors.d33(m33)
      )
      assert(b33 == Mat4x4d(m33))


      val m34 = Mat3x4f(mf)
      val a34 = Mat4x4f(
        Accessors.f00(m34), Accessors.f10(m34), Accessors.f20(m34), Accessors.f30(m34),
        Accessors.f01(m34), Accessors.f11(m34), Accessors.f21(m34), Accessors.f31(m34),
        Accessors.f02(m34), Accessors.f12(m34), Accessors.f22(m34), Accessors.f32(m34),
        Accessors.f03(m34), Accessors.f13(m34), Accessors.f23(m34), Accessors.f33(m34)
      )
      assert(a34 == Mat4x4f(m34))
      val b34 = Mat4x4d(
        Accessors.d00(m34), Accessors.d10(m34), Accessors.d20(m34), Accessors.d30(m34),
        Accessors.d01(m34), Accessors.d11(m34), Accessors.d21(m34), Accessors.d31(m34),
        Accessors.d02(m34), Accessors.d12(m34), Accessors.d22(m34), Accessors.d32(m34),
        Accessors.d03(m34), Accessors.d13(m34), Accessors.d23(m34), Accessors.d33(m34)
      )
      assert(b34 == Mat4x4d(m34))


      val m42 = Mat4x2f(mf)
      val a42 = Mat4x4f(
        Accessors.f00(m42), Accessors.f10(m42), Accessors.f20(m42), Accessors.f30(m42),
        Accessors.f01(m42), Accessors.f11(m42), Accessors.f21(m42), Accessors.f31(m42),
        Accessors.f02(m42), Accessors.f12(m42), Accessors.f22(m42), Accessors.f32(m42),
        Accessors.f03(m42), Accessors.f13(m42), Accessors.f23(m42), Accessors.f33(m42)
      )
      assert(a42 == Mat4x4f(m42))
      val b42 = Mat4x4d(
        Accessors.d00(m42), Accessors.d10(m42), Accessors.d20(m42), Accessors.d30(m42),
        Accessors.d01(m42), Accessors.d11(m42), Accessors.d21(m42), Accessors.d31(m42),
        Accessors.d02(m42), Accessors.d12(m42), Accessors.d22(m42), Accessors.d32(m42),
        Accessors.d03(m42), Accessors.d13(m42), Accessors.d23(m42), Accessors.d33(m42)
      )
      assert(b42 == Mat4x4d(m42))


      val m43 = Mat4x3f(mf)
      val a43 = Mat4x4f(
        Accessors.f00(m43), Accessors.f10(m43), Accessors.f20(m43), Accessors.f30(m43),
        Accessors.f01(m43), Accessors.f11(m43), Accessors.f21(m43), Accessors.f31(m43),
        Accessors.f02(m43), Accessors.f12(m43), Accessors.f22(m43), Accessors.f32(m43),
        Accessors.f03(m43), Accessors.f13(m43), Accessors.f23(m43), Accessors.f33(m43)
      )
      assert(a43 == Mat4x4f(m43))
      val b43 = Mat4x4d(
        Accessors.d00(m43), Accessors.d10(m43), Accessors.d20(m43), Accessors.d30(m43),
        Accessors.d01(m43), Accessors.d11(m43), Accessors.d21(m43), Accessors.d31(m43),
        Accessors.d02(m43), Accessors.d12(m43), Accessors.d22(m43), Accessors.d32(m43),
        Accessors.d03(m43), Accessors.d13(m43), Accessors.d23(m43), Accessors.d33(m43)
      )
      assert(b43 == Mat4x4d(m43))


      val m44 = Mat4x4f(mf)
      val a44 = Mat4x4f(
        Accessors.f00(m44), Accessors.f10(m44), Accessors.f20(m44), Accessors.f30(m44),
        Accessors.f01(m44), Accessors.f11(m44), Accessors.f21(m44), Accessors.f31(m44),
        Accessors.f02(m44), Accessors.f12(m44), Accessors.f22(m44), Accessors.f32(m44),
        Accessors.f03(m44), Accessors.f13(m44), Accessors.f23(m44), Accessors.f33(m44)
      )
      assert(a44 == Mat4x4f(m44))
      val b44 = Mat4x4d(
        Accessors.d00(m44), Accessors.d10(m44), Accessors.d20(m44), Accessors.d30(m44),
        Accessors.d01(m44), Accessors.d11(m44), Accessors.d21(m44), Accessors.d31(m44),
        Accessors.d02(m44), Accessors.d12(m44), Accessors.d22(m44), Accessors.d32(m44),
        Accessors.d03(m44), Accessors.d13(m44), Accessors.d23(m44), Accessors.d33(m44)
      )
      assert(b44 == Mat4x4d(m44))
    }


    for (i <- 0 until 100) {
      val md = Mat4x4d(
        rd, rd, rd, rd,
        rd, rd, rd, rd,
        rd, rd, rd, rd,
        rd, rd, rd, rd
      )

      val m22 = Mat2x2d(md)
      val a22 = Mat4x4f(
        Accessors.f00(m22), Accessors.f10(m22), Accessors.f20(m22), Accessors.f30(m22),
        Accessors.f01(m22), Accessors.f11(m22), Accessors.f21(m22), Accessors.f31(m22),
        Accessors.f02(m22), Accessors.f12(m22), Accessors.f22(m22), Accessors.f32(m22),
        Accessors.f03(m22), Accessors.f13(m22), Accessors.f23(m22), Accessors.f33(m22)
      )
      assert(a22 == Mat4x4f(m22))
      val b22 = Mat4x4d(
        Accessors.d00(m22), Accessors.d10(m22), Accessors.d20(m22), Accessors.d30(m22),
        Accessors.d01(m22), Accessors.d11(m22), Accessors.d21(m22), Accessors.d31(m22),
        Accessors.d02(m22), Accessors.d12(m22), Accessors.d22(m22), Accessors.d32(m22),
        Accessors.d03(m22), Accessors.d13(m22), Accessors.d23(m22), Accessors.d33(m22)
      )
      assert(b22 == Mat4x4d(m22))


      val m23 = Mat2x3d(md)
      val a23 = Mat4x4f(
        Accessors.f00(m23), Accessors.f10(m23), Accessors.f20(m23), Accessors.f30(m23),
        Accessors.f01(m23), Accessors.f11(m23), Accessors.f21(m23), Accessors.f31(m23),
        Accessors.f02(m23), Accessors.f12(m23), Accessors.f22(m23), Accessors.f32(m23),
        Accessors.f03(m23), Accessors.f13(m23), Accessors.f23(m23), Accessors.f33(m23)
      )
      assert(a23 == Mat4x4f(m23))
      val b23 = Mat4x4d(
        Accessors.d00(m23), Accessors.d10(m23), Accessors.d20(m23), Accessors.d30(m23),
        Accessors.d01(m23), Accessors.d11(m23), Accessors.d21(m23), Accessors.d31(m23),
        Accessors.d02(m23), Accessors.d12(m23), Accessors.d22(m23), Accessors.d32(m23),
        Accessors.d03(m23), Accessors.d13(m23), Accessors.d23(m23), Accessors.d33(m23)
      )
      assert(b23 == Mat4x4d(m23))


      val m24 = Mat2x4d(md)
      val a24 = Mat4x4f(
        Accessors.f00(m24), Accessors.f10(m24), Accessors.f20(m24), Accessors.f30(m24),
        Accessors.f01(m24), Accessors.f11(m24), Accessors.f21(m24), Accessors.f31(m24),
        Accessors.f02(m24), Accessors.f12(m24), Accessors.f22(m24), Accessors.f32(m24),
        Accessors.f03(m24), Accessors.f13(m24), Accessors.f23(m24), Accessors.f33(m24)
      )
      assert(a24 == Mat4x4f(m24))
      val b24 = Mat4x4d(
        Accessors.d00(m24), Accessors.d10(m24), Accessors.d20(m24), Accessors.d30(m24),
        Accessors.d01(m24), Accessors.d11(m24), Accessors.d21(m24), Accessors.d31(m24),
        Accessors.d02(m24), Accessors.d12(m24), Accessors.d22(m24), Accessors.d32(m24),
        Accessors.d03(m24), Accessors.d13(m24), Accessors.d23(m24), Accessors.d33(m24)
      )
      assert(b24 == Mat4x4d(m24))


      val m32 = Mat3x2d(md)
      val a32 = Mat4x4f(
        Accessors.f00(m32), Accessors.f10(m32), Accessors.f20(m32), Accessors.f30(m32),
        Accessors.f01(m32), Accessors.f11(m32), Accessors.f21(m32), Accessors.f31(m32),
        Accessors.f02(m32), Accessors.f12(m32), Accessors.f22(m32), Accessors.f32(m32),
        Accessors.f03(m32), Accessors.f13(m32), Accessors.f23(m32), Accessors.f33(m32)
      )
      assert(a32 == Mat4x4f(m32))
      val b32 = Mat4x4d(
        Accessors.d00(m32), Accessors.d10(m32), Accessors.d20(m32), Accessors.d30(m32),
        Accessors.d01(m32), Accessors.d11(m32), Accessors.d21(m32), Accessors.d31(m32),
        Accessors.d02(m32), Accessors.d12(m32), Accessors.d22(m32), Accessors.d32(m32),
        Accessors.d03(m32), Accessors.d13(m32), Accessors.d23(m32), Accessors.d33(m32)
      )
      assert(b32 == Mat4x4d(m32))


      val m33 = Mat3x3d(md)
      val a33 = Mat4x4f(
        Accessors.f00(m33), Accessors.f10(m33), Accessors.f20(m33), Accessors.f30(m33),
        Accessors.f01(m33), Accessors.f11(m33), Accessors.f21(m33), Accessors.f31(m33),
        Accessors.f02(m33), Accessors.f12(m33), Accessors.f22(m33), Accessors.f32(m33),
        Accessors.f03(m33), Accessors.f13(m33), Accessors.f23(m33), Accessors.f33(m33)
      )
      assert(a33 == Mat4x4f(m33))
      val b33 = Mat4x4d(
        Accessors.d00(m33), Accessors.d10(m33), Accessors.d20(m33), Accessors.d30(m33),
        Accessors.d01(m33), Accessors.d11(m33), Accessors.d21(m33), Accessors.d31(m33),
        Accessors.d02(m33), Accessors.d12(m33), Accessors.d22(m33), Accessors.d32(m33),
        Accessors.d03(m33), Accessors.d13(m33), Accessors.d23(m33), Accessors.d33(m33)
      )
      assert(b33 == Mat4x4d(m33))


      val m34 = Mat3x4d(md)
      val a34 = Mat4x4f(
        Accessors.f00(m34), Accessors.f10(m34), Accessors.f20(m34), Accessors.f30(m34),
        Accessors.f01(m34), Accessors.f11(m34), Accessors.f21(m34), Accessors.f31(m34),
        Accessors.f02(m34), Accessors.f12(m34), Accessors.f22(m34), Accessors.f32(m34),
        Accessors.f03(m34), Accessors.f13(m34), Accessors.f23(m34), Accessors.f33(m34)
      )
      assert(a34 == Mat4x4f(m34))
      val b34 = Mat4x4d(
        Accessors.d00(m34), Accessors.d10(m34), Accessors.d20(m34), Accessors.d30(m34),
        Accessors.d01(m34), Accessors.d11(m34), Accessors.d21(m34), Accessors.d31(m34),
        Accessors.d02(m34), Accessors.d12(m34), Accessors.d22(m34), Accessors.d32(m34),
        Accessors.d03(m34), Accessors.d13(m34), Accessors.d23(m34), Accessors.d33(m34)
      )
      assert(b34 == Mat4x4d(m34))


      val m42 = Mat4x2d(md)
      val a42 = Mat4x4f(
        Accessors.f00(m42), Accessors.f10(m42), Accessors.f20(m42), Accessors.f30(m42),
        Accessors.f01(m42), Accessors.f11(m42), Accessors.f21(m42), Accessors.f31(m42),
        Accessors.f02(m42), Accessors.f12(m42), Accessors.f22(m42), Accessors.f32(m42),
        Accessors.f03(m42), Accessors.f13(m42), Accessors.f23(m42), Accessors.f33(m42)
      )
      assert(a42 == Mat4x4f(m42))
      val b42 = Mat4x4d(
        Accessors.d00(m42), Accessors.d10(m42), Accessors.d20(m42), Accessors.d30(m42),
        Accessors.d01(m42), Accessors.d11(m42), Accessors.d21(m42), Accessors.d31(m42),
        Accessors.d02(m42), Accessors.d12(m42), Accessors.d22(m42), Accessors.d32(m42),
        Accessors.d03(m42), Accessors.d13(m42), Accessors.d23(m42), Accessors.d33(m42)
      )
      assert(b42 == Mat4x4d(m42))


      val m43 = Mat4x3d(md)
      val a43 = Mat4x4f(
        Accessors.f00(m43), Accessors.f10(m43), Accessors.f20(m43), Accessors.f30(m43),
        Accessors.f01(m43), Accessors.f11(m43), Accessors.f21(m43), Accessors.f31(m43),
        Accessors.f02(m43), Accessors.f12(m43), Accessors.f22(m43), Accessors.f32(m43),
        Accessors.f03(m43), Accessors.f13(m43), Accessors.f23(m43), Accessors.f33(m43)
      )
      assert(a43 == Mat4x4f(m43))
      val b43 = Mat4x4d(
        Accessors.d00(m43), Accessors.d10(m43), Accessors.d20(m43), Accessors.d30(m43),
        Accessors.d01(m43), Accessors.d11(m43), Accessors.d21(m43), Accessors.d31(m43),
        Accessors.d02(m43), Accessors.d12(m43), Accessors.d22(m43), Accessors.d32(m43),
        Accessors.d03(m43), Accessors.d13(m43), Accessors.d23(m43), Accessors.d33(m43)
      )
      assert(b43 == Mat4x4d(m43))


      val m44 = Mat4x4d(md)
      val a44 = Mat4x4f(
        Accessors.f00(m44), Accessors.f10(m44), Accessors.f20(m44), Accessors.f30(m44),
        Accessors.f01(m44), Accessors.f11(m44), Accessors.f21(m44), Accessors.f31(m44),
        Accessors.f02(m44), Accessors.f12(m44), Accessors.f22(m44), Accessors.f32(m44),
        Accessors.f03(m44), Accessors.f13(m44), Accessors.f23(m44), Accessors.f33(m44)
      )
      assert(a44 == Mat4x4f(m44))
      val b44 = Mat4x4d(
        Accessors.d00(m44), Accessors.d10(m44), Accessors.d20(m44), Accessors.d30(m44),
        Accessors.d01(m44), Accessors.d11(m44), Accessors.d21(m44), Accessors.d31(m44),
        Accessors.d02(m44), Accessors.d12(m44), Accessors.d22(m44), Accessors.d32(m44),
        Accessors.d03(m44), Accessors.d13(m44), Accessors.d23(m44), Accessors.d33(m44)
      )
      assert(b44 == Mat4x4d(m44))
    }
  }
}
