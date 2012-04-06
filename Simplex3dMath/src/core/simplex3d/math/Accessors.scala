/*
 * Simplex3dMath - Core Module
 * Copyright (C) 2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dMath.
 *
 * Simplex3dMath is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dMath is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.math

import simplex3d.math.types._


// An empty class to make -Xno-forwarders work
private[math] class Accessors


/** Allows access to components of undefined type.
 *
 * @author Aleksey Nikiforov (lex)
 */
object Accessors {

  /** Read the component as Boolean. */ def bx(u: AnyVec[_]) = u.bx
  /** Read the component as Boolean. */ def by(u: AnyVec2[_]) = u.by
  /** Read the component as Boolean. */ def by(u: AnyVec3[_]) = u.by
  /** Read the component as Boolean. */ def by(u: AnyVec4[_]) = u.by
  /** Read the component as Boolean. */ def bz(u: AnyVec3[_]) = u.bz
  /** Read the component as Boolean. */ def bz(u: AnyVec4[_]) = u.bz
  /** Read the component as Boolean. */ def bw(u: AnyVec4[_]) = u.bw

  /** Read the component as Int. */ def ix(u: AnyVec[_]) = u.ix
  /** Read the component as Int. */ def iy(u: AnyVec2[_]) = u.iy
  /** Read the component as Int. */ def iy(u: AnyVec3[_]) = u.iy
  /** Read the component as Int. */ def iy(u: AnyVec4[_]) = u.iy
  /** Read the component as Int. */ def iz(u: AnyVec3[_]) = u.iz
  /** Read the component as Int. */ def iz(u: AnyVec4[_]) = u.iz
  /** Read the component as Int. */ def iw(u: AnyVec4[_]) = u.iw

  /** Read the component as Float. */ def fx(u: AnyVec[_]) = u.fx
  /** Read the component as Float. */ def fy(u: AnyVec2[_]) = u.fy
  /** Read the component as Float. */ def fy(u: AnyVec3[_]) = u.fy
  /** Read the component as Float. */ def fy(u: AnyVec4[_]) = u.fy
  /** Read the component as Float. */ def fz(u: AnyVec3[_]) = u.fz
  /** Read the component as Float. */ def fz(u: AnyVec4[_]) = u.fz
  /** Read the component as Float. */ def fw(u: AnyVec4[_]) = u.fw

  /** Read the component as Double. */ def dx(u: AnyVec[_]) = u.dx
  /** Read the component as Double. */ def dy(u: AnyVec2[_]) = u.dy
  /** Read the component as Double. */ def dy(u: AnyVec3[_]) = u.dy
  /** Read the component as Double. */ def dy(u: AnyVec4[_]) = u.dy
  /** Read the component as Double. */ def dz(u: AnyVec3[_]) = u.dz
  /** Read the component as Double. */ def dz(u: AnyVec4[_]) = u.dz
  /** Read the component as Double. */ def dw(u: AnyVec4[_]) = u.dw


  /** Read the component as Float. */ def fa(q: AnyQuat4[_]) = q.fa
  /** Read the component as Float. */ def fb(q: AnyQuat4[_]) = q.fb
  /** Read the component as Float. */ def fc(q: AnyQuat4[_]) = q.fc
  /** Read the component as Float. */ def fd(q: AnyQuat4[_]) = q.fd

  /** Read the component as Double. */ def da(q: AnyQuat4[_]) = q.da
  /** Read the component as Double. */ def db(q: AnyQuat4[_]) = q.db
  /** Read the component as Double. */ def dc(q: AnyQuat4[_]) = q.dc
  /** Read the component as Double. */ def dd(q: AnyQuat4[_]) = q.dd


  /** Read the component as Float. */ def f00(m: AnyMat[_]) = m.f00
  /** Read the component as Float. */ def f01(m: AnyMat[_]) = m.f01
  /** Read the component as Float. */ def f02(m: AnyMat[_]) = m.f02
  /** Read the component as Float. */ def f03(m: AnyMat[_]) = m.f03

  /** Read the component as Float. */ def f10(m: AnyMat[_]) = m.f10
  /** Read the component as Float. */ def f11(m: AnyMat[_]) = m.f11
  /** Read the component as Float. */ def f12(m: AnyMat[_]) = m.f12
  /** Read the component as Float. */ def f13(m: AnyMat[_]) = m.f13

  /** Read the component as Float. */ def f20(m: AnyMat[_]) = m.f20
  /** Read the component as Float. */ def f21(m: AnyMat[_]) = m.f21
  /** Read the component as Float. */ def f22(m: AnyMat[_]) = m.f22
  /** Read the component as Float. */ def f23(m: AnyMat[_]) = m.f23

  /** Read the component as Float. */ def f30(m: AnyMat[_]) = m.f30
  /** Read the component as Float. */ def f31(m: AnyMat[_]) = m.f31
  /** Read the component as Float. */ def f32(m: AnyMat[_]) = m.f32
  /** Read the component as Float. */ def f33(m: AnyMat[_]) = m.f33


  /** Read the component as Double. */ def d00(m: AnyMat[_]) = m.d00
  /** Read the component as Double. */ def d01(m: AnyMat[_]) = m.d01
  /** Read the component as Double. */ def d02(m: AnyMat[_]) = m.d02
  /** Read the component as Double. */ def d03(m: AnyMat[_]) = m.d03

  /** Read the component as Double. */ def d10(m: AnyMat[_]) = m.d10
  /** Read the component as Double. */ def d11(m: AnyMat[_]) = m.d11
  /** Read the component as Double. */ def d12(m: AnyMat[_]) = m.d12
  /** Read the component as Double. */ def d13(m: AnyMat[_]) = m.d13

  /** Read the component as Double. */ def d20(m: AnyMat[_]) = m.d20
  /** Read the component as Double. */ def d21(m: AnyMat[_]) = m.d21
  /** Read the component as Double. */ def d22(m: AnyMat[_]) = m.d22
  /** Read the component as Double. */ def d23(m: AnyMat[_]) = m.d23

  /** Read the component as Double. */ def d30(m: AnyMat[_]) = m.d30
  /** Read the component as Double. */ def d31(m: AnyMat[_]) = m.d31
  /** Read the component as Double. */ def d32(m: AnyMat[_]) = m.d32
  /** Read the component as Double. */ def d33(m: AnyMat[_]) = m.d33
}
