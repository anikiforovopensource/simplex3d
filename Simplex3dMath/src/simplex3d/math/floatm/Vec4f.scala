/*
 * Simplex3d, FloatMath module
 * Copyright (C) 2009-2010 Simplex3d Team
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

package simplex3d.math.floatm

import simplex3d.math._
import simplex3d.math.BaseMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyVec4f extends Read4[Float] {

  private[math] type R2 = ConstVec2f
  private[math] type R3 = ConstVec3f
  private[math] type R4 = ConstVec4f

  protected def make2(x: Float, y: Float) =
    new ConstVec2f(x, y)
  protected def make3(x: Float, y: Float, z: Float) =
    new ConstVec3f(x, y, z)
  protected def make4(x: Float, y: Float, z: Float, w: Float) =
    new ConstVec4f(x, y, z, w)

  private[math] def bx: Boolean = bool(x)
  private[math] def by: Boolean = bool(y)
  private[math] def bz: Boolean = bool(z)
  private[math] def bw: Boolean = bool(w)

  private[math] def ix: Int = int(x)
  private[math] def iy: Int = int(y)
  private[math] def iz: Int = int(z)
  private[math] def iw: Int = int(w)

  private[math] def fx: Float = x
  private[math] def fy: Float = y
  private[math] def fz: Float = z
  private[math] def fw: Float = w

  private[math] def dx: Double = x
  private[math] def dy: Double = y
  private[math] def dz: Double = z
  private[math] def dw: Double = w


  def x: Float
  def y: Float
  def z: Float
  def w: Float
  
  def r = x
  def g = y
  def b = z
  def a = w

  def s = x
  def t = y
  def p = z
  def q = w


  def apply(i: Int) :Float = {
    i match {
      case 0 => x
      case 1 => y
      case 2 => z
      case 3 => w
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 3, got " + j
        )
    }
  }

  def unary_+() :this.type = this
  def unary_-() = new Vec4f(-x, -y, -z, -w)
  def *(s: Float) = new Vec4f(x * s, y * s, z * s, w * s)
  def /(s: Float) = { val inv = 1/s;
     new Vec4f(x * inv, y * inv, z * inv, w * inv)
  }

  def +(s: Float) = new Vec4f(x + s, y + s, z + s, w + s)
  def -(s: Float) = new Vec4f(x - s, y - s, z - s, w - s)

  private[math] def divideByComponent(s: Float) = {
    new Vec4f(s / x, s / y, s / z, s / w)
  }

  def +(u: inVec4f) = new Vec4f(x + u.x, y + u.y, z + u.z, w + u.w)
  def -(u: inVec4f) = new Vec4f(x - u.x, y - u.y, z - u.z, w - u.w)
  def *(u: inVec4f) = new Vec4f(x * u.x, y * u.y, z * u.z, w * u.w)
  def /(u: inVec4f) = new Vec4f(x / u.x, y / u.y, z / u.z, w / u.w)

  def *(m: inMat4x2f) :Vec2f = m.transposeMul(this)
  def *(m: inMat4x3f) :Vec3f = m.transposeMul(this)
  def *(m: inMat4f) :Vec4f = m.transposeMul(this)

  def ==(u: inVec4f) :Boolean = {
    if (u eq null) false
    else x == u.x && y == u.y && z == u.z && w == u.w
  }

  def !=(u: inVec4f) :Boolean = !(this == u)

  private[math] def hasErrors: Boolean = {
    import java.lang.Float._
    (
      isNaN(x) || isInfinite(x) ||
      isNaN(y) || isInfinite(y) ||
      isNaN(z) || isInfinite(z) ||
      isNaN(w) || isInfinite(w)
    )
  }

  override def equals(other: Any) :Boolean = {
    other match {
      case u: inVec4f => this == u
      case _ => false
    }
  }

  override def hashCode() :Int = {
    41 * (
      41 * (
        41 * (
          41 + x.hashCode
        ) + y.hashCode
      ) + z.hashCode
    ) + w.hashCode
  }

  override def toString() :String = {
    this.getClass.getSimpleName + "(" + x + ", " + y + ", " + z + ", " + w + ")"
  }
}


@serializable @SerialVersionUID(5359695191257934190L)
final class ConstVec4f private[math] (
  val x: Float, val y: Float, val z: Float, val w: Float
) extends AnyVec4f with Immutable

object ConstVec4f {
  /* @inline */ def apply(x: Float, y: Float, z: Float, w: Float) =
    new ConstVec4f(x, y, z, w)

  def apply(u: Read4[_]) = new ConstVec4f(u.fx, u.fy, u.fz, u.fw)

  implicit def toConst(u: AnyVec4f) = new ConstVec4f(u.x, u.y, u.z, u.w)
}


@serializable @SerialVersionUID(5359695191257934190L)
final class Vec4f private[math] (
  var x: Float, var y: Float, var z: Float, var w: Float
) extends AnyVec4f with Mutable with Implicits[On]
{
  override def r = x
  override def g = y
  override def b = z
  override def a = w

  override def s = x
  override def t = y
  override def p = z
  override def q = w

  def r_=(r: Float) { x = r }
  def g_=(g: Float) { y = g }
  def b_=(b: Float) { z = b }
  def a_=(a: Float) { w = a }

  def s_=(s: Float) { x = s }
  def t_=(t: Float) { y = t }
  def p_=(p: Float) { z = p }
  def q_=(q: Float) { w = q }


  def *=(s: Float) { x *= s; y *= s; z *= s; w *= s }
  def /=(s: Float) { val inv = 1/s; x *= inv; y *= inv; z *= inv; w *= inv }

  def +=(s: Float) { x += s; y += s; z += s; w += s }
  def -=(s: Float) { x -= s; y -= s; z -= s; w -= s }

  def +=(u: inVec4f) { x += u.x; y += u.y; z += u.z; w += u.w }
  def -=(u: inVec4f) { x -= u.x; y -= u.y; z -= u.z; w -= u.w }
  def *=(u: inVec4f) { x *= u.x; y *= u.y; z *= u.z; w *= u.w }
  def /=(u: inVec4f) { x /= u.x; y /= u.y; z /= u.z; w /= u.w }

  def *=(m: inMat4f) { this := m.transposeMul(this) }

  def :=(u: inVec4f) { x = u.x; y = u.y; z = u.z; w = u.w }
  def set(x: Float, y: Float, z: Float, w: Float) {
    this.x = x; this.y = y; this.z = z; this.w = w
  }

  def update(i: Int, s: Float) {
    i match {
      case 0 => x = s
      case 1 => y = s
      case 2 => z = s
      case 3 => w = s
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 3, got " + j
        )
    }
  }

  // Swizzling
  override def xy: ConstVec2f = new ConstVec2f(x, y)
  override def xz: ConstVec2f = new ConstVec2f(x, z)
  override def xw: ConstVec2f = new ConstVec2f(x, w)
  override def yx: ConstVec2f = new ConstVec2f(y, x)
  override def yz: ConstVec2f = new ConstVec2f(y, z)
  override def yw: ConstVec2f = new ConstVec2f(y, w)
  override def zx: ConstVec2f = new ConstVec2f(z, x)
  override def zy: ConstVec2f = new ConstVec2f(z, y)
  override def zw: ConstVec2f = new ConstVec2f(z, w)
  override def wx: ConstVec2f = new ConstVec2f(w, x)
  override def wy: ConstVec2f = new ConstVec2f(w, y)
  override def wz: ConstVec2f = new ConstVec2f(w, z)

  override def xyz: ConstVec3f = new ConstVec3f(x, y, z)
  override def xyw: ConstVec3f = new ConstVec3f(x, y, w)
  override def xzy: ConstVec3f = new ConstVec3f(x, z, y)
  override def xzw: ConstVec3f = new ConstVec3f(x, z, w)
  override def xwy: ConstVec3f = new ConstVec3f(x, w, y)
  override def xwz: ConstVec3f = new ConstVec3f(x, w, z)
  override def yxz: ConstVec3f = new ConstVec3f(y, x, z)
  override def yxw: ConstVec3f = new ConstVec3f(y, x, w)
  override def yzx: ConstVec3f = new ConstVec3f(y, z, x)
  override def yzw: ConstVec3f = new ConstVec3f(y, z, w)
  override def ywx: ConstVec3f = new ConstVec3f(y, w, x)
  override def ywz: ConstVec3f = new ConstVec3f(y, w, z)
  override def zxy: ConstVec3f = new ConstVec3f(z, x, y)
  override def zxw: ConstVec3f = new ConstVec3f(z, x, w)
  override def zyx: ConstVec3f = new ConstVec3f(z, y, x)
  override def zyw: ConstVec3f = new ConstVec3f(z, y, w)
  override def zwx: ConstVec3f = new ConstVec3f(z, w, x)
  override def zwy: ConstVec3f = new ConstVec3f(z, w, y)
  override def wxy: ConstVec3f = new ConstVec3f(w, x, y)
  override def wxz: ConstVec3f = new ConstVec3f(w, x, z)
  override def wyx: ConstVec3f = new ConstVec3f(w, y, x)
  override def wyz: ConstVec3f = new ConstVec3f(w, y, z)
  override def wzx: ConstVec3f = new ConstVec3f(w, z, x)
  override def wzy: ConstVec3f = new ConstVec3f(w, z, y)

  override def xyzw: ConstVec4f = new ConstVec4f(x, y, z, w)
  override def xywz: ConstVec4f = new ConstVec4f(x, y, w, z)
  override def xzyw: ConstVec4f = new ConstVec4f(x, z, y, w)
  override def xzwy: ConstVec4f = new ConstVec4f(x, z, w, y)
  override def xwyz: ConstVec4f = new ConstVec4f(x, w, y, z)
  override def xwzy: ConstVec4f = new ConstVec4f(x, w, z, y)
  override def yxzw: ConstVec4f = new ConstVec4f(y, x, z, w)
  override def yxwz: ConstVec4f = new ConstVec4f(y, x, w, z)
  override def yzxw: ConstVec4f = new ConstVec4f(y, z, x, w)
  override def yzwx: ConstVec4f = new ConstVec4f(y, z, w, x)
  override def ywxz: ConstVec4f = new ConstVec4f(y, w, x, z)
  override def ywzx: ConstVec4f = new ConstVec4f(y, w, z, x)
  override def zxyw: ConstVec4f = new ConstVec4f(z, x, y, w)
  override def zxwy: ConstVec4f = new ConstVec4f(z, x, w, y)
  override def zyxw: ConstVec4f = new ConstVec4f(z, y, x, w)
  override def zywx: ConstVec4f = new ConstVec4f(z, y, w, x)
  override def zwxy: ConstVec4f = new ConstVec4f(z, w, x, y)
  override def zwyx: ConstVec4f = new ConstVec4f(z, w, y, x)
  override def wxyz: ConstVec4f = new ConstVec4f(w, x, y, z)
  override def wxzy: ConstVec4f = new ConstVec4f(w, x, z, y)
  override def wyxz: ConstVec4f = new ConstVec4f(w, y, x, z)
  override def wyzx: ConstVec4f = new ConstVec4f(w, y, z, x)
  override def wzxy: ConstVec4f = new ConstVec4f(w, z, x, y)
  override def wzyx: ConstVec4f = new ConstVec4f(w, z, y, x)

  override def rg = xy
  override def rb = xz
  override def ra = xw
  override def gr = yx
  override def gb = yz
  override def ga = yw
  override def br = zx
  override def bg = zy
  override def ba = zw
  override def ar = wx
  override def ag = wy
  override def ab = wz

  override def rgb = xyz
  override def rga = xyw
  override def rbg = xzy
  override def rba = xzw
  override def rag = xwy
  override def rab = xwz
  override def grb = yxz
  override def gra = yxw
  override def gbr = yzx
  override def gba = yzw
  override def gar = ywx
  override def gab = ywz
  override def brg = zxy
  override def bra = zxw
  override def bgr = zyx
  override def bga = zyw
  override def bar = zwx
  override def bag = zwy
  override def arg = wxy
  override def arb = wxz
  override def agr = wyx
  override def agb = wyz
  override def abr = wzx
  override def abg = wzy

  override def rgba = xyzw
  override def rgab = xywz
  override def rbga = xzyw
  override def rbag = xzwy
  override def ragb = xwyz
  override def rabg = xwzy
  override def grba = yxzw
  override def grab = yxwz
  override def gbra = yzxw
  override def gbar = yzwx
  override def garb = ywxz
  override def gabr = ywzx
  override def brga = zxyw
  override def brag = zxwy
  override def bgra = zyxw
  override def bgar = zywx
  override def barg = zwxy
  override def bagr = zwyx
  override def argb = wxyz
  override def arbg = wxzy
  override def agrb = wyxz
  override def agbr = wyzx
  override def abrg = wzxy
  override def abgr = wzyx

  override def st = xy
  override def sp = xz
  override def sq = xw
  override def ts = yx
  override def tp = yz
  override def tq = yw
  override def ps = zx
  override def pt = zy
  override def pq = zw
  override def qs = wx
  override def qt = wy
  override def qp = wz

  override def stp = xyz
  override def stq = xyw
  override def spt = xzy
  override def spq = xzw
  override def sqt = xwy
  override def sqp = xwz
  override def tsp = yxz
  override def tsq = yxw
  override def tps = yzx
  override def tpq = yzw
  override def tqs = ywx
  override def tqp = ywz
  override def pst = zxy
  override def psq = zxw
  override def pts = zyx
  override def ptq = zyw
  override def pqs = zwx
  override def pqt = zwy
  override def qst = wxy
  override def qsp = wxz
  override def qts = wyx
  override def qtp = wyz
  override def qps = wzx
  override def qpt = wzy

  override def stpq = xyzw
  override def stqp = xywz
  override def sptq = xzyw
  override def spqt = xzwy
  override def sqtp = xwyz
  override def sqpt = xwzy
  override def tspq = yxzw
  override def tsqp = yxwz
  override def tpsq = yzxw
  override def tpqs = yzwx
  override def tqsp = ywxz
  override def tqps = ywzx
  override def pstq = zxyw
  override def psqt = zxwy
  override def ptsq = zyxw
  override def ptqs = zywx
  override def pqst = zwxy
  override def pqts = zwyx
  override def qstp = wxyz
  override def qspt = wxzy
  override def qtsp = wyxz
  override def qtps = wyzx
  override def qpst = wzxy
  override def qpts = wzyx

  
  def xy_=(u: inVec2f) { x = u.x; y = u.y }
  def xz_=(u: inVec2f) { x = u.x; z = u.y }
  def xw_=(u: inVec2f) { x = u.x; w = u.y }
  def yx_=(u: inVec2f) { y = u.x; x = u.y }
  def yz_=(u: inVec2f) { y = u.x; z = u.y }
  def yw_=(u: inVec2f) { y = u.x; w = u.y }
  def zx_=(u: inVec2f) { z = u.x; x = u.y }
  def zy_=(u: inVec2f) { z = u.x; y = u.y }
  def zw_=(u: inVec2f) { z = u.x; w = u.y }
  def wx_=(u: inVec2f) { w = u.x; x = u.y }
  def wy_=(u: inVec2f) { w = u.x; y = u.y }
  def wz_=(u: inVec2f) { w = u.x; z = u.y }

  def xyz_=(u: inVec3f) { x = u.x; y = u.y; z = u.z }
  def xyw_=(u: inVec3f) { x = u.x; y = u.y; w = u.z }
  def xzy_=(u: inVec3f) { x = u.x; z = u.y; y = u.z }
  def xzw_=(u: inVec3f) { x = u.x; z = u.y; w = u.z }
  def xwy_=(u: inVec3f) { x = u.x; w = u.y; y = u.z }
  def xwz_=(u: inVec3f) { x = u.x; w = u.y; z = u.z }
  def yxz_=(u: inVec3f) { y = u.x; x = u.y; z = u.z }
  def yxw_=(u: inVec3f) { y = u.x; x = u.y; w = u.z }
  def yzx_=(u: inVec3f) { y = u.x; z = u.y; x = u.z }
  def yzw_=(u: inVec3f) { y = u.x; z = u.y; w = u.z }
  def ywx_=(u: inVec3f) { y = u.x; w = u.y; x = u.z }
  def ywz_=(u: inVec3f) { y = u.x; w = u.y; z = u.z }
  def zxy_=(u: inVec3f) { z = u.x; x = u.y; y = u.z }
  def zxw_=(u: inVec3f) { z = u.x; x = u.y; w = u.z }
  def zyx_=(u: inVec3f) { z = u.x; y = u.y; x = u.z }
  def zyw_=(u: inVec3f) { z = u.x; y = u.y; w = u.z }
  def zwx_=(u: inVec3f) { z = u.x; w = u.y; x = u.z }
  def zwy_=(u: inVec3f) { z = u.x; w = u.y; y = u.z }
  def wxy_=(u: inVec3f) { w = u.x; x = u.y; y = u.z }
  def wxz_=(u: inVec3f) { w = u.x; x = u.y; z = u.z }
  def wyx_=(u: inVec3f) { w = u.x; y = u.y; x = u.z }
  def wyz_=(u: inVec3f) { w = u.x; y = u.y; z = u.z }
  def wzx_=(u: inVec3f) { w = u.x; z = u.y; x = u.z }
  def wzy_=(u: inVec3f) { w = u.x; z = u.y; y = u.z }

  def xyzw_=(u: inVec4f) { x = u.x; y = u.y; z = u.z; w = u.w }
  def xywz_=(u: inVec4f) { x = u.x; y = u.y; var t = u.w; w = u.z; z = t }
  def xzyw_=(u: inVec4f) { x = u.x; var t = u.z; z = u.y; y = t; w = u.w }
  def xzwy_=(u: inVec4f) { x = u.x; var t = u.z; z = u.y; y = u.w; w = t }
  def xwyz_=(u: inVec4f) { x = u.x; var t = u.w; w = u.y; y = u.z; z = t }
  def xwzy_=(u: inVec4f) { x = u.x; var t = u.w; w = u.y; y = t; z = u.z }
  def yxzw_=(u: inVec4f) { var t = u.y; y = u.x; x = t; z = u.z; w = u.w }
  def yxwz_=(u: inVec4f) { var t = u.y; y = u.x; x = t; t = u.w; w = u.z; z=t }
  def yzxw_=(u: inVec4f) { var t = u.y; y = u.x; x = u.z; z = t; w = u.w }
  def yzwx_=(u: inVec4f) { var t = u.y; y = u.x; x = u.w; w = u.z; z = t }
  def ywxz_=(u: inVec4f) { var t = u.y; y = u.x; x = u.z; z = u.w; w = t }
  def ywzx_=(u: inVec4f) { var t = u.y; y = u.x; x = u.w; w = t; z = u.z }
  def zxyw_=(u: inVec4f) { var t = u.z; z = u.x; x = u.y; y = t; w = u.w }
  def zxwy_=(u: inVec4f) { var t = u.z; z = u.x; x = u.y; y = u.w; w = t }
  def zyxw_=(u: inVec4f) { var t = u.z; z = u.x; x = t; y = u.y; w = u.w }
  def zywx_=(u: inVec4f) { var t = u.z; z = u.x; x = u.w; w = t; y = u.y }
  def zwxy_=(u: inVec4f) { var t = u.z; z = u.x; x = t; t = u.w; w = u.y; y=t }
  def zwyx_=(u: inVec4f) { var t = u.z; z = u.x; x = u.w; w = u.y; y = t }
  def wxyz_=(u: inVec4f) { var t = u.w; w = u.x; x = u.y; y = u.z; z = t }
  def wxzy_=(u: inVec4f) { var t = u.w; w = u.x; x = u.y; y = t; z = u.z }
  def wyxz_=(u: inVec4f) { var t = u.w; w = u.x; x = u.z; z = t; y = u.y }
  def wyzx_=(u: inVec4f) { var t = u.w; w = u.x; x = t; y = u.y; z = u.z }
  def wzxy_=(u: inVec4f) { var t = u.w; w = u.x; x = u.z; z = u.y; y = t }
  def wzyx_=(u: inVec4f) { var t = u.w; w = u.x; x = t; t = u.z; z = u.y; y=t }

  def rg_=(u: inVec2f) { xy_=(u) }
  def rb_=(u: inVec2f) { xz_=(u) }
  def ra_=(u: inVec2f) { xw_=(u) }
  def gr_=(u: inVec2f) { yx_=(u) }
  def gb_=(u: inVec2f) { yz_=(u) }
  def ga_=(u: inVec2f) { yw_=(u) }
  def br_=(u: inVec2f) { zx_=(u) }
  def bg_=(u: inVec2f) { zy_=(u) }
  def ba_=(u: inVec2f) { zw_=(u) }
  def ar_=(u: inVec2f) { wx_=(u) }
  def ag_=(u: inVec2f) { wy_=(u) }
  def ab_=(u: inVec2f) { wz_=(u) }

  def rgb_=(u: inVec3f) { xyz_=(u) }
  def rga_=(u: inVec3f) { xyw_=(u) }
  def rbg_=(u: inVec3f) { xzy_=(u) }
  def rba_=(u: inVec3f) { xzw_=(u) }
  def rag_=(u: inVec3f) { xwy_=(u) }
  def rab_=(u: inVec3f) { xwz_=(u) }
  def grb_=(u: inVec3f) { yxz_=(u) }
  def gra_=(u: inVec3f) { yxw_=(u) }
  def gbr_=(u: inVec3f) { yzx_=(u) }
  def gba_=(u: inVec3f) { yzw_=(u) }
  def gar_=(u: inVec3f) { ywx_=(u) }
  def gab_=(u: inVec3f) { ywz_=(u) }
  def brg_=(u: inVec3f) { zxy_=(u) }
  def bra_=(u: inVec3f) { zxw_=(u) }
  def bgr_=(u: inVec3f) { zyx_=(u) }
  def bga_=(u: inVec3f) { zyw_=(u) }
  def bar_=(u: inVec3f) { zwx_=(u) }
  def bag_=(u: inVec3f) { zwy_=(u) }
  def arg_=(u: inVec3f) { wxy_=(u) }
  def arb_=(u: inVec3f) { wxz_=(u) }
  def agr_=(u: inVec3f) { wyx_=(u) }
  def agb_=(u: inVec3f) { wyz_=(u) }
  def abr_=(u: inVec3f) { wzx_=(u) }
  def abg_=(u: inVec3f) { wzy_=(u) }

  def rgba_=(u: inVec4f) { xyzw_=(u) }
  def rgab_=(u: inVec4f) { xywz_=(u) }
  def rbga_=(u: inVec4f) { xzyw_=(u) }
  def rbag_=(u: inVec4f) { xzwy_=(u) }
  def ragb_=(u: inVec4f) { xwyz_=(u) }
  def rabg_=(u: inVec4f) { xwzy_=(u) }
  def grba_=(u: inVec4f) { yxzw_=(u) }
  def grab_=(u: inVec4f) { yxwz_=(u) }
  def gbra_=(u: inVec4f) { yzxw_=(u) }
  def gbar_=(u: inVec4f) { yzwx_=(u) }
  def garb_=(u: inVec4f) { ywxz_=(u) }
  def gabr_=(u: inVec4f) { ywzx_=(u) }
  def brga_=(u: inVec4f) { zxyw_=(u) }
  def brag_=(u: inVec4f) { zxwy_=(u) }
  def bgra_=(u: inVec4f) { zyxw_=(u) }
  def bgar_=(u: inVec4f) { zywx_=(u) }
  def barg_=(u: inVec4f) { zwxy_=(u) }
  def bagr_=(u: inVec4f) { zwyx_=(u) }
  def argb_=(u: inVec4f) { wxyz_=(u) }
  def arbg_=(u: inVec4f) { wxzy_=(u) }
  def agrb_=(u: inVec4f) { wyxz_=(u) }
  def agbr_=(u: inVec4f) { wyzx_=(u) }
  def abrg_=(u: inVec4f) { wzxy_=(u) }
  def abgr_=(u: inVec4f) { wzyx_=(u) }

  def st_=(u: inVec2f) { xy_=(u) }
  def sp_=(u: inVec2f) { xz_=(u) }
  def sq_=(u: inVec2f) { xw_=(u) }
  def ts_=(u: inVec2f) { yx_=(u) }
  def tp_=(u: inVec2f) { yz_=(u) }
  def tq_=(u: inVec2f) { yw_=(u) }
  def ps_=(u: inVec2f) { zx_=(u) }
  def pt_=(u: inVec2f) { zy_=(u) }
  def pq_=(u: inVec2f) { zw_=(u) }
  def qs_=(u: inVec2f) { wx_=(u) }
  def qt_=(u: inVec2f) { wy_=(u) }
  def qp_=(u: inVec2f) { wz_=(u) }

  def stp_=(u: inVec3f) { xyz_=(u) }
  def stq_=(u: inVec3f) { xyw_=(u) }
  def spt_=(u: inVec3f) { xzy_=(u) }
  def spq_=(u: inVec3f) { xzw_=(u) }
  def sqt_=(u: inVec3f) { xwy_=(u) }
  def sqp_=(u: inVec3f) { xwz_=(u) }
  def tsp_=(u: inVec3f) { yxz_=(u) }
  def tsq_=(u: inVec3f) { yxw_=(u) }
  def tps_=(u: inVec3f) { yzx_=(u) }
  def tpq_=(u: inVec3f) { yzw_=(u) }
  def tqs_=(u: inVec3f) { ywx_=(u) }
  def tqp_=(u: inVec3f) { ywz_=(u) }
  def pst_=(u: inVec3f) { zxy_=(u) }
  def psq_=(u: inVec3f) { zxw_=(u) }
  def pts_=(u: inVec3f) { zyx_=(u) }
  def ptq_=(u: inVec3f) { zyw_=(u) }
  def pqs_=(u: inVec3f) { zwx_=(u) }
  def pqt_=(u: inVec3f) { zwy_=(u) }
  def qst_=(u: inVec3f) { wxy_=(u) }
  def qsp_=(u: inVec3f) { wxz_=(u) }
  def qts_=(u: inVec3f) { wyx_=(u) }
  def qtp_=(u: inVec3f) { wyz_=(u) }
  def qps_=(u: inVec3f) { wzx_=(u) }
  def qpt_=(u: inVec3f) { wzy_=(u) }

  def stpq_=(u: inVec4f) { xyzw_=(u) }
  def stqp_=(u: inVec4f) { xywz_=(u) }
  def sptq_=(u: inVec4f) { xzyw_=(u) }
  def spqt_=(u: inVec4f) { xzwy_=(u) }
  def sqtp_=(u: inVec4f) { xwyz_=(u) }
  def sqpt_=(u: inVec4f) { xwzy_=(u) }
  def tspq_=(u: inVec4f) { yxzw_=(u) }
  def tsqp_=(u: inVec4f) { yxwz_=(u) }
  def tpsq_=(u: inVec4f) { yzxw_=(u) }
  def tpqs_=(u: inVec4f) { yzwx_=(u) }
  def tqsp_=(u: inVec4f) { ywxz_=(u) }
  def tqps_=(u: inVec4f) { ywzx_=(u) }
  def pstq_=(u: inVec4f) { zxyw_=(u) }
  def psqt_=(u: inVec4f) { zxwy_=(u) }
  def ptsq_=(u: inVec4f) { zyxw_=(u) }
  def ptqs_=(u: inVec4f) { zywx_=(u) }
  def pqst_=(u: inVec4f) { zwxy_=(u) }
  def pqts_=(u: inVec4f) { zwyx_=(u) }
  def qstp_=(u: inVec4f) { wxyz_=(u) }
  def qspt_=(u: inVec4f) { wxzy_=(u) }
  def qtsp_=(u: inVec4f) { wyxz_=(u) }
  def qtps_=(u: inVec4f) { wyzx_=(u) }
  def qpst_=(u: inVec4f) { wzxy_=(u) }
  def qpts_=(u: inVec4f) { wzyx_=(u) }
}

object Vec4f {
  val Zero = new ConstVec4f(0, 0, 0, 0)
  val UnitX = new ConstVec4f(1, 0, 0, 0)
  val UnitY = new ConstVec4f(0, 1, 0, 0)
  val UnitZ = new ConstVec4f(0, 0, 1, 0)
  val UnitW = new ConstVec4f(0, 0, 0, 1)
  val One = new ConstVec4f(1, 1, 1, 1)

  def apply(s: Float) =
    new Vec4f(s, s, s, s)

  /* @inline */ def apply(x: Float, y: Float, z: Float, w: Float) =
    new Vec4f(x, y, z, w)

  def apply(u: Read4[_]) =
    new Vec4f(u.fx, u.fy, u.fz, u.fw)

  def apply(xy: Read2[_], z: Float, w: Float) =
    new Vec4f(xy.fx, xy.fy, z, w)

  def apply(x: Float, yz: Read2[_], w: Float) =
    new Vec4f(x, yz.fx, yz.fy, w)

  def apply(x: Float, y: Float, zw: Read2[_]) =
    new Vec4f(x, y, zw.fx, zw.fy)

  def apply(xy: Read2[_], zw: Read2[_]) =
    new Vec4f(xy.fx, xy.fy, zw.fx, zw.fy)

  def apply(xyz: Read3[_], w: Float) =
    new Vec4f(xyz.fx, xyz.fy, xyz.fz, w)

  def apply(x: Float, yzw: Read3[_]) =
    new Vec4f(x, yzw.fx, yzw.fy, yzw.fz)

  def apply(m: Read2x2[_]) =
    new Vec4f(m.f00, m.f10, m.f01, m.f11)

  def unapply(u: AnyVec4f) = Some((u.x, u.y, u.z, u.w))

  implicit def toMutable(u: AnyVec4f) = new Vec4f(u.x, u.y, u.z, u.w)
  implicit def castInt(u: Read4[Int]) = new Vec4f(u.fx, u.fy, u.fz, u.fw)
}
