/*
 * Simplex3d, FloatMath module
 * Copyright (C) 2009-2011, Aleksey Nikiforov
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
package floatx

import scala.reflect.ClassManifest.{classType}
import simplex3d.math.integration._


/**
 * @author Aleksey Nikiforov (lex)
 */
@SerialVersionUID(8104346712419693669L)
sealed abstract class ReadVec4f extends ProtectedVec4f[Float]
with ReadPropertyRef[ReadVec4f] with Serializable
{

  type Clone <: ReadVec4f
  type Const = ConstVec4f
  def toConst() = ConstVec4f(this)

  private[math] type R2 = ReadVec2f
  private[math] type R3 = ReadVec3f
  private[math] type R4 = ReadVec4f

  private[math] type C2 = ConstVec2f
  private[math] type C3 = ConstVec3f
  private[math] type C4 = ConstVec4f

  protected final def make2(x: Double, y: Double) =
    new ConstVec2f(x.toFloat, y.toFloat)
  protected final def make3(x: Double, y: Double, z: Double) =
    new ConstVec3f(x.toFloat, y.toFloat, z.toFloat)
  protected final def make4(x: Double, y: Double, z: Double, w: Double) =
    new ConstVec4f(x.toFloat, y.toFloat, z.toFloat, w.toFloat)

  private[math] final def bx: Boolean = simplex3d.math.Boolean(x)
  private[math] final def by: Boolean = simplex3d.math.Boolean(y)
  private[math] final def bz: Boolean = simplex3d.math.Boolean(z)
  private[math] final def bw: Boolean = simplex3d.math.Boolean(w)

  private[math] final def ix: Int = x.toInt
  private[math] final def iy: Int = y.toInt
  private[math] final def iz: Int = z.toInt
  private[math] final def iw: Int = w.toInt

  private[math] final def fx: Float = x
  private[math] final def fy: Float = y
  private[math] final def fz: Float = z
  private[math] final def fw: Float = w

  private[math] final def dx: Double = x
  private[math] final def dy: Double = y
  private[math] final def dz: Double = z
  private[math] final def dw: Double = w


  final def x = px
  final def y = py
  final def z = pz
  final def w = pw

  final def r = px
  final def g = py
  final def b = pz
  final def a = pw

  final def s = px
  final def t = py
  final def p = pz
  final def q = pw


  protected def x_=(s: Float) { throw new UnsupportedOperationException }
  protected def y_=(s: Float) { throw new UnsupportedOperationException }
  protected def z_=(s: Float) { throw new UnsupportedOperationException }
  protected def w_=(s: Float) { throw new UnsupportedOperationException }

  protected def r_=(s: Float) { throw new UnsupportedOperationException }
  protected def g_=(s: Float) { throw new UnsupportedOperationException }
  protected def b_=(s: Float) { throw new UnsupportedOperationException }
  protected def a_=(s: Float) { throw new UnsupportedOperationException }

  protected def s_=(s: Float) { throw new UnsupportedOperationException }
  protected def t_=(s: Float) { throw new UnsupportedOperationException }
  protected def p_=(s: Float) { throw new UnsupportedOperationException }
  protected def q_=(s: Float) { throw new UnsupportedOperationException }

  final def apply(i: Int) :Float = {
    i match {
      case 0 => x
      case 1 => y
      case 2 => z
      case 3 => w
      case j => throw new IndexOutOfBoundsException(
          "Expected from 0 to 3, got " + j + "."
        )
    }
  }

  final def unary_+() :ReadVec4f = this
  final def unary_-() = new Vec4f(-x, -y, -z, -w)

  final def *(s: Float) = new Vec4f(x * s, y * s, z * s, w * s)
  final def /(s: Float) = new Vec4f(x / s, y / s, z / s, w / s)
  private[math] final def divByComp(s: Float) = new Vec4f(s / x, s / y, s / z, s / w)
  final def +(s: Float) = new Vec4f(x + s, y + s, z + s, w + s)
  final def -(s: Float) = new Vec4f(x - s, y - s, z - s, w - s)

  final def *(u: inVec4f) = new Vec4f(x * u.x, y * u.y, z * u.z, w * u.w)
  final def /(u: inVec4f) = new Vec4f(x / u.x, y / u.y, z / u.z, w / u.w)
  final def +(u: inVec4f) = new Vec4f(x + u.x, y + u.y, z + u.z, w + u.w)
  final def -(u: inVec4f) = new Vec4f(x - u.x, y - u.y, z - u.z, w - u.w)

  final def *(m: inMat4x2f) :Vec2f = m.transposeMult(this)
  final def *(m: inMat4x3f) :Vec3f = m.transposeMult(this)
  final def *(m: inMat4f) :Vec4f = m.transposeMult(this)


  final override def equals(other: Any) :Boolean = {
    other match {
      case u: ReadVec4b => false
      case u: AnyVec4[_] => dx == u.dx && dy == u.dy && dz == u.dz && dw == u.dw
      case _ => false
    }
  }

  final override def hashCode() :Int = {
    41 * (
      41 * (
        41 * (
          41 + w.hashCode
        ) + z.hashCode
      ) + y.hashCode
    ) + x.hashCode
  }

  final override def toString() :String = {
    val prefix = this match {
      case self: Immutable => "Const"
      case _ => ""
    }
    prefix + "Vec4" + "(" + x + "f, " + y + "f, " + z + "f, " + w + "f)"
  }
}


@SerialVersionUID(8104346712419693669L)
final class ConstVec4f private[math] (cx: Float, cy: Float, cz: Float, cw: Float)
extends ReadVec4f with Immutable with Serializable {
  px = cx; py = cy; pz = cz; pw = cw

  type Clone = ConstVec4f
  override def clone() = this
}


object ConstVec4f {

  def apply(s: Float) = new ConstVec4f(s, s, s, s)
  def apply(x: Float, y: Float, z: Float, w: Float) = new ConstVec4f(x, y, z, w)

  def apply(u: AnyVec4[_]) = new ConstVec4f(u.fx, u.fy, u.fz, u.fw)
  def apply(xy: AnyVec2[_], z: Float, w: Float) = new ConstVec4f(xy.fx, xy.fy, z, w)
  def apply(x: Float, yz: AnyVec2[_], w: Float) = new ConstVec4f(x, yz.fx, yz.fy, w)
  def apply(x: Float, y: Float, zw: AnyVec2[_]) = new ConstVec4f(x, y, zw.fx, zw.fy)
  def apply(xy: AnyVec2[_], zw: AnyVec2[_]) = new ConstVec4f(xy.fx, xy.fy, zw.fx, zw.fy)
  def apply(xyz: AnyVec3[_], w: Float) = new ConstVec4f(xyz.fx, xyz.fy, xyz.fz, w)
  def apply(x: Float, yzw: AnyVec3[_]) = new ConstVec4f(x, yzw.fx, yzw.fy, yzw.fz)

  def apply(m: AnyMat2[_]) = new ConstVec4f(m.f00, m.f10, m.f01, m.f11)
  def apply(q: AnyQuat4[_]) = new ConstVec4f(q.fb, q.fc, q.fd, q.fa)

  implicit def toConst(u: ReadVec4f) = apply(u)
}


@SerialVersionUID(8104346712419693669L)
final class Vec4f private[math] (cx: Float, cy: Float, cz: Float, cw: Float)
extends ReadVec4f with CompositeFormat with Implicits[On]
with PropertyRef[ReadVec4f] with Serializable
{
  px = cx; py = cy; pz = cz; pw = cw

  type Read = ReadVec4f
  type Component = RFloat
  type Clone = Vec4f
  override def clone() = Vec4f(this)
  def :=(u: ConstVec4f) { this := u.asInstanceOf[inVec4f] }
  def :=(u: inVec4f) { x = u.x; y = u.y; z = u.z; w = u.w }


  @noinline override def x_=(s: Float) { px = s }
  @noinline override def y_=(s: Float) { py = s }
  @noinline override def z_=(s: Float) { pz = s }
  @noinline override def w_=(s: Float) { pw = s }

  override def r_=(s: Float) { px = s }
  override def g_=(s: Float) { py = s }
  override def b_=(s: Float) { pz = s }
  override def a_=(s: Float) { pw = s }

  override def s_=(s: Float) { px = s }
  override def t_=(s: Float) { py = s }
  override def p_=(s: Float) { pz = s }
  override def q_=(s: Float) { pw = s }

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

  def *=(s: Float) { x *= s; y *= s; z *= s; w *= s }
  def /=(s: Float) { x /= s; y /= s; z /= s; w /= s }
  def +=(s: Float) { x += s; y += s; z += s; w += s }
  def -=(s: Float) { x -= s; y -= s; z -= s; w -= s }

  def *=(u: inVec4f) { x *= u.x; y *= u.y; z *= u.z; w *= u.w }
  def /=(u: inVec4f) { x /= u.x; y /= u.y; z /= u.z; w /= u.w }
  def +=(u: inVec4f) { x += u.x; y += u.y; z += u.z; w += u.w }
  def -=(u: inVec4f) { x -= u.x; y -= u.y; z -= u.z; w -= u.w }

  def *=(m: inMat4f) { this := m.transposeMult(this) }

  // Swizzling
  override def xy_=(u: inVec2f) { x = u.x; y = u.y }
  override def xz_=(u: inVec2f) { x = u.x; z = u.y }
  override def xw_=(u: inVec2f) { x = u.x; w = u.y }
  override def yx_=(u: inVec2f) { y = u.x; x = u.y }
  override def yz_=(u: inVec2f) { y = u.x; z = u.y }
  override def yw_=(u: inVec2f) { y = u.x; w = u.y }
  override def zx_=(u: inVec2f) { z = u.x; x = u.y }
  override def zy_=(u: inVec2f) { z = u.x; y = u.y }
  override def zw_=(u: inVec2f) { z = u.x; w = u.y }
  override def wx_=(u: inVec2f) { w = u.x; x = u.y }
  override def wy_=(u: inVec2f) { w = u.x; y = u.y }
  override def wz_=(u: inVec2f) { w = u.x; z = u.y }

  override def xyz_=(u: inVec3f) { x = u.x; y = u.y; z = u.z }
  override def xyw_=(u: inVec3f) { x = u.x; y = u.y; w = u.z }
  override def xzy_=(u: inVec3f) { x = u.x; z = u.y; y = u.z }
  override def xzw_=(u: inVec3f) { x = u.x; z = u.y; w = u.z }
  override def xwy_=(u: inVec3f) { x = u.x; w = u.y; y = u.z }
  override def xwz_=(u: inVec3f) { x = u.x; w = u.y; z = u.z }
  override def yxz_=(u: inVec3f) { y = u.x; x = u.y; z = u.z }
  override def yxw_=(u: inVec3f) { y = u.x; x = u.y; w = u.z }
  override def yzx_=(u: inVec3f) { y = u.x; z = u.y; x = u.z }
  override def yzw_=(u: inVec3f) { y = u.x; z = u.y; w = u.z }
  override def ywx_=(u: inVec3f) { y = u.x; w = u.y; x = u.z }
  override def ywz_=(u: inVec3f) { y = u.x; w = u.y; z = u.z }
  override def zxy_=(u: inVec3f) { z = u.x; x = u.y; y = u.z }
  override def zxw_=(u: inVec3f) { z = u.x; x = u.y; w = u.z }
  override def zyx_=(u: inVec3f) { z = u.x; y = u.y; x = u.z }
  override def zyw_=(u: inVec3f) { z = u.x; y = u.y; w = u.z }
  override def zwx_=(u: inVec3f) { z = u.x; w = u.y; x = u.z }
  override def zwy_=(u: inVec3f) { z = u.x; w = u.y; y = u.z }
  override def wxy_=(u: inVec3f) { w = u.x; x = u.y; y = u.z }
  override def wxz_=(u: inVec3f) { w = u.x; x = u.y; z = u.z }
  override def wyx_=(u: inVec3f) { w = u.x; y = u.y; x = u.z }
  override def wyz_=(u: inVec3f) { w = u.x; y = u.y; z = u.z }
  override def wzx_=(u: inVec3f) { w = u.x; z = u.y; x = u.z }
  override def wzy_=(u: inVec3f) { w = u.x; z = u.y; y = u.z }

  override def xyzw_=(u: inVec4f) { x = u.x; y = u.y; z = u.z; w = u.w }
  override def xywz_=(u: inVec4f) { x = u.x; y = u.y; var t = u.w; w = u.z; z = t }
  override def xzyw_=(u: inVec4f) { x = u.x; var t = u.z; z = u.y; y = t; w = u.w }
  override def xzwy_=(u: inVec4f) { x = u.x; var t = u.z; z = u.y; y = u.w; w = t }
  override def xwyz_=(u: inVec4f) { x = u.x; var t = u.w; w = u.y; y = u.z; z = t }
  override def xwzy_=(u: inVec4f) { x = u.x; var t = u.w; w = u.y; y = t; z = u.z }
  override def yxzw_=(u: inVec4f) { var t = u.y; y = u.x; x = t; z = u.z; w = u.w }
  override def yxwz_=(u: inVec4f) { var t = u.y; y = u.x; x = t; t = u.w; w = u.z; z = t }
  override def yzxw_=(u: inVec4f) { var t = u.y; y = u.x; x = u.z; z = t; w = u.w }
  override def yzwx_=(u: inVec4f) { var t = u.y; y = u.x; x = u.w; w = u.z; z = t }
  override def ywxz_=(u: inVec4f) { var t = u.y; y = u.x; x = u.z; z = u.w; w = t }
  override def ywzx_=(u: inVec4f) { var t = u.y; y = u.x; x = u.w; w = t; z = u.z }
  override def zxyw_=(u: inVec4f) { var t = u.z; z = u.x; x = u.y; y = t; w = u.w }
  override def zxwy_=(u: inVec4f) { var t = u.z; z = u.x; x = u.y; y = u.w; w = t }
  override def zyxw_=(u: inVec4f) { var t = u.z; z = u.x; x = t; y = u.y; w = u.w }
  override def zywx_=(u: inVec4f) { var t = u.z; z = u.x; x = u.w; w = t; y = u.y }
  override def zwxy_=(u: inVec4f) { var t = u.z; z = u.x; x = t; t = u.w; w = u.y; y = t }
  override def zwyx_=(u: inVec4f) { var t = u.z; z = u.x; x = u.w; w = u.y; y = t }
  override def wxyz_=(u: inVec4f) { var t = u.w; w = u.x; x = u.y; y = u.z; z = t }
  override def wxzy_=(u: inVec4f) { var t = u.w; w = u.x; x = u.y; y = t; z = u.z }
  override def wyxz_=(u: inVec4f) { var t = u.w; w = u.x; x = u.z; z = t; y = u.y }
  override def wyzx_=(u: inVec4f) { var t = u.w; w = u.x; x = t; y = u.y; z = u.z }
  override def wzxy_=(u: inVec4f) { var t = u.w; w = u.x; x = u.z; z = u.y; y = t }
  override def wzyx_=(u: inVec4f) { var t = u.w; w = u.x; x = t; t = u.z; z = u.y; y = t }

  override def rg_=(u: inVec2f) { xy_=(u) }
  override def rb_=(u: inVec2f) { xz_=(u) }
  override def ra_=(u: inVec2f) { xw_=(u) }
  override def gr_=(u: inVec2f) { yx_=(u) }
  override def gb_=(u: inVec2f) { yz_=(u) }
  override def ga_=(u: inVec2f) { yw_=(u) }
  override def br_=(u: inVec2f) { zx_=(u) }
  override def bg_=(u: inVec2f) { zy_=(u) }
  override def ba_=(u: inVec2f) { zw_=(u) }
  override def ar_=(u: inVec2f) { wx_=(u) }
  override def ag_=(u: inVec2f) { wy_=(u) }
  override def ab_=(u: inVec2f) { wz_=(u) }

  override def rgb_=(u: inVec3f) { xyz_=(u) }
  override def rga_=(u: inVec3f) { xyw_=(u) }
  override def rbg_=(u: inVec3f) { xzy_=(u) }
  override def rba_=(u: inVec3f) { xzw_=(u) }
  override def rag_=(u: inVec3f) { xwy_=(u) }
  override def rab_=(u: inVec3f) { xwz_=(u) }
  override def grb_=(u: inVec3f) { yxz_=(u) }
  override def gra_=(u: inVec3f) { yxw_=(u) }
  override def gbr_=(u: inVec3f) { yzx_=(u) }
  override def gba_=(u: inVec3f) { yzw_=(u) }
  override def gar_=(u: inVec3f) { ywx_=(u) }
  override def gab_=(u: inVec3f) { ywz_=(u) }
  override def brg_=(u: inVec3f) { zxy_=(u) }
  override def bra_=(u: inVec3f) { zxw_=(u) }
  override def bgr_=(u: inVec3f) { zyx_=(u) }
  override def bga_=(u: inVec3f) { zyw_=(u) }
  override def bar_=(u: inVec3f) { zwx_=(u) }
  override def bag_=(u: inVec3f) { zwy_=(u) }
  override def arg_=(u: inVec3f) { wxy_=(u) }
  override def arb_=(u: inVec3f) { wxz_=(u) }
  override def agr_=(u: inVec3f) { wyx_=(u) }
  override def agb_=(u: inVec3f) { wyz_=(u) }
  override def abr_=(u: inVec3f) { wzx_=(u) }
  override def abg_=(u: inVec3f) { wzy_=(u) }

  override def rgba_=(u: inVec4f) { xyzw_=(u) }
  override def rgab_=(u: inVec4f) { xywz_=(u) }
  override def rbga_=(u: inVec4f) { xzyw_=(u) }
  override def rbag_=(u: inVec4f) { xzwy_=(u) }
  override def ragb_=(u: inVec4f) { xwyz_=(u) }
  override def rabg_=(u: inVec4f) { xwzy_=(u) }
  override def grba_=(u: inVec4f) { yxzw_=(u) }
  override def grab_=(u: inVec4f) { yxwz_=(u) }
  override def gbra_=(u: inVec4f) { yzxw_=(u) }
  override def gbar_=(u: inVec4f) { yzwx_=(u) }
  override def garb_=(u: inVec4f) { ywxz_=(u) }
  override def gabr_=(u: inVec4f) { ywzx_=(u) }
  override def brga_=(u: inVec4f) { zxyw_=(u) }
  override def brag_=(u: inVec4f) { zxwy_=(u) }
  override def bgra_=(u: inVec4f) { zyxw_=(u) }
  override def bgar_=(u: inVec4f) { zywx_=(u) }
  override def barg_=(u: inVec4f) { zwxy_=(u) }
  override def bagr_=(u: inVec4f) { zwyx_=(u) }
  override def argb_=(u: inVec4f) { wxyz_=(u) }
  override def arbg_=(u: inVec4f) { wxzy_=(u) }
  override def agrb_=(u: inVec4f) { wyxz_=(u) }
  override def agbr_=(u: inVec4f) { wyzx_=(u) }
  override def abrg_=(u: inVec4f) { wzxy_=(u) }
  override def abgr_=(u: inVec4f) { wzyx_=(u) }

  override def st_=(u: inVec2f) { xy_=(u) }
  override def sp_=(u: inVec2f) { xz_=(u) }
  override def sq_=(u: inVec2f) { xw_=(u) }
  override def ts_=(u: inVec2f) { yx_=(u) }
  override def tp_=(u: inVec2f) { yz_=(u) }
  override def tq_=(u: inVec2f) { yw_=(u) }
  override def ps_=(u: inVec2f) { zx_=(u) }
  override def pt_=(u: inVec2f) { zy_=(u) }
  override def pq_=(u: inVec2f) { zw_=(u) }
  override def qs_=(u: inVec2f) { wx_=(u) }
  override def qt_=(u: inVec2f) { wy_=(u) }
  override def qp_=(u: inVec2f) { wz_=(u) }

  override def stp_=(u: inVec3f) { xyz_=(u) }
  override def stq_=(u: inVec3f) { xyw_=(u) }
  override def spt_=(u: inVec3f) { xzy_=(u) }
  override def spq_=(u: inVec3f) { xzw_=(u) }
  override def sqt_=(u: inVec3f) { xwy_=(u) }
  override def sqp_=(u: inVec3f) { xwz_=(u) }
  override def tsp_=(u: inVec3f) { yxz_=(u) }
  override def tsq_=(u: inVec3f) { yxw_=(u) }
  override def tps_=(u: inVec3f) { yzx_=(u) }
  override def tpq_=(u: inVec3f) { yzw_=(u) }
  override def tqs_=(u: inVec3f) { ywx_=(u) }
  override def tqp_=(u: inVec3f) { ywz_=(u) }
  override def pst_=(u: inVec3f) { zxy_=(u) }
  override def psq_=(u: inVec3f) { zxw_=(u) }
  override def pts_=(u: inVec3f) { zyx_=(u) }
  override def ptq_=(u: inVec3f) { zyw_=(u) }
  override def pqs_=(u: inVec3f) { zwx_=(u) }
  override def pqt_=(u: inVec3f) { zwy_=(u) }
  override def qst_=(u: inVec3f) { wxy_=(u) }
  override def qsp_=(u: inVec3f) { wxz_=(u) }
  override def qts_=(u: inVec3f) { wyx_=(u) }
  override def qtp_=(u: inVec3f) { wyz_=(u) }
  override def qps_=(u: inVec3f) { wzx_=(u) }
  override def qpt_=(u: inVec3f) { wzy_=(u) }

  override def stpq_=(u: inVec4f) { xyzw_=(u) }
  override def stqp_=(u: inVec4f) { xywz_=(u) }
  override def sptq_=(u: inVec4f) { xzyw_=(u) }
  override def spqt_=(u: inVec4f) { xzwy_=(u) }
  override def sqtp_=(u: inVec4f) { xwyz_=(u) }
  override def sqpt_=(u: inVec4f) { xwzy_=(u) }
  override def tspq_=(u: inVec4f) { yxzw_=(u) }
  override def tsqp_=(u: inVec4f) { yxwz_=(u) }
  override def tpsq_=(u: inVec4f) { yzxw_=(u) }
  override def tpqs_=(u: inVec4f) { yzwx_=(u) }
  override def tqsp_=(u: inVec4f) { ywxz_=(u) }
  override def tqps_=(u: inVec4f) { ywzx_=(u) }
  override def pstq_=(u: inVec4f) { zxyw_=(u) }
  override def psqt_=(u: inVec4f) { zxwy_=(u) }
  override def ptsq_=(u: inVec4f) { zyxw_=(u) }
  override def ptqs_=(u: inVec4f) { zywx_=(u) }
  override def pqst_=(u: inVec4f) { zwxy_=(u) }
  override def pqts_=(u: inVec4f) { zwyx_=(u) }
  override def qstp_=(u: inVec4f) { wxyz_=(u) }
  override def qspt_=(u: inVec4f) { wxzy_=(u) }
  override def qtsp_=(u: inVec4f) { wyxz_=(u) }
  override def qtps_=(u: inVec4f) { wyzx_=(u) }
  override def qpst_=(u: inVec4f) { wzxy_=(u) }
  override def qpts_=(u: inVec4f) { wzyx_=(u) }
}


object Vec4f {
  final val Zero = new ConstVec4f(0, 0, 0, 0)
  final val UnitX = new ConstVec4f(1, 0, 0, 0)
  final val UnitY = new ConstVec4f(0, 1, 0, 0)
  final val UnitZ = new ConstVec4f(0, 0, 1, 0)
  final val UnitW = new ConstVec4f(0, 0, 0, 1)
  final val One = new ConstVec4f(1, 1, 1, 1)

  final val Manifest = classType[Vec4f](classOf[Vec4f])
  final val ConstManifest = classType[ConstVec4f](classOf[ConstVec4f])
  final val ReadManifest = classType[ReadVec4f](classOf[ReadVec4f])


  def apply(s: Float) = new Vec4f(s, s, s, s)
  def apply(x: Float, y: Float, z: Float, w: Float) = new Vec4f(x, y, z, w)

  def apply(u: AnyVec4[_]) = new Vec4f(u.fx, u.fy, u.fz, u.fw)
  def apply(xy: AnyVec2[_], z: Float, w: Float) = new Vec4f(xy.fx, xy.fy, z, w)
  def apply(x: Float, yz: AnyVec2[_], w: Float) = new Vec4f(x, yz.fx, yz.fy, w)
  def apply(x: Float, y: Float, zw: AnyVec2[_]) = new Vec4f(x, y, zw.fx, zw.fy)
  def apply(xy: AnyVec2[_], zw: AnyVec2[_]) = new Vec4f(xy.fx, xy.fy, zw.fx, zw.fy)
  def apply(xyz: AnyVec3[_], w: Float) = new Vec4f(xyz.fx, xyz.fy, xyz.fz, w)
  def apply(x: Float, yzw: AnyVec3[_]) = new Vec4f(x, yzw.fx, yzw.fy, yzw.fz)

  def apply(m: AnyMat2[_]) = new Vec4f(m.f00, m.f10, m.f01, m.f11)
  def apply(q: AnyQuat4[_]) = new Vec4f(q.fb, q.fc, q.fd, q.fa)

  def unapply(u: ReadVec4f) = Some((u.x, u.y, u.z, u.w))
  implicit def toMutable(u: ReadVec4f) = apply(u)

  implicit def castInt(u: AnyVec4[Int]) = new Vec4f(u.fx, u.fy, u.fz, u.fw)
}
