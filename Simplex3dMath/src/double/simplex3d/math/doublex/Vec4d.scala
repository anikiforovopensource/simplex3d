/*
 * Simplex3dMath - Double Module
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
package doublex

import scala.reflect.ClassManifest.{classType}
import simplex3d.math.integration._
import simplex3d.math.types._


/**
 * @author Aleksey Nikiforov (lex)
 */
@SerialVersionUID(8104346712419693669L)
sealed abstract class ReadVec4d extends ProtectedVec4d[Double]
with ReadPropertyValue[Vec4d] with Serializable
{

  type Clone <: ReadVec4d
  def toConst() :ConstVec4d
  final def mutableCopy() = Vec4d(this)

  private[math] type R2 = ReadVec2d
  private[math] type R3 = ReadVec3d
  private[math] type R4 = ReadVec4d

  private[math] type C2 = ConstVec2d
  private[math] type C3 = ConstVec3d
  private[math] type C4 = ConstVec4d

  protected final def make2(x: Double, y: Double) =
    new ConstVec2d(x, y)
  protected final def make3(x: Double, y: Double, z: Double) =
    new ConstVec3d(x, y, z)
  protected final def make4(x: Double, y: Double, z: Double, w: Double) =
    new ConstVec4d(x, y, z, w)

  private[math] final def bx: Boolean = simplex3d.math.toBoolean(x)
  private[math] final def by: Boolean = simplex3d.math.toBoolean(y)
  private[math] final def bz: Boolean = simplex3d.math.toBoolean(z)
  private[math] final def bw: Boolean = simplex3d.math.toBoolean(w)

  private[math] final def ix: Int = x.toInt
  private[math] final def iy: Int = y.toInt
  private[math] final def iz: Int = z.toInt
  private[math] final def iw: Int = w.toInt

  private[math] final def fx: Float = x.toFloat
  private[math] final def fy: Float = y.toFloat
  private[math] final def fz: Float = z.toFloat
  private[math] final def fw: Float = w.toFloat

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


  protected def x_=(s: Double) { throw new UnsupportedOperationException }
  protected def y_=(s: Double) { throw new UnsupportedOperationException }
  protected def z_=(s: Double) { throw new UnsupportedOperationException }
  protected def w_=(s: Double) { throw new UnsupportedOperationException }

  protected def r_=(s: Double) { throw new UnsupportedOperationException }
  protected def g_=(s: Double) { throw new UnsupportedOperationException }
  protected def b_=(s: Double) { throw new UnsupportedOperationException }
  protected def a_=(s: Double) { throw new UnsupportedOperationException }

  protected def s_=(s: Double) { throw new UnsupportedOperationException }
  protected def t_=(s: Double) { throw new UnsupportedOperationException }
  protected def p_=(s: Double) { throw new UnsupportedOperationException }
  protected def q_=(s: Double) { throw new UnsupportedOperationException }

  final def apply(i: Int) :Double = {
    i match {
      case 0 => x
      case 1 => y
      case 2 => z
      case 3 => w
      case j => throw new IndexOutOfBoundsException(
          "Trying to read index (" + j + ") in " + this.getClass.getSimpleName + "."
        )
    }
  }

  final def unary_+() :ReadVec4d = this
  final def unary_-() = new Vec4d(-x, -y, -z, -w)

  final def *(s: Double) = new Vec4d(x * s, y * s, z * s, w * s)
  final def /(s: Double) = { val inv = 1/s; new Vec4d(x * inv, y * inv, z * inv, w * inv) }
  private[math] final def divByComp(s: Double) = new Vec4d(s / x, s / y, s / z, s / w)
  final def +(s: Double) = new Vec4d(x + s, y + s, z + s, w + s)
  final def -(s: Double) = new Vec4d(x - s, y - s, z - s, w - s)

  final def *(u: inVec4d) = new Vec4d(x * u.x, y * u.y, z * u.z, w * u.w)
  final def /(u: inVec4d) = new Vec4d(x / u.x, y / u.y, z / u.z, w / u.w)
  final def +(u: inVec4d) = new Vec4d(x + u.x, y + u.y, z + u.z, w + u.w)
  final def -(u: inVec4d) = new Vec4d(x - u.x, y - u.y, z - u.z, w - u.w)

  final def *(m: inMat2x4d) :Vec2d = m.transposeMult(this)
  final def *(m: inMat3x4d) :Vec3d = m.transposeMult(this)
  final def *(m: inMat4d) :Vec4d = m.transposeMult(this)


  final override def equals(other: Any) :Boolean = {
    other match {
      case u: ReadVec4b => false
      case u: AnyVec4[_] => x == u.dx && y == u.dy && z == u.dz && w == u.dw
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
    prefix + "Vec4" + "(" + x + ", " + y + ", " + z + ", " + w + ")"
  }
}


@SerialVersionUID(8104346712419693669L)
final class ConstVec4d private[math] (cx: Double, cy: Double, cz: Double, cw: Double)
extends ReadVec4d with Immutable with Serializable {
  px = cx; py = cy; pz = cz; pw = cw

  type Clone = ConstVec4d
  override def clone() = this
  def toConst() = this
}


object ConstVec4d {

  def apply(s: Double) = new ConstVec4d(s, s, s, s)
  def apply(x: Double, y: Double, z: Double, w: Double) = new ConstVec4d(x, y, z, w)

  def apply(u: AnyVec4[_]) = new ConstVec4d(u.dx, u.dy, u.dz, u.dw)
  def apply(xy: AnyVec2[_], z: Double, w: Double) = new ConstVec4d(xy.dx, xy.dy, z, w)
  def apply(x: Double, yz: AnyVec2[_], w: Double) = new ConstVec4d(x, yz.dx, yz.dy, w)
  def apply(x: Double, y: Double, zw: AnyVec2[_]) = new ConstVec4d(x, y, zw.dx, zw.dy)
  def apply(xy: AnyVec2[_], zw: AnyVec2[_]) = new ConstVec4d(xy.dx, xy.dy, zw.dx, zw.dy)
  def apply(xyz: AnyVec3[_], w: Double) = new ConstVec4d(xyz.dx, xyz.dy, xyz.dz, w)
  def apply(x: Double, yzw: AnyVec3[_]) = new ConstVec4d(x, yzw.dx, yzw.dy, yzw.dz)

  def apply(m: AnyMat2[_]) = new ConstVec4d(m.d00, m.d01, m.d10, m.d11)
  def apply(q: AnyQuat4[_]) = new ConstVec4d(q.db, q.dc, q.dd, q.da)

  implicit def toConst(u: ReadVec4d) = apply(u)
}


@SerialVersionUID(8104346712419693669L)
final class Vec4d private[math] (cx: Double, cy: Double, cz: Double, cw: Double)
extends ReadVec4d with Accessor with CompositeFormat
with PropertyValue[Vec4d] with Serializable
{
  px = cx; py = cy; pz = cz; pw = cw

  private[math] def this() { this(0, 0, 0, 0) }
  
  type Clone = Vec4d
  type Read = ReadVec4d
  type Const = ConstVec4d

  type Accessor = Vec4d
  type Component = RDouble

  override def clone() = Vec4d(this)
  def toConst() = ConstVec4d(this)
  def :=(u: ConstVec4d) { this := u.asInstanceOf[inVec4d] }
  def :=(r: Readable[Vec4d]) { val u = r.asInstanceOf[ReadVec4d]; x = u.x; y = u.y; z = u.z; w = u.w }


  @noinline override def x_=(s: Double) { px = s }
  @noinline override def y_=(s: Double) { py = s }
  @noinline override def z_=(s: Double) { pz = s }
  @noinline override def w_=(s: Double) { pw = s }

  override def r_=(s: Double) { px = s }
  override def g_=(s: Double) { py = s }
  override def b_=(s: Double) { pz = s }
  override def a_=(s: Double) { pw = s }

  override def s_=(s: Double) { px = s }
  override def t_=(s: Double) { py = s }
  override def p_=(s: Double) { pz = s }
  override def q_=(s: Double) { pw = s }

  def update(i: Int, s: Double) {
    i match {
      case 0 => x = s
      case 1 => y = s
      case 2 => z = s
      case 3 => w = s
      case j => throw new IndexOutOfBoundsException(
          "Trying to update index (" + j + ") in " + this.getClass.getSimpleName + "."
        )
    }
  }

  def *=(s: Double) { x *= s; y *= s; z *= s; w *= s }
  def /=(s: Double) { val inv = 1/s; x *= inv; y *= inv; z *= inv; w *= inv }
  def +=(s: Double) { x += s; y += s; z += s; w += s }
  def -=(s: Double) { x -= s; y -= s; z -= s; w -= s }

  def *=(u: inVec4d) { x *= u.x; y *= u.y; z *= u.z; w *= u.w }
  def /=(u: inVec4d) { x /= u.x; y /= u.y; z /= u.z; w /= u.w }
  def +=(u: inVec4d) { x += u.x; y += u.y; z += u.z; w += u.w }
  def -=(u: inVec4d) { x -= u.x; y -= u.y; z -= u.z; w -= u.w }

  def *=(m: inMat4d) { this := m.transposeMult(this) }

  // @SwizzlingStart
  override def xy_=(u: inVec2d) { x = u.x; y = u.y }
  override def xz_=(u: inVec2d) { x = u.x; z = u.y }
  override def xw_=(u: inVec2d) { x = u.x; w = u.y }
  override def yx_=(u: inVec2d) { y = u.x; x = u.y }
  override def yz_=(u: inVec2d) { y = u.x; z = u.y }
  override def yw_=(u: inVec2d) { y = u.x; w = u.y }
  override def zx_=(u: inVec2d) { z = u.x; x = u.y }
  override def zy_=(u: inVec2d) { z = u.x; y = u.y }
  override def zw_=(u: inVec2d) { z = u.x; w = u.y }
  override def wx_=(u: inVec2d) { w = u.x; x = u.y }
  override def wy_=(u: inVec2d) { w = u.x; y = u.y }
  override def wz_=(u: inVec2d) { w = u.x; z = u.y }

  override def xyz_=(u: inVec3d) { x = u.x; y = u.y; z = u.z }
  override def xyw_=(u: inVec3d) { x = u.x; y = u.y; w = u.z }
  override def xzy_=(u: inVec3d) { x = u.x; z = u.y; y = u.z }
  override def xzw_=(u: inVec3d) { x = u.x; z = u.y; w = u.z }
  override def xwy_=(u: inVec3d) { x = u.x; w = u.y; y = u.z }
  override def xwz_=(u: inVec3d) { x = u.x; w = u.y; z = u.z }
  override def yxz_=(u: inVec3d) { y = u.x; x = u.y; z = u.z }
  override def yxw_=(u: inVec3d) { y = u.x; x = u.y; w = u.z }
  override def yzx_=(u: inVec3d) { y = u.x; z = u.y; x = u.z }
  override def yzw_=(u: inVec3d) { y = u.x; z = u.y; w = u.z }
  override def ywx_=(u: inVec3d) { y = u.x; w = u.y; x = u.z }
  override def ywz_=(u: inVec3d) { y = u.x; w = u.y; z = u.z }
  override def zxy_=(u: inVec3d) { z = u.x; x = u.y; y = u.z }
  override def zxw_=(u: inVec3d) { z = u.x; x = u.y; w = u.z }
  override def zyx_=(u: inVec3d) { z = u.x; y = u.y; x = u.z }
  override def zyw_=(u: inVec3d) { z = u.x; y = u.y; w = u.z }
  override def zwx_=(u: inVec3d) { z = u.x; w = u.y; x = u.z }
  override def zwy_=(u: inVec3d) { z = u.x; w = u.y; y = u.z }
  override def wxy_=(u: inVec3d) { w = u.x; x = u.y; y = u.z }
  override def wxz_=(u: inVec3d) { w = u.x; x = u.y; z = u.z }
  override def wyx_=(u: inVec3d) { w = u.x; y = u.y; x = u.z }
  override def wyz_=(u: inVec3d) { w = u.x; y = u.y; z = u.z }
  override def wzx_=(u: inVec3d) { w = u.x; z = u.y; x = u.z }
  override def wzy_=(u: inVec3d) { w = u.x; z = u.y; y = u.z }

  override def xyzw_=(u: inVec4d) { x = u.x; y = u.y; z = u.z; w = u.w }
  override def xywz_=(u: inVec4d) { x = u.x; y = u.y; var t = u.w; w = u.z; z = t }
  override def xzyw_=(u: inVec4d) { x = u.x; var t = u.z; z = u.y; y = t; w = u.w }
  override def xzwy_=(u: inVec4d) { x = u.x; var t = u.z; z = u.y; y = u.w; w = t }
  override def xwyz_=(u: inVec4d) { x = u.x; var t = u.w; w = u.y; y = u.z; z = t }
  override def xwzy_=(u: inVec4d) { x = u.x; var t = u.w; w = u.y; y = t; z = u.z }
  override def yxzw_=(u: inVec4d) { var t = u.y; y = u.x; x = t; z = u.z; w = u.w }
  override def yxwz_=(u: inVec4d) { var t = u.y; y = u.x; x = t; t = u.w; w = u.z; z = t }
  override def yzxw_=(u: inVec4d) { var t = u.y; y = u.x; x = u.z; z = t; w = u.w }
  override def yzwx_=(u: inVec4d) { var t = u.y; y = u.x; x = u.w; w = u.z; z = t }
  override def ywxz_=(u: inVec4d) { var t = u.y; y = u.x; x = u.z; z = u.w; w = t }
  override def ywzx_=(u: inVec4d) { var t = u.y; y = u.x; x = u.w; w = t; z = u.z }
  override def zxyw_=(u: inVec4d) { var t = u.z; z = u.x; x = u.y; y = t; w = u.w }
  override def zxwy_=(u: inVec4d) { var t = u.z; z = u.x; x = u.y; y = u.w; w = t }
  override def zyxw_=(u: inVec4d) { var t = u.z; z = u.x; x = t; y = u.y; w = u.w }
  override def zywx_=(u: inVec4d) { var t = u.z; z = u.x; x = u.w; w = t; y = u.y }
  override def zwxy_=(u: inVec4d) { var t = u.z; z = u.x; x = t; t = u.w; w = u.y; y = t }
  override def zwyx_=(u: inVec4d) { var t = u.z; z = u.x; x = u.w; w = u.y; y = t }
  override def wxyz_=(u: inVec4d) { var t = u.w; w = u.x; x = u.y; y = u.z; z = t }
  override def wxzy_=(u: inVec4d) { var t = u.w; w = u.x; x = u.y; y = t; z = u.z }
  override def wyxz_=(u: inVec4d) { var t = u.w; w = u.x; x = u.z; z = t; y = u.y }
  override def wyzx_=(u: inVec4d) { var t = u.w; w = u.x; x = t; y = u.y; z = u.z }
  override def wzxy_=(u: inVec4d) { var t = u.w; w = u.x; x = u.z; z = u.y; y = t }
  override def wzyx_=(u: inVec4d) { var t = u.w; w = u.x; x = t; t = u.z; z = u.y; y = t }

  override def rg_=(u: inVec2d) { xy_=(u) }
  override def rb_=(u: inVec2d) { xz_=(u) }
  override def ra_=(u: inVec2d) { xw_=(u) }
  override def gr_=(u: inVec2d) { yx_=(u) }
  override def gb_=(u: inVec2d) { yz_=(u) }
  override def ga_=(u: inVec2d) { yw_=(u) }
  override def br_=(u: inVec2d) { zx_=(u) }
  override def bg_=(u: inVec2d) { zy_=(u) }
  override def ba_=(u: inVec2d) { zw_=(u) }
  override def ar_=(u: inVec2d) { wx_=(u) }
  override def ag_=(u: inVec2d) { wy_=(u) }
  override def ab_=(u: inVec2d) { wz_=(u) }

  override def rgb_=(u: inVec3d) { xyz_=(u) }
  override def rga_=(u: inVec3d) { xyw_=(u) }
  override def rbg_=(u: inVec3d) { xzy_=(u) }
  override def rba_=(u: inVec3d) { xzw_=(u) }
  override def rag_=(u: inVec3d) { xwy_=(u) }
  override def rab_=(u: inVec3d) { xwz_=(u) }
  override def grb_=(u: inVec3d) { yxz_=(u) }
  override def gra_=(u: inVec3d) { yxw_=(u) }
  override def gbr_=(u: inVec3d) { yzx_=(u) }
  override def gba_=(u: inVec3d) { yzw_=(u) }
  override def gar_=(u: inVec3d) { ywx_=(u) }
  override def gab_=(u: inVec3d) { ywz_=(u) }
  override def brg_=(u: inVec3d) { zxy_=(u) }
  override def bra_=(u: inVec3d) { zxw_=(u) }
  override def bgr_=(u: inVec3d) { zyx_=(u) }
  override def bga_=(u: inVec3d) { zyw_=(u) }
  override def bar_=(u: inVec3d) { zwx_=(u) }
  override def bag_=(u: inVec3d) { zwy_=(u) }
  override def arg_=(u: inVec3d) { wxy_=(u) }
  override def arb_=(u: inVec3d) { wxz_=(u) }
  override def agr_=(u: inVec3d) { wyx_=(u) }
  override def agb_=(u: inVec3d) { wyz_=(u) }
  override def abr_=(u: inVec3d) { wzx_=(u) }
  override def abg_=(u: inVec3d) { wzy_=(u) }

  override def rgba_=(u: inVec4d) { xyzw_=(u) }
  override def rgab_=(u: inVec4d) { xywz_=(u) }
  override def rbga_=(u: inVec4d) { xzyw_=(u) }
  override def rbag_=(u: inVec4d) { xzwy_=(u) }
  override def ragb_=(u: inVec4d) { xwyz_=(u) }
  override def rabg_=(u: inVec4d) { xwzy_=(u) }
  override def grba_=(u: inVec4d) { yxzw_=(u) }
  override def grab_=(u: inVec4d) { yxwz_=(u) }
  override def gbra_=(u: inVec4d) { yzxw_=(u) }
  override def gbar_=(u: inVec4d) { yzwx_=(u) }
  override def garb_=(u: inVec4d) { ywxz_=(u) }
  override def gabr_=(u: inVec4d) { ywzx_=(u) }
  override def brga_=(u: inVec4d) { zxyw_=(u) }
  override def brag_=(u: inVec4d) { zxwy_=(u) }
  override def bgra_=(u: inVec4d) { zyxw_=(u) }
  override def bgar_=(u: inVec4d) { zywx_=(u) }
  override def barg_=(u: inVec4d) { zwxy_=(u) }
  override def bagr_=(u: inVec4d) { zwyx_=(u) }
  override def argb_=(u: inVec4d) { wxyz_=(u) }
  override def arbg_=(u: inVec4d) { wxzy_=(u) }
  override def agrb_=(u: inVec4d) { wyxz_=(u) }
  override def agbr_=(u: inVec4d) { wyzx_=(u) }
  override def abrg_=(u: inVec4d) { wzxy_=(u) }
  override def abgr_=(u: inVec4d) { wzyx_=(u) }

  override def st_=(u: inVec2d) { xy_=(u) }
  override def sp_=(u: inVec2d) { xz_=(u) }
  override def sq_=(u: inVec2d) { xw_=(u) }
  override def ts_=(u: inVec2d) { yx_=(u) }
  override def tp_=(u: inVec2d) { yz_=(u) }
  override def tq_=(u: inVec2d) { yw_=(u) }
  override def ps_=(u: inVec2d) { zx_=(u) }
  override def pt_=(u: inVec2d) { zy_=(u) }
  override def pq_=(u: inVec2d) { zw_=(u) }
  override def qs_=(u: inVec2d) { wx_=(u) }
  override def qt_=(u: inVec2d) { wy_=(u) }
  override def qp_=(u: inVec2d) { wz_=(u) }

  override def stp_=(u: inVec3d) { xyz_=(u) }
  override def stq_=(u: inVec3d) { xyw_=(u) }
  override def spt_=(u: inVec3d) { xzy_=(u) }
  override def spq_=(u: inVec3d) { xzw_=(u) }
  override def sqt_=(u: inVec3d) { xwy_=(u) }
  override def sqp_=(u: inVec3d) { xwz_=(u) }
  override def tsp_=(u: inVec3d) { yxz_=(u) }
  override def tsq_=(u: inVec3d) { yxw_=(u) }
  override def tps_=(u: inVec3d) { yzx_=(u) }
  override def tpq_=(u: inVec3d) { yzw_=(u) }
  override def tqs_=(u: inVec3d) { ywx_=(u) }
  override def tqp_=(u: inVec3d) { ywz_=(u) }
  override def pst_=(u: inVec3d) { zxy_=(u) }
  override def psq_=(u: inVec3d) { zxw_=(u) }
  override def pts_=(u: inVec3d) { zyx_=(u) }
  override def ptq_=(u: inVec3d) { zyw_=(u) }
  override def pqs_=(u: inVec3d) { zwx_=(u) }
  override def pqt_=(u: inVec3d) { zwy_=(u) }
  override def qst_=(u: inVec3d) { wxy_=(u) }
  override def qsp_=(u: inVec3d) { wxz_=(u) }
  override def qts_=(u: inVec3d) { wyx_=(u) }
  override def qtp_=(u: inVec3d) { wyz_=(u) }
  override def qps_=(u: inVec3d) { wzx_=(u) }
  override def qpt_=(u: inVec3d) { wzy_=(u) }

  override def stpq_=(u: inVec4d) { xyzw_=(u) }
  override def stqp_=(u: inVec4d) { xywz_=(u) }
  override def sptq_=(u: inVec4d) { xzyw_=(u) }
  override def spqt_=(u: inVec4d) { xzwy_=(u) }
  override def sqtp_=(u: inVec4d) { xwyz_=(u) }
  override def sqpt_=(u: inVec4d) { xwzy_=(u) }
  override def tspq_=(u: inVec4d) { yxzw_=(u) }
  override def tsqp_=(u: inVec4d) { yxwz_=(u) }
  override def tpsq_=(u: inVec4d) { yzxw_=(u) }
  override def tpqs_=(u: inVec4d) { yzwx_=(u) }
  override def tqsp_=(u: inVec4d) { ywxz_=(u) }
  override def tqps_=(u: inVec4d) { ywzx_=(u) }
  override def pstq_=(u: inVec4d) { zxyw_=(u) }
  override def psqt_=(u: inVec4d) { zxwy_=(u) }
  override def ptsq_=(u: inVec4d) { zyxw_=(u) }
  override def ptqs_=(u: inVec4d) { zywx_=(u) }
  override def pqst_=(u: inVec4d) { zwxy_=(u) }
  override def pqts_=(u: inVec4d) { zwyx_=(u) }
  override def qstp_=(u: inVec4d) { wxyz_=(u) }
  override def qspt_=(u: inVec4d) { wxzy_=(u) }
  override def qtsp_=(u: inVec4d) { wyxz_=(u) }
  override def qtps_=(u: inVec4d) { wyzx_=(u) }
  override def qpst_=(u: inVec4d) { wzxy_=(u) }
  override def qpts_=(u: inVec4d) { wzyx_=(u) }
  // @SwizzlingEnd
}


object Vec4d {
  final val Zero = new ConstVec4d(0, 0, 0, 0)
  final val UnitX = new ConstVec4d(1, 0, 0, 0)
  final val UnitY = new ConstVec4d(0, 1, 0, 0)
  final val UnitZ = new ConstVec4d(0, 0, 1, 0)
  final val UnitW = new ConstVec4d(0, 0, 0, 1)
  final val One = new ConstVec4d(1, 1, 1, 1)

  final val Manifest = classType[Vec4d](classOf[Vec4d])
  final val ConstManifest = classType[ConstVec4d](classOf[ConstVec4d])
  final val ReadManifest = classType[ReadVec4d](classOf[ReadVec4d])


  def apply(s: Double) = new Vec4d(s, s, s, s)
  def apply(x: Double, y: Double, z: Double, w: Double) = new Vec4d(x, y, z, w)

  def apply(u: AnyVec4[_]) = new Vec4d(u.dx, u.dy, u.dz, u.dw)
  def apply(xy: AnyVec2[_], z: Double, w: Double) = new Vec4d(xy.dx, xy.dy, z, w)
  def apply(x: Double, yz: AnyVec2[_], w: Double) = new Vec4d(x, yz.dx, yz.dy, w)
  def apply(x: Double, y: Double, zw: AnyVec2[_]) = new Vec4d(x, y, zw.dx, zw.dy)
  def apply(xy: AnyVec2[_], zw: AnyVec2[_]) = new Vec4d(xy.dx, xy.dy, zw.dx, zw.dy)
  def apply(xyz: AnyVec3[_], w: Double) = new Vec4d(xyz.dx, xyz.dy, xyz.dz, w)
  def apply(x: Double, yzw: AnyVec3[_]) = new Vec4d(x, yzw.dx, yzw.dy, yzw.dz)

  def apply(m: AnyMat2[_]) = new Vec4d(m.d00, m.d01, m.d10, m.d11)
  def apply(q: AnyQuat4[_]) = new Vec4d(q.db, q.dc, q.dd, q.da)

  def unapply(u: ReadVec4d) = Some((u.x, u.y, u.z, u.w))
}
