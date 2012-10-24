/*
 * Simplex3dMath - Core Module
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

import scala.reflect.ClassManifest.{classType}
import simplex3d.math.integration._
import simplex3d.math.types._
import simplex3d.math.{toBoolean => toBool}


/**
 * @author Aleksey Nikiforov (lex)
 */
@SerialVersionUID(8104346712419693669L)
sealed abstract class ReadVec4b extends ProtectedVec4b[Boolean]
with Protected with Serializable
{

  type Clone <: ReadVec4b
  def toConst() :ConstVec4b
  
  type Read = ReadVec4b
  type Mutable = Vec4b
  final def readType: Class[Read] = classOf[ReadVec4b]
  final def mutableCopy() = Vec4b(this)

  private[math] type R2 = ReadVec2b
  private[math] type R3 = ReadVec3b
  private[math] type R4 = ReadVec4b

  private[math] type C2 = ConstVec2b
  private[math] type C3 = ConstVec3b
  private[math] type C4 = ConstVec4b

  protected final def make2(x: Double, y: Double) =
    new ConstVec2b(toBool(x), toBool(y))
  protected final def make3(x: Double, y: Double, z: Double) =
    new ConstVec3b(toBool(x), toBool(y), toBool(z))
  protected final def make4(x: Double, y: Double, z: Double, w: Double) =
    new ConstVec4b(toBool(x), toBool(y), toBool(z), toBool(w))

  private[math] final def bx: Boolean = x
  private[math] final def by: Boolean = y
  private[math] final def bz: Boolean = z
  private[math] final def bw: Boolean = w

  private[math] final def ix: Int = simplex3d.math.toInt(x)
  private[math] final def iy: Int = simplex3d.math.toInt(y)
  private[math] final def iz: Int = simplex3d.math.toInt(z)
  private[math] final def iw: Int = simplex3d.math.toInt(w)

  private[math] final def fx: Float = simplex3d.math.toFloat(x)
  private[math] final def fy: Float = simplex3d.math.toFloat(y)
  private[math] final def fz: Float = simplex3d.math.toFloat(z)
  private[math] final def fw: Float = simplex3d.math.toFloat(w)

  private[math] final def dx: Double = simplex3d.math.toDouble(x)
  private[math] final def dy: Double = simplex3d.math.toDouble(y)
  private[math] final def dz: Double = simplex3d.math.toDouble(z)
  private[math] final def dw: Double = simplex3d.math.toDouble(w)


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


  protected def x_=(s: Boolean) { throw new UnsupportedOperationException }
  protected def y_=(s: Boolean) { throw new UnsupportedOperationException }
  protected def z_=(s: Boolean) { throw new UnsupportedOperationException }
  protected def w_=(s: Boolean) { throw new UnsupportedOperationException }

  protected def r_=(s: Boolean) { throw new UnsupportedOperationException }
  protected def g_=(s: Boolean) { throw new UnsupportedOperationException }
  protected def b_=(s: Boolean) { throw new UnsupportedOperationException }
  protected def a_=(s: Boolean) { throw new UnsupportedOperationException }

  protected def s_=(s: Boolean) { throw new UnsupportedOperationException }
  protected def t_=(s: Boolean) { throw new UnsupportedOperationException }
  protected def p_=(s: Boolean) { throw new UnsupportedOperationException }
  protected def q_=(s: Boolean) { throw new UnsupportedOperationException }

  final def apply(i: Int) :Boolean = {
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



  final override def equals(other: Any) :Boolean = {
    other match {
      case u: ReadVec4b => x == u.x && y == u.y && z == u.z && w == u.w
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
    prefix + "Vec4b" + "(" + x + ", " + y + ", " + z + ", " + w + ")"
  }
}


@SerialVersionUID(8104346712419693669L)
final class ConstVec4b private[math] (cx: Boolean, cy: Boolean, cz: Boolean, cw: Boolean)
extends ReadVec4b with Immutable with Serializable {
  px = cx; py = cy; pz = cz; pw = cw

  type Clone = ConstVec4b
  override def clone() = this
  def toConst() = this
}


object ConstVec4b {

  def apply(s: Boolean) = new ConstVec4b(s, s, s, s)
  def apply(x: Boolean, y: Boolean, z: Boolean, w: Boolean) = new ConstVec4b(x, y, z, w)

  def apply(u: AnyVec4[_]) = new ConstVec4b(u.bx, u.by, u.bz, u.bw)
  def apply(xy: AnyVec2[_], z: Boolean, w: Boolean) = new ConstVec4b(xy.bx, xy.by, z, w)
  def apply(x: Boolean, yz: AnyVec2[_], w: Boolean) = new ConstVec4b(x, yz.bx, yz.by, w)
  def apply(x: Boolean, y: Boolean, zw: AnyVec2[_]) = new ConstVec4b(x, y, zw.bx, zw.by)
  def apply(xy: AnyVec2[_], zw: AnyVec2[_]) = new ConstVec4b(xy.bx, xy.by, zw.bx, zw.by)
  def apply(xyz: AnyVec3[_], w: Boolean) = new ConstVec4b(xyz.bx, xyz.by, xyz.bz, w)
  def apply(x: Boolean, yzw: AnyVec3[_]) = new ConstVec4b(x, yzw.bx, yzw.by, yzw.bz)

  def apply(m: AnyMat2[_]) = new ConstVec4b(toBool(m.d00), toBool(m.d01), toBool(m.d10), toBool(m.d11))
  def apply(q: AnyQuat4[_]) = new ConstVec4b(toBool(q.db), toBool(q.dc), toBool(q.dd), toBool(q.da))

  implicit def toConst(u: ReadVec4b) = apply(u)
}


@SerialVersionUID(8104346712419693669L)
final class Vec4b private[math] (cx: Boolean, cy: Boolean, cz: Boolean, cw: Boolean)
extends ReadVec4b with Accessor with CompositeFormat
with Accessible with Serializable
{
  px = cx; py = cy; pz = cz; pw = cw

  private[math] def this() { this(false, false, false, false) }
  
  type Clone = Vec4b

  type Const = ConstVec4b
  type Accessor = Vec4b
  type Component = SInt

  override def clone() = Vec4b(this)
  def toConst() = ConstVec4b(this)
  def :=(u: inVec4b) { x = u.x; y = u.y; z = u.z; w = u.w }


  @noinline override def x_=(s: Boolean) { px = s }
  @noinline override def y_=(s: Boolean) { py = s }
  @noinline override def z_=(s: Boolean) { pz = s }
  @noinline override def w_=(s: Boolean) { pw = s }

  override def r_=(s: Boolean) { px = s }
  override def g_=(s: Boolean) { py = s }
  override def b_=(s: Boolean) { pz = s }
  override def a_=(s: Boolean) { pw = s }

  override def s_=(s: Boolean) { px = s }
  override def t_=(s: Boolean) { py = s }
  override def p_=(s: Boolean) { pz = s }
  override def q_=(s: Boolean) { pw = s }

  def update(i: Int, s: Boolean) {
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


  // @SwizzlingStart
  override def xy_=(u: inVec2b) { x = u.x; y = u.y }
  override def xz_=(u: inVec2b) { x = u.x; z = u.y }
  override def xw_=(u: inVec2b) { x = u.x; w = u.y }
  override def yx_=(u: inVec2b) { y = u.x; x = u.y }
  override def yz_=(u: inVec2b) { y = u.x; z = u.y }
  override def yw_=(u: inVec2b) { y = u.x; w = u.y }
  override def zx_=(u: inVec2b) { z = u.x; x = u.y }
  override def zy_=(u: inVec2b) { z = u.x; y = u.y }
  override def zw_=(u: inVec2b) { z = u.x; w = u.y }
  override def wx_=(u: inVec2b) { w = u.x; x = u.y }
  override def wy_=(u: inVec2b) { w = u.x; y = u.y }
  override def wz_=(u: inVec2b) { w = u.x; z = u.y }

  override def xyz_=(u: inVec3b) { x = u.x; y = u.y; z = u.z }
  override def xyw_=(u: inVec3b) { x = u.x; y = u.y; w = u.z }
  override def xzy_=(u: inVec3b) { x = u.x; z = u.y; y = u.z }
  override def xzw_=(u: inVec3b) { x = u.x; z = u.y; w = u.z }
  override def xwy_=(u: inVec3b) { x = u.x; w = u.y; y = u.z }
  override def xwz_=(u: inVec3b) { x = u.x; w = u.y; z = u.z }
  override def yxz_=(u: inVec3b) { y = u.x; x = u.y; z = u.z }
  override def yxw_=(u: inVec3b) { y = u.x; x = u.y; w = u.z }
  override def yzx_=(u: inVec3b) { y = u.x; z = u.y; x = u.z }
  override def yzw_=(u: inVec3b) { y = u.x; z = u.y; w = u.z }
  override def ywx_=(u: inVec3b) { y = u.x; w = u.y; x = u.z }
  override def ywz_=(u: inVec3b) { y = u.x; w = u.y; z = u.z }
  override def zxy_=(u: inVec3b) { z = u.x; x = u.y; y = u.z }
  override def zxw_=(u: inVec3b) { z = u.x; x = u.y; w = u.z }
  override def zyx_=(u: inVec3b) { z = u.x; y = u.y; x = u.z }
  override def zyw_=(u: inVec3b) { z = u.x; y = u.y; w = u.z }
  override def zwx_=(u: inVec3b) { z = u.x; w = u.y; x = u.z }
  override def zwy_=(u: inVec3b) { z = u.x; w = u.y; y = u.z }
  override def wxy_=(u: inVec3b) { w = u.x; x = u.y; y = u.z }
  override def wxz_=(u: inVec3b) { w = u.x; x = u.y; z = u.z }
  override def wyx_=(u: inVec3b) { w = u.x; y = u.y; x = u.z }
  override def wyz_=(u: inVec3b) { w = u.x; y = u.y; z = u.z }
  override def wzx_=(u: inVec3b) { w = u.x; z = u.y; x = u.z }
  override def wzy_=(u: inVec3b) { w = u.x; z = u.y; y = u.z }

  override def xyzw_=(u: inVec4b) { x = u.x; y = u.y; z = u.z; w = u.w }
  override def xywz_=(u: inVec4b) { x = u.x; y = u.y; var t = u.w; w = u.z; z = t }
  override def xzyw_=(u: inVec4b) { x = u.x; var t = u.z; z = u.y; y = t; w = u.w }
  override def xzwy_=(u: inVec4b) { x = u.x; var t = u.z; z = u.y; y = u.w; w = t }
  override def xwyz_=(u: inVec4b) { x = u.x; var t = u.w; w = u.y; y = u.z; z = t }
  override def xwzy_=(u: inVec4b) { x = u.x; var t = u.w; w = u.y; y = t; z = u.z }
  override def yxzw_=(u: inVec4b) { var t = u.y; y = u.x; x = t; z = u.z; w = u.w }
  override def yxwz_=(u: inVec4b) { var t = u.y; y = u.x; x = t; t = u.w; w = u.z; z = t }
  override def yzxw_=(u: inVec4b) { var t = u.y; y = u.x; x = u.z; z = t; w = u.w }
  override def yzwx_=(u: inVec4b) { var t = u.y; y = u.x; x = u.w; w = u.z; z = t }
  override def ywxz_=(u: inVec4b) { var t = u.y; y = u.x; x = u.z; z = u.w; w = t }
  override def ywzx_=(u: inVec4b) { var t = u.y; y = u.x; x = u.w; w = t; z = u.z }
  override def zxyw_=(u: inVec4b) { var t = u.z; z = u.x; x = u.y; y = t; w = u.w }
  override def zxwy_=(u: inVec4b) { var t = u.z; z = u.x; x = u.y; y = u.w; w = t }
  override def zyxw_=(u: inVec4b) { var t = u.z; z = u.x; x = t; y = u.y; w = u.w }
  override def zywx_=(u: inVec4b) { var t = u.z; z = u.x; x = u.w; w = t; y = u.y }
  override def zwxy_=(u: inVec4b) { var t = u.z; z = u.x; x = t; t = u.w; w = u.y; y = t }
  override def zwyx_=(u: inVec4b) { var t = u.z; z = u.x; x = u.w; w = u.y; y = t }
  override def wxyz_=(u: inVec4b) { var t = u.w; w = u.x; x = u.y; y = u.z; z = t }
  override def wxzy_=(u: inVec4b) { var t = u.w; w = u.x; x = u.y; y = t; z = u.z }
  override def wyxz_=(u: inVec4b) { var t = u.w; w = u.x; x = u.z; z = t; y = u.y }
  override def wyzx_=(u: inVec4b) { var t = u.w; w = u.x; x = t; y = u.y; z = u.z }
  override def wzxy_=(u: inVec4b) { var t = u.w; w = u.x; x = u.z; z = u.y; y = t }
  override def wzyx_=(u: inVec4b) { var t = u.w; w = u.x; x = t; t = u.z; z = u.y; y = t }

  override def rg_=(u: inVec2b) { xy_=(u) }
  override def rb_=(u: inVec2b) { xz_=(u) }
  override def ra_=(u: inVec2b) { xw_=(u) }
  override def gr_=(u: inVec2b) { yx_=(u) }
  override def gb_=(u: inVec2b) { yz_=(u) }
  override def ga_=(u: inVec2b) { yw_=(u) }
  override def br_=(u: inVec2b) { zx_=(u) }
  override def bg_=(u: inVec2b) { zy_=(u) }
  override def ba_=(u: inVec2b) { zw_=(u) }
  override def ar_=(u: inVec2b) { wx_=(u) }
  override def ag_=(u: inVec2b) { wy_=(u) }
  override def ab_=(u: inVec2b) { wz_=(u) }

  override def rgb_=(u: inVec3b) { xyz_=(u) }
  override def rga_=(u: inVec3b) { xyw_=(u) }
  override def rbg_=(u: inVec3b) { xzy_=(u) }
  override def rba_=(u: inVec3b) { xzw_=(u) }
  override def rag_=(u: inVec3b) { xwy_=(u) }
  override def rab_=(u: inVec3b) { xwz_=(u) }
  override def grb_=(u: inVec3b) { yxz_=(u) }
  override def gra_=(u: inVec3b) { yxw_=(u) }
  override def gbr_=(u: inVec3b) { yzx_=(u) }
  override def gba_=(u: inVec3b) { yzw_=(u) }
  override def gar_=(u: inVec3b) { ywx_=(u) }
  override def gab_=(u: inVec3b) { ywz_=(u) }
  override def brg_=(u: inVec3b) { zxy_=(u) }
  override def bra_=(u: inVec3b) { zxw_=(u) }
  override def bgr_=(u: inVec3b) { zyx_=(u) }
  override def bga_=(u: inVec3b) { zyw_=(u) }
  override def bar_=(u: inVec3b) { zwx_=(u) }
  override def bag_=(u: inVec3b) { zwy_=(u) }
  override def arg_=(u: inVec3b) { wxy_=(u) }
  override def arb_=(u: inVec3b) { wxz_=(u) }
  override def agr_=(u: inVec3b) { wyx_=(u) }
  override def agb_=(u: inVec3b) { wyz_=(u) }
  override def abr_=(u: inVec3b) { wzx_=(u) }
  override def abg_=(u: inVec3b) { wzy_=(u) }

  override def rgba_=(u: inVec4b) { xyzw_=(u) }
  override def rgab_=(u: inVec4b) { xywz_=(u) }
  override def rbga_=(u: inVec4b) { xzyw_=(u) }
  override def rbag_=(u: inVec4b) { xzwy_=(u) }
  override def ragb_=(u: inVec4b) { xwyz_=(u) }
  override def rabg_=(u: inVec4b) { xwzy_=(u) }
  override def grba_=(u: inVec4b) { yxzw_=(u) }
  override def grab_=(u: inVec4b) { yxwz_=(u) }
  override def gbra_=(u: inVec4b) { yzxw_=(u) }
  override def gbar_=(u: inVec4b) { yzwx_=(u) }
  override def garb_=(u: inVec4b) { ywxz_=(u) }
  override def gabr_=(u: inVec4b) { ywzx_=(u) }
  override def brga_=(u: inVec4b) { zxyw_=(u) }
  override def brag_=(u: inVec4b) { zxwy_=(u) }
  override def bgra_=(u: inVec4b) { zyxw_=(u) }
  override def bgar_=(u: inVec4b) { zywx_=(u) }
  override def barg_=(u: inVec4b) { zwxy_=(u) }
  override def bagr_=(u: inVec4b) { zwyx_=(u) }
  override def argb_=(u: inVec4b) { wxyz_=(u) }
  override def arbg_=(u: inVec4b) { wxzy_=(u) }
  override def agrb_=(u: inVec4b) { wyxz_=(u) }
  override def agbr_=(u: inVec4b) { wyzx_=(u) }
  override def abrg_=(u: inVec4b) { wzxy_=(u) }
  override def abgr_=(u: inVec4b) { wzyx_=(u) }

  override def st_=(u: inVec2b) { xy_=(u) }
  override def sp_=(u: inVec2b) { xz_=(u) }
  override def sq_=(u: inVec2b) { xw_=(u) }
  override def ts_=(u: inVec2b) { yx_=(u) }
  override def tp_=(u: inVec2b) { yz_=(u) }
  override def tq_=(u: inVec2b) { yw_=(u) }
  override def ps_=(u: inVec2b) { zx_=(u) }
  override def pt_=(u: inVec2b) { zy_=(u) }
  override def pq_=(u: inVec2b) { zw_=(u) }
  override def qs_=(u: inVec2b) { wx_=(u) }
  override def qt_=(u: inVec2b) { wy_=(u) }
  override def qp_=(u: inVec2b) { wz_=(u) }

  override def stp_=(u: inVec3b) { xyz_=(u) }
  override def stq_=(u: inVec3b) { xyw_=(u) }
  override def spt_=(u: inVec3b) { xzy_=(u) }
  override def spq_=(u: inVec3b) { xzw_=(u) }
  override def sqt_=(u: inVec3b) { xwy_=(u) }
  override def sqp_=(u: inVec3b) { xwz_=(u) }
  override def tsp_=(u: inVec3b) { yxz_=(u) }
  override def tsq_=(u: inVec3b) { yxw_=(u) }
  override def tps_=(u: inVec3b) { yzx_=(u) }
  override def tpq_=(u: inVec3b) { yzw_=(u) }
  override def tqs_=(u: inVec3b) { ywx_=(u) }
  override def tqp_=(u: inVec3b) { ywz_=(u) }
  override def pst_=(u: inVec3b) { zxy_=(u) }
  override def psq_=(u: inVec3b) { zxw_=(u) }
  override def pts_=(u: inVec3b) { zyx_=(u) }
  override def ptq_=(u: inVec3b) { zyw_=(u) }
  override def pqs_=(u: inVec3b) { zwx_=(u) }
  override def pqt_=(u: inVec3b) { zwy_=(u) }
  override def qst_=(u: inVec3b) { wxy_=(u) }
  override def qsp_=(u: inVec3b) { wxz_=(u) }
  override def qts_=(u: inVec3b) { wyx_=(u) }
  override def qtp_=(u: inVec3b) { wyz_=(u) }
  override def qps_=(u: inVec3b) { wzx_=(u) }
  override def qpt_=(u: inVec3b) { wzy_=(u) }

  override def stpq_=(u: inVec4b) { xyzw_=(u) }
  override def stqp_=(u: inVec4b) { xywz_=(u) }
  override def sptq_=(u: inVec4b) { xzyw_=(u) }
  override def spqt_=(u: inVec4b) { xzwy_=(u) }
  override def sqtp_=(u: inVec4b) { xwyz_=(u) }
  override def sqpt_=(u: inVec4b) { xwzy_=(u) }
  override def tspq_=(u: inVec4b) { yxzw_=(u) }
  override def tsqp_=(u: inVec4b) { yxwz_=(u) }
  override def tpsq_=(u: inVec4b) { yzxw_=(u) }
  override def tpqs_=(u: inVec4b) { yzwx_=(u) }
  override def tqsp_=(u: inVec4b) { ywxz_=(u) }
  override def tqps_=(u: inVec4b) { ywzx_=(u) }
  override def pstq_=(u: inVec4b) { zxyw_=(u) }
  override def psqt_=(u: inVec4b) { zxwy_=(u) }
  override def ptsq_=(u: inVec4b) { zyxw_=(u) }
  override def ptqs_=(u: inVec4b) { zywx_=(u) }
  override def pqst_=(u: inVec4b) { zwxy_=(u) }
  override def pqts_=(u: inVec4b) { zwyx_=(u) }
  override def qstp_=(u: inVec4b) { wxyz_=(u) }
  override def qspt_=(u: inVec4b) { wxzy_=(u) }
  override def qtsp_=(u: inVec4b) { wyxz_=(u) }
  override def qtps_=(u: inVec4b) { wyzx_=(u) }
  override def qpst_=(u: inVec4b) { wzxy_=(u) }
  override def qpts_=(u: inVec4b) { wzyx_=(u) }
  // @SwizzlingEnd
}


object Vec4b {
  final val True = new ConstVec4b(true, true, true, true)
  final val False = new ConstVec4b(false, false, false, false)

  final val Manifest = classType[Vec4b](classOf[Vec4b])
  final val ConstManifest = classType[ConstVec4b](classOf[ConstVec4b])
  final val ReadManifest = classType[ReadVec4b](classOf[ReadVec4b])


  def apply(s: Boolean) = new Vec4b(s, s, s, s)
  def apply(x: Boolean, y: Boolean, z: Boolean, w: Boolean) = new Vec4b(x, y, z, w)

  def apply(u: AnyVec4[_]) = new Vec4b(u.bx, u.by, u.bz, u.bw)
  def apply(xy: AnyVec2[_], z: Boolean, w: Boolean) = new Vec4b(xy.bx, xy.by, z, w)
  def apply(x: Boolean, yz: AnyVec2[_], w: Boolean) = new Vec4b(x, yz.bx, yz.by, w)
  def apply(x: Boolean, y: Boolean, zw: AnyVec2[_]) = new Vec4b(x, y, zw.bx, zw.by)
  def apply(xy: AnyVec2[_], zw: AnyVec2[_]) = new Vec4b(xy.bx, xy.by, zw.bx, zw.by)
  def apply(xyz: AnyVec3[_], w: Boolean) = new Vec4b(xyz.bx, xyz.by, xyz.bz, w)
  def apply(x: Boolean, yzw: AnyVec3[_]) = new Vec4b(x, yzw.bx, yzw.by, yzw.bz)

  def apply(m: AnyMat2[_]) = new Vec4b(toBool(m.d00), toBool(m.d01), toBool(m.d10), toBool(m.d11))
  def apply(q: AnyQuat4[_]) = new Vec4b(toBool(q.db), toBool(q.dc), toBool(q.dd), toBool(q.da))

  def unapply(u: ReadVec4b) = Some((u.x, u.y, u.z, u.w))
}
