/*
 * Simplex3d, CoreMath module
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
import simplex3d.math.CommonMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
@serializable @SerialVersionUID(8104346712419693669L)
sealed abstract class ReadVec3b extends ProtectedVec3b[Boolean] {

  type Clone <: ReadVec3b

  private[math] type R2 = ReadVec2b
  private[math] type R3 = ReadVec3b
  private[math] type R4 = ReadVec4b

  private[math] type C2 = ConstVec2b
  private[math] type C3 = ConstVec3b
  private[math] type C4 = ConstVec4b

  protected final def make2(x: Double, y: Double) =
    new ConstVec2b(Boolean(x), Boolean(y))
  protected final def make3(x: Double, y: Double, z: Double) =
    new ConstVec3b(Boolean(x), Boolean(y), Boolean(z))
  protected final def make4(x: Double, y: Double, z: Double, w: Double) =
    new ConstVec4b(Boolean(x), Boolean(y), Boolean(z), Boolean(w))

  private[math] final def bx: Boolean = x
  private[math] final def by: Boolean = y
  private[math] final def bz: Boolean = z

  private[math] final def ix: Int = Int(x)
  private[math] final def iy: Int = Int(y)
  private[math] final def iz: Int = Int(z)

  private[math] final def fx: Float = Float(x)
  private[math] final def fy: Float = Float(y)
  private[math] final def fz: Float = Float(z)

  private[math] final def dx: Double = Double(x)
  private[math] final def dy: Double = Double(y)
  private[math] final def dz: Double = Double(z)


  final def x = px
  final def y = py
  final def z = pz

  final def r = px
  final def g = py
  final def b = pz

  final def s = px
  final def t = py
  final def p = pz


  protected def x_=(s: Boolean) { throw new UnsupportedOperationException }
  protected def y_=(s: Boolean) { throw new UnsupportedOperationException }
  protected def z_=(s: Boolean) { throw new UnsupportedOperationException }

  protected def r_=(s: Boolean) { throw new UnsupportedOperationException }
  protected def g_=(s: Boolean) { throw new UnsupportedOperationException }
  protected def b_=(s: Boolean) { throw new UnsupportedOperationException }

  protected def s_=(s: Boolean) { throw new UnsupportedOperationException }
  protected def t_=(s: Boolean) { throw new UnsupportedOperationException }
  protected def p_=(s: Boolean) { throw new UnsupportedOperationException }

  final def apply(i: Int) :Boolean = {
    i match {
      case 0 => x
      case 1 => y
      case 2 => z
      case j => throw new IndexOutOfBoundsException(
          "Expected from 0 to 2, got " + j + "."
        )
    }
  }



  final override def equals(other: Any) :Boolean = {
    other match {
      case u: ReadVec3b => x == u.x && y == u.y && z == u.z
      case _ => false
    }
  }

  final override def hashCode() :Int = {
    41 * (
      41 * (
        41 + z.hashCode
      ) + y.hashCode
    ) + x.hashCode
  }

  final override def toString() :String = {
    val prefix = this match {
      case self: Immutable => "Const"
      case _ => ""
    }
    prefix + "Vec3b" + "(" + x + ", " + y + ", " + z + ")"
  }
}


@serializable @SerialVersionUID(8104346712419693669L)
final class ConstVec3b private[math] (cx: Boolean, cy: Boolean, cz: Boolean)
extends ReadVec3b with Immutable {
  px = cx; py = cy; pz = cz

  type Clone = ConstVec3b
  override def clone() = this
}


object ConstVec3b {

  def apply(s: Boolean) = new ConstVec3b(s, s, s)
  def apply(x: Boolean, y: Boolean, z: Boolean) = new ConstVec3b(x, y, z)

  def apply(u: AnyVec3[_]) = new ConstVec3b(u.bx, u.by, u.bz)
  def apply(u: AnyVec4[_]) = new ConstVec3b(u.bx, u.by, u.bz)

  def apply(xy: AnyVec2[_], z: Boolean) = new ConstVec3b(xy.bx, xy.by, z)
  def apply(x: Boolean, yz: AnyVec2[_]) = new ConstVec3b(x, yz.bx, yz.by)

  implicit def toConst(u: ReadVec3b) = apply(u)
}


@serializable @SerialVersionUID(8104346712419693669L)
final class Vec3b private[math] (cx: Boolean, cy: Boolean, cz: Boolean)
extends ReadVec3b with PropertyRef with Composite with Implicits[On]
{
  type Read = ReadVec3b
  type Const = ConstVec3b
  type Component = Bool

  px = cx; py = cy; pz = cz

  @noinline override def x_=(s: Boolean) { px = s }
  @noinline override def y_=(s: Boolean) { py = s }
  @noinline override def z_=(s: Boolean) { pz = s }

  override def r_=(s: Boolean) { px = s }
  override def g_=(s: Boolean) { py = s }
  override def b_=(s: Boolean) { pz = s }

  override def s_=(s: Boolean) { px = s }
  override def t_=(s: Boolean) { py = s }
  override def p_=(s: Boolean) { pz = s }

  def update(i: Int, s: Boolean) {
    i match {
      case 0 => x = s
      case 1 => y = s
      case 2 => z = s
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 2, got " + j
        )
    }
  }



  type Clone = Vec3b
  override def clone() = Vec3b(this)
  def toConst() = ConstVec3b(this)
  def :=(u: inVec3b) { x = u.x; y = u.y; z = u.z }

  // Swizzling
  override def xy_=(u: inVec2b) { x = u.x; y = u.y }
  override def xz_=(u: inVec2b) { x = u.x; z = u.y }
  override def yx_=(u: inVec2b) { y = u.x; x = u.y }
  override def yz_=(u: inVec2b) { y = u.x; z = u.y }
  override def zx_=(u: inVec2b) { z = u.x; x = u.y }
  override def zy_=(u: inVec2b) { z = u.x; y = u.y }

  override def xyz_=(u: inVec3b) { x = u.x; y = u.y; z = u.z }
  override def xzy_=(u: inVec3b) { x = u.x; var t = u.z; z = u.y; y = t }
  override def yxz_=(u: inVec3b) { var t = u.y; y = u.x; x = t; z = u.z }
  override def yzx_=(u: inVec3b) { var t = u.y; y = u.x; x = u.z; z = t }
  override def zxy_=(u: inVec3b) { var t = u.z; z = u.x; x = u.y; y = t }
  override def zyx_=(u: inVec3b) { var t = u.z; z = u.x; x = t; y = u.y }

  override def rg_=(u: inVec2b) { xy_=(u) }
  override def rb_=(u: inVec2b) { xz_=(u) }
  override def gr_=(u: inVec2b) { yx_=(u) }
  override def gb_=(u: inVec2b) { yz_=(u) }
  override def br_=(u: inVec2b) { zx_=(u) }
  override def bg_=(u: inVec2b) { zy_=(u) }

  override def rgb_=(u: inVec3b) { xyz_=(u) }
  override def rbg_=(u: inVec3b) { xzy_=(u) }
  override def grb_=(u: inVec3b) { yxz_=(u) }
  override def gbr_=(u: inVec3b) { yzx_=(u) }
  override def brg_=(u: inVec3b) { zxy_=(u) }
  override def bgr_=(u: inVec3b) { zyx_=(u) }

  override def st_=(u: inVec2b) { xy_=(u) }
  override def sp_=(u: inVec2b) { xz_=(u) }
  override def ts_=(u: inVec2b) { yx_=(u) }
  override def tp_=(u: inVec2b) { yz_=(u) }
  override def ps_=(u: inVec2b) { zx_=(u) }
  override def pt_=(u: inVec2b) { zy_=(u) }

  override def stp_=(u: inVec3b) { xyz_=(u) }
  override def spt_=(u: inVec3b) { xzy_=(u) }
  override def tsp_=(u: inVec3b) { yxz_=(u) }
  override def tps_=(u: inVec3b) { yzx_=(u) }
  override def pst_=(u: inVec3b) { zxy_=(u) }
  override def pts_=(u: inVec3b) { zyx_=(u) }
}


object Vec3b {
  final val True = new ConstVec3b(true, true, true)
  final val False = new ConstVec3b(false, false, false)

  final val Manifest = classType[Vec3b](classOf[Vec3b])
  final val ConstManifest = classType[ConstVec3b](classOf[ConstVec3b])
  final val ReadManifest = classType[ReadVec3b](classOf[ReadVec3b])


  def apply(s: Boolean) = new Vec3b(s, s, s)
  def apply(x: Boolean, y: Boolean, z: Boolean) = new Vec3b(x, y, z)

  def apply(u: AnyVec3[_]) = new Vec3b(u.bx, u.by, u.bz)
  def apply(u: AnyVec4[_]) = new Vec3b(u.bx, u.by, u.bz)

  def apply(xy: AnyVec2[_], z: Boolean) = new Vec3b(xy.bx, xy.by, z)
  def apply(x: Boolean, yz: AnyVec2[_]) = new Vec3b(x, yz.bx, yz.by)

  def unapply(u: ReadVec3b) = Some((u.x, u.y, u.z))
  implicit def toMutable(u: ReadVec3b) = apply(u)
}
