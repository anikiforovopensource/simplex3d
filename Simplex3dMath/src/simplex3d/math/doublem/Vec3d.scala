/*
 * Simplex3d, DoubleMath module
 * Copyright (C) 2009-2010, Simplex3d Team
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

package simplex3d.math.doublem

import simplex3d.math.types._
import simplex3d.math._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyVec3d extends Read3[Double] {

  private[math] type R2 = ConstVec2d
  private[math] type R3 = ConstVec3d
  private[math] type R4 = ConstVec4d

  protected final def make2(x: Double, y: Double) =
    new ConstVec2d(x, y)
  protected final def make3(x: Double, y: Double, z: Double) =
    new ConstVec3d(x, y, z)
  protected final def make4(x: Double, y: Double, z: Double, w: Double) =
    new ConstVec4d(x, y, z, w)

  private[math] final def bx: Boolean = bool(x)
  private[math] final def by: Boolean = bool(y)
  private[math] final def bz: Boolean = bool(z)

  private[math] final def ix: Int = int(x)
  private[math] final def iy: Int = int(y)
  private[math] final def iz: Int = int(z)

  private[math] final def fx: Float = float(x)
  private[math] final def fy: Float = float(y)
  private[math] final def fz: Float = float(z)

  private[math] final def dx: Double = x
  private[math] final def dy: Double = y
  private[math] final def dz: Double = z


  def x: Double
  def y: Double
  def z: Double

  def r = x
  def g = y
  def b = z

  def s = x
  def t = y
  def p = z

  
  final def apply(i: Int) :Double = {
    i match {
      case 0 => x
      case 1 => y
      case 2 => z
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 2, got " + j
        )
    }
  }

  final def unary_+() :AnyVec3d = this
  final def unary_-() = new Vec3d(-x, -y, -z)
  final def *(s: Double) = new Vec3d(x * s, y * s, z * s)
  final def /(s: Double) = { val inv = 1/s; new Vec3d(x * inv, y * inv, z * inv) }

  final def +(s: Double) = new Vec3d(x + s, y + s, z + s)
  final def -(s: Double) = new Vec3d(x - s, y - s, z - s)

  private[math] final def divideByComponent(s: Double) = {
    new Vec3d(s / x, s / y, s / z)
  }

  final def +(u: inVec3d) = new Vec3d(x + u.x, y + u.y, z + u.z)
  final def -(u: inVec3d) = new Vec3d(x - u.x, y - u.y, z - u.z)
  final def *(u: inVec3d) = new Vec3d(x * u.x, y * u.y, z * u.z)
  final def /(u: inVec3d) = new Vec3d(x / u.x, y / u.y, z / u.z)

  final def *(m: inMat3x2d) :Vec2d = m.transposeMul(this)
  final def *(m: inMat3d) :Vec3d = m.transposeMul(this)
  final def *(m: inMat3x4d) :Vec4d = m.transposeMul(this)

  private[math] final def hasErrors: Boolean = {
    import java.lang.Double._
    (
      isNaN(x) || isInfinite(x) ||
      isNaN(y) || isInfinite(y) ||
      isNaN(z) || isInfinite(z)
    )
  }

  final override def equals(other: Any) :Boolean = {
    other match {
      case u: AnyVec3b => false
      case u: Read3[_] => dx == u.dx && dy == u.dy && dz == u.dz
      case _ => false
    }
  }

  final override def hashCode() :Int = {
    41 * (
      41 * (
        41 + x.hashCode
      ) + y.hashCode
    ) + z.hashCode
  }

  final override def toString() :String = {
    this.getClass.getSimpleName + "(" + x + ", " + y + ", " + z + ")"
  }
}


@serializable @SerialVersionUID(5359695191257934190L)
final class ConstVec3d private[math] (
  val x: Double, val y: Double, val z: Double
) extends AnyVec3d with Immutable

object ConstVec3d {
  def apply(s: Double) = new ConstVec3d(s, s, s)
  /* main factory */ def apply(x: Double, y: Double, z: Double) = new ConstVec3d(x, y, z)
  def apply(u: Read3[_]) = new ConstVec3d(u.dx, u.dy, u.dz)
  def apply(u: Read4[_]) = new ConstVec3d(u.dx, u.dy, u.dz)
  def apply(xy: Read2[_], z: Double) = new ConstVec3d(xy.dx, xy.dy, z)
  def apply(x: Double, yz: Read2[_]) = new ConstVec3d(x, yz.dx, yz.dy)

  implicit def toConst(u: AnyVec3d) = new ConstVec3d(u.x, u.y, u.z)
}


@serializable @SerialVersionUID(5359695191257934190L)
final class Vec3d private[math] (
  var x: Double, var y: Double, var z: Double
) extends AnyVec3d with Mutable with Implicits[On] with Composite
{
  type Element = AnyVec3d
  type Component = Double1

  override def r = x
  override def g = y
  override def b = z

  override def s = x
  override def t = y
  override def p = z

  def r_=(r: Double) { x = r }
  def g_=(g: Double) { y = g }
  def b_=(b: Double) { z = b }

  def s_=(s: Double) { x = s }
  def t_=(t: Double) { y = t }
  def p_=(p: Double) { z = p }


  def *=(s: Double) { x *= s; y *= s; z *= s }
  def /=(s: Double) { val inv = 1/s; x *= inv; y *= inv; z *= inv }

  def +=(s: Double) { x += s; y += s; z += s }
  def -=(s: Double) { x -= s; y -= s; z -= s }

  def +=(u: inVec3d) { x += u.x; y += u.y; z += u.z }
  def -=(u: inVec3d) { x -= u.x; y -= u.y; z -= u.z }
  def *=(u: inVec3d) { x *= u.x; y *= u.y; z *= u.z }
  def /=(u: inVec3d) { x /= u.x; y /= u.y; z /= u.z }

  def *=(m: inMat3d) { this := m.transposeMul(this) }

  def :=(u: inVec3d) { x = u.x; y = u.y; z = u.z }
  def set(x: Double, y: Double, z: Double) { this.x = x; this.y = y; this.z = z }

  def update(i: Int, s: Double) {
    i match {
      case 0 => x = s
      case 1 => y = s
      case 2 => z = s
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 2, got " + j
        )
    }
  }

  // Swizzling
  override def xy: ConstVec2d = new ConstVec2d(x, y)
  override def xz: ConstVec2d = new ConstVec2d(x, z)
  override def yx: ConstVec2d = new ConstVec2d(y, x)
  override def yz: ConstVec2d = new ConstVec2d(y, z)
  override def zx: ConstVec2d = new ConstVec2d(z, x)
  override def zy: ConstVec2d = new ConstVec2d(z, y)

  override def xyz: ConstVec3d = new ConstVec3d(x, y, z)
  override def xzy: ConstVec3d = new ConstVec3d(x, z, y)
  override def yxz: ConstVec3d = new ConstVec3d(y, x, z)
  override def yzx: ConstVec3d = new ConstVec3d(y, z, x)
  override def zxy: ConstVec3d = new ConstVec3d(z, x, y)
  override def zyx: ConstVec3d = new ConstVec3d(z, y, x)

  override def rg = xy
  override def rb = xz
  override def gr = yx
  override def gb = yz
  override def br = zx
  override def bg = zy

  override def rgb = xyz
  override def rbg = xzy
  override def grb = yxz
  override def gbr = yzx
  override def brg = zxy
  override def bgr = zyx

  override def st = xy
  override def sp = xz
  override def ts = yx
  override def tp = yz
  override def ps = zx
  override def pt = zy

  override def stp = xyz
  override def spt = xzy
  override def tsp = yxz
  override def tps = yzx
  override def pst = zxy
  override def pts = zyx


  def xy_=(u: inVec2d) { x = u.x; y = u.y }
  def xz_=(u: inVec2d) { x = u.x; z = u.y }
  def yx_=(u: inVec2d) { y = u.x; x = u.y }
  def yz_=(u: inVec2d) { y = u.x; z = u.y }
  def zx_=(u: inVec2d) { z = u.x; x = u.y }
  def zy_=(u: inVec2d) { z = u.x; y = u.y }

  def xyz_=(u: inVec3d) { x = u.x; y = u.y; z = u.z }
  def xzy_=(u: inVec3d) { x = u.x; var t = u.z; z = u.y; y = t }
  def yxz_=(u: inVec3d) { var t = u.y; y = u.x; x = t; z = u.z }
  def yzx_=(u: inVec3d) { var t = u.y; y = u.x; x = u.z; z = t }
  def zxy_=(u: inVec3d) { var t = u.z; z = u.x; x = u.y; y = t }
  def zyx_=(u: inVec3d) { var t = u.z; z = u.x; x = t; y = u.y }

  def rg_=(u: inVec2d) { xy_=(u) }
  def rb_=(u: inVec2d) { xz_=(u) }
  def gr_=(u: inVec2d) { yx_=(u) }
  def gb_=(u: inVec2d) { yz_=(u) }
  def br_=(u: inVec2d) { zx_=(u) }
  def bg_=(u: inVec2d) { zy_=(u) }

  def rgb_=(u: inVec3d) { xyz_=(u) }
  def rbg_=(u: inVec3d) { xzy_=(u) }
  def grb_=(u: inVec3d) { yxz_=(u) }
  def gbr_=(u: inVec3d) { yzx_=(u) }
  def brg_=(u: inVec3d) { zxy_=(u) }
  def bgr_=(u: inVec3d) { zyx_=(u) }

  def st_=(u: inVec2d) { xy_=(u) }
  def sp_=(u: inVec2d) { xz_=(u) }
  def ts_=(u: inVec2d) { yx_=(u) }
  def tp_=(u: inVec2d) { yz_=(u) }
  def ps_=(u: inVec2d) { zx_=(u) }
  def pt_=(u: inVec2d) { zy_=(u) }

  def stp_=(u: inVec3d) { xyz_=(u) }
  def spt_=(u: inVec3d) { xzy_=(u) }
  def tsp_=(u: inVec3d) { yxz_=(u) }
  def tps_=(u: inVec3d) { yzx_=(u) }
  def pst_=(u: inVec3d) { zxy_=(u) }
  def pts_=(u: inVec3d) { zyx_=(u) }
}

object Vec3d {
  val Zero = new ConstVec3d(0, 0, 0)
  val UnitX = new ConstVec3d(1, 0, 0)
  val UnitY = new ConstVec3d(0, 1, 0)
  val UnitZ = new ConstVec3d(0, 0, 1)
  val One = new ConstVec3d(1, 1, 1)

  def apply(s: Double) = new Vec3d(s, s, s)
  /* main factory */ def apply(x: Double, y: Double, z: Double) = new Vec3d(x, y, z)
  def apply(u: Read3[_]) = new Vec3d(u.dx, u.dy, u.dz)
  def apply(u: Read4[_]) = new Vec3d(u.dx, u.dy, u.dz)
  def apply(xy: Read2[_], z: Double) = new Vec3d(xy.dx, xy.dy, z)
  def apply(x: Double, yz: Read2[_]) = new Vec3d(x, yz.dx, yz.dy)

  def unapply(u: AnyVec3d) = Some((u.x, u.y, u.z))

  implicit def toMutable(u: AnyVec3d) = new Vec3d(u.x, u.y, u.z)
  implicit def castInt(u: Read3[Int]) = new Vec3d(u.dx, u.dy, u.dz)
  implicit def castFloat(u: Read3[Float]) = new Vec3d(u.dx, u.dy, u.dz)
}
