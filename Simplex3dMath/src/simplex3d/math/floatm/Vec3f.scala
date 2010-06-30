/*
 * Simplex3d, FloatMath module
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

package simplex3d.math.floatm

import scala.reflect.Manifest._
import simplex3d.math.types._
import simplex3d.math._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyVec3f extends Read3[Float] {

  private[math] type R2 = ConstVec2f
  private[math] type R3 = ConstVec3f
  private[math] type R4 = ConstVec4f

  protected final def make2(x: Double, y: Double) =
    new ConstVec2f(float(x), float(y))
  protected final def make3(x: Double, y: Double, z: Double) =
    new ConstVec3f(float(x), float(y), float(z))
  protected final def make4(x: Double, y: Double, z: Double, w: Double) =
    new ConstVec4f(float(x), float(y), float(z), float(w))

  private[math] final def bx: Boolean = bool(x)
  private[math] final def by: Boolean = bool(y)
  private[math] final def bz: Boolean = bool(z)

  private[math] final def ix: Int = int(x)
  private[math] final def iy: Int = int(y)
  private[math] final def iz: Int = int(z)

  private[math] final def fx: Float = x
  private[math] final def fy: Float = y
  private[math] final def fz: Float = z

  private[math] final def dx: Double = x
  private[math] final def dy: Double = y
  private[math] final def dz: Double = z


  def x: Float
  def y: Float
  def z: Float

  def r = x
  def g = y
  def b = z

  def s = x
  def t = y
  def p = z

  
  final def apply(i: Int) :Float = {
    i match {
      case 0 => x
      case 1 => y
      case 2 => z
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 2, got " + j
        )
    }
  }

  final def unary_+() :AnyVec3f = this
  final def unary_-() = new Vec3f(-x, -y, -z)
  final def *(s: Float) = new Vec3f(x * s, y * s, z * s)
  final def /(s: Float) = { val inv = 1/s; new Vec3f(x * inv, y * inv, z * inv) }

  final def +(s: Float) = new Vec3f(x + s, y + s, z + s)
  final def -(s: Float) = new Vec3f(x - s, y - s, z - s)

  private[math] final def divideByComponent(s: Float) = {
    new Vec3f(s / x, s / y, s / z)
  }

  final def +(u: inVec3f) = new Vec3f(x + u.x, y + u.y, z + u.z)
  final def -(u: inVec3f) = new Vec3f(x - u.x, y - u.y, z - u.z)
  final def *(u: inVec3f) = new Vec3f(x * u.x, y * u.y, z * u.z)
  final def /(u: inVec3f) = new Vec3f(x / u.x, y / u.y, z / u.z)

  final def *(m: inMat3x2f) :Vec2f = m.transposeMul(this)
  final def *(m: inMat3f) :Vec3f = m.transposeMul(this)
  final def *(m: inMat3x4f) :Vec4f = m.transposeMul(this)

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
final class ConstVec3f private[math] (val x: Float, val y: Float, val z: Float)
extends AnyVec3f with Immutable

object ConstVec3f {
  def apply(s: Float) = new ConstVec3f(s, s, s)
  /* main factory */ def apply(x: Float, y: Float, z: Float) = new ConstVec3f(x, y, z)
  def apply(u: Read3[_]) = new ConstVec3f(u.fx, u.fy, u.fz)
  def apply(u: Read4[_]) = new ConstVec3f(u.fx, u.fy, u.fz)
  def apply(xy: Read2[_], z: Float) = new ConstVec3f(xy.fx, xy.fy, z)
  def apply(x: Float, yz: Read2[_]) = new ConstVec3f(x, yz.fx, yz.fy)

  implicit def toConst(u: AnyVec3f) = new ConstVec3f(u.x, u.y, u.z)
}


@serializable @SerialVersionUID(5359695191257934190L)
final class Vec3f private[math] (var x: Float, var y: Float, var z: Float)
extends AnyVec3f with Mutable with Implicits[On] with Composite
{
  type Element = AnyVec3f
  type Component = Float1

  override def r = x
  override def g = y
  override def b = z

  override def s = x
  override def t = y
  override def p = z

  def r_=(r: Float) { x = r }
  def g_=(g: Float) { y = g }
  def b_=(b: Float) { z = b }

  def s_=(s: Float) { x = s }
  def t_=(t: Float) { y = t }
  def p_=(p: Float) { z = p }


  def *=(s: Float) { x *= s; y *= s; z *= s }
  def /=(s: Float) { val inv = 1/s; x *= inv; y *= inv; z *= inv }

  def +=(s: Float) { x += s; y += s; z += s }
  def -=(s: Float) { x -= s; y -= s; z -= s }

  def +=(u: inVec3f) { x += u.x; y += u.y; z += u.z }
  def -=(u: inVec3f) { x -= u.x; y -= u.y; z -= u.z }
  def *=(u: inVec3f) { x *= u.x; y *= u.y; z *= u.z }
  def /=(u: inVec3f) { x /= u.x; y /= u.y; z /= u.z }

  def *=(m: inMat3f) { this := m.transposeMul(this) }

  def :=(u: inVec3f) { x = u.x; y = u.y; z = u.z }
  def set(x: Float, y: Float, z: Float) { this.x = x; this.y = y; this.z = z }

  def update(i: Int, s: Float) {
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
  override def xy: ConstVec2f = new ConstVec2f(x, y)
  override def xz: ConstVec2f = new ConstVec2f(x, z)
  override def yx: ConstVec2f = new ConstVec2f(y, x)
  override def yz: ConstVec2f = new ConstVec2f(y, z)
  override def zx: ConstVec2f = new ConstVec2f(z, x)
  override def zy: ConstVec2f = new ConstVec2f(z, y)

  override def xyz: ConstVec3f = new ConstVec3f(x, y, z)
  override def xzy: ConstVec3f = new ConstVec3f(x, z, y)
  override def yxz: ConstVec3f = new ConstVec3f(y, x, z)
  override def yzx: ConstVec3f = new ConstVec3f(y, z, x)
  override def zxy: ConstVec3f = new ConstVec3f(z, x, y)
  override def zyx: ConstVec3f = new ConstVec3f(z, y, x)

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


  def xy_=(u: inVec2f) { x = u.x; y = u.y }
  def xz_=(u: inVec2f) { x = u.x; z = u.y }
  def yx_=(u: inVec2f) { y = u.x; x = u.y }
  def yz_=(u: inVec2f) { y = u.x; z = u.y }
  def zx_=(u: inVec2f) { z = u.x; x = u.y }
  def zy_=(u: inVec2f) { z = u.x; y = u.y }

  def xyz_=(u: inVec3f) { x = u.x; y = u.y; z = u.z }
  def xzy_=(u: inVec3f) { x = u.x; var t = u.z; z = u.y; y = t }
  def yxz_=(u: inVec3f) { var t = u.y; y = u.x; x = t; z = u.z }
  def yzx_=(u: inVec3f) { var t = u.y; y = u.x; x = u.z; z = t }
  def zxy_=(u: inVec3f) { var t = u.z; z = u.x; x = u.y; y = t }
  def zyx_=(u: inVec3f) { var t = u.z; z = u.x; x = t; y = u.y }

  def rg_=(u: inVec2f) { xy_=(u) }
  def rb_=(u: inVec2f) { xz_=(u) }
  def gr_=(u: inVec2f) { yx_=(u) }
  def gb_=(u: inVec2f) { yz_=(u) }
  def br_=(u: inVec2f) { zx_=(u) }
  def bg_=(u: inVec2f) { zy_=(u) }

  def rgb_=(u: inVec3f) { xyz_=(u) }
  def rbg_=(u: inVec3f) { xzy_=(u) }
  def grb_=(u: inVec3f) { yxz_=(u) }
  def gbr_=(u: inVec3f) { yzx_=(u) }
  def brg_=(u: inVec3f) { zxy_=(u) }
  def bgr_=(u: inVec3f) { zyx_=(u) }

  def st_=(u: inVec2f) { xy_=(u) }
  def sp_=(u: inVec2f) { xz_=(u) }
  def ts_=(u: inVec2f) { yx_=(u) }
  def tp_=(u: inVec2f) { yz_=(u) }
  def ps_=(u: inVec2f) { zx_=(u) }
  def pt_=(u: inVec2f) { zy_=(u) }

  def stp_=(u: inVec3f) { xyz_=(u) }
  def spt_=(u: inVec3f) { xzy_=(u) }
  def tsp_=(u: inVec3f) { yxz_=(u) }
  def tps_=(u: inVec3f) { yzx_=(u) }
  def pst_=(u: inVec3f) { zxy_=(u) }
  def pts_=(u: inVec3f) { zyx_=(u) } 
}

object Vec3f {
  final val Zero = new ConstVec3f(0, 0, 0)
  final val UnitX = new ConstVec3f(1, 0, 0)
  final val UnitY = new ConstVec3f(0, 1, 0)
  final val UnitZ = new ConstVec3f(0, 0, 1)
  final val One = new ConstVec3f(1, 1, 1)
  final val Manifest = classType[AnyVec3f](classOf[AnyVec3f])

  def apply(s: Float) = new Vec3f(s, s, s)
  /* main factory */ def apply(x: Float, y: Float, z: Float) = new Vec3f(x, y, z)
  def apply(u: Read3[_]) = new Vec3f(u.fx, u.fy, u.fz)
  def apply(u: Read4[_]) = new Vec3f(u.fx, u.fy, u.fz)
  def apply(xy: Read2[_], z: Float) = new Vec3f(xy.fx, xy.fy, z)
  def apply(x: Float, yz: Read2[_]) = new Vec3f(x, yz.fx, yz.fy)

  def unapply(u: AnyVec3f) = Some((u.x, u.y, u.z))

  implicit def toMutable(u: AnyVec3f) = new Vec3f(u.x, u.y, u.z)
  implicit def castInt(u: Read3[Int]) = new Vec3f(u.fx, u.fy, u.fz)
}
