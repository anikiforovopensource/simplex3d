/*
 * Simplex3d, IntMath module
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

package simplex3d.math.intm

import simplex3d.math._
import simplex3d.math.BaseMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyVec3i extends Read3[Int] {

  private[math] type R2 = ConstVec2i
  private[math] type R3 = ConstVec3i
  private[math] type R4 = ConstVec4i

  protected def make2(x: Int, y: Int) =
    new ConstVec2i(x, y)
  protected def make3(x: Int, y: Int, z: Int) =
    new ConstVec3i(x, y, z)
  protected def make4(x: Int, y: Int, z: Int, w: Int) =
    new ConstVec4i(x, y, z, w)

  private[math] def bx: Boolean = bool(x)
  private[math] def by: Boolean = bool(y)
  private[math] def bz: Boolean = bool(z)

  private[math] def ix: Int = x
  private[math] def iy: Int = y
  private[math] def iz: Int = z

  private[math] def fx: Float = x
  private[math] def fy: Float = y
  private[math] def fz: Float = z

  private[math] def dx: Double = x
  private[math] def dy: Double = y
  private[math] def dz: Double = z


  def x: Int
  def y: Int
  def z: Int

  def r = x
  def g = y
  def b = z

  def s = x
  def t = y
  def p = z


  def apply(i: Int) :Int = {
    i match {
      case 0 => x
      case 1 => y
      case 2 => z
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 2, got " + j
        )
    }
  }

  def unary_+() :this.type = this
  def unary_-() = new Vec3i(-x, -y, -z)
  def unary_~() = new Vec3i(~x, ~y, ~z)

  def *(s: Int) = new Vec3i(x * s, y * s, z * s)
  def /(s: Int) = new Vec3i(x / s, y / s, z / s)

  def +(s: Int) = new Vec3i(x + s, y + s, z + s)
  def -(s: Int) = new Vec3i(x - s, y - s, z - s)

  private[math] def divideByComponent(s: Int) = new Vec3i(s / x, s / y, s / z)
  def %(s: Int) = new Vec3i(x % s, y % s, z % s)
  private[math] def modByComponent(s: Int) = new Vec3i(s % x, s % y, s % z)
  def >>(s: Int) = new Vec3i( x >> s, y >> s, z >> s)
  def >>>(s: Int) = new Vec3i( x >>> s, y >>> s, z >>> s)
  def <<(s: Int) = new Vec3i( x << s, y << s, z << s)
  def &(s: Int) = new Vec3i( x & s, y & s, z & s)
  def |(s: Int) = new Vec3i( x | s, y | s, z | s)
  def ^(s: Int) = new Vec3i( x ^ s, y ^ s, z ^ s)

  def +(u: AnyVec3i) = new Vec3i(x + u.x, y + u.y, z + u.z)
  def -(u: AnyVec3i) = new Vec3i(x - u.x, y - u.y, z - u.z)
  def *(u: AnyVec3i) = new Vec3i(x * u.x, y * u.y, z * u.z)
  def /(u: AnyVec3i) = new Vec3i(x / u.x, y / u.y, z / u.z)
  def %(u: AnyVec3i) = new Vec3i(x % u.x, y % u.y, z % u.z)
  def >>(u: AnyVec3i) = new Vec3i( x >> u.x, y >> u.y, z >> u.z)
  def >>>(u: AnyVec3i) = new Vec3i( x >>> u.x, y >>> u.y, z >>> u.z)
  def <<(u: AnyVec3i) = new Vec3i( x << u.x, y << u.y, z << u.z)
  def &(u: AnyVec3i) = new Vec3i( x & u.x, y & u.y, z & u.z)
  def |(u: AnyVec3i) = new Vec3i( x | u.x, y | u.y, z | u.z)
  def ^(u: AnyVec3i) = new Vec3i( x ^ u.x, y ^ u.y, z ^ u.z)

  def ==(u: AnyVec3i) :Boolean = {
    if (u eq null) false
    else x == u.x && y == u.y && z == u.z
  }

  def !=(u: AnyVec3i) :Boolean = !(this == u)

  override def equals(other: Any) :Boolean = {
    other match {
      case u: AnyVec3i => this == u
      case _ => false
    }
  }

  override def hashCode() :Int = {
    41 * (
      41 * (
        41 + x.hashCode
      ) + y.hashCode
    ) + z.hashCode
  }

  override def toString() :String = {
    this.getClass.getSimpleName + "(" + x + ", " + y + ", " + z + ")"
  }
}

final class ConstVec3i private[math] (val x: Int, val y: Int, val z: Int)
extends AnyVec3i with Immutable

object ConstVec3i {
  def apply(x: Int, y: Int, z: Int) = new ConstVec3i(x, y, z)
  def apply(u: Read3[_]) = new ConstVec3i(u.ix, u.iy, u.iz)

  implicit def toConst(u: AnyVec3i) = new ConstVec3i(u.x, u.y, u.z)
}


final class Vec3i private[math] (var x: Int, var y: Int, var z: Int)
extends AnyVec3i with Mutable with Implicits[On]
{
  override def r = x
  override def g = y
  override def b = z

  override def s = x
  override def t = y
  override def p = z

  def r_=(r: Int) { x = r }
  def g_=(g: Int) { y = g }
  def b_=(b: Int) { z = b }

  def s_=(s: Int) { x = s }
  def t_=(t: Int) { y = t }
  def p_=(p: Int) { z = p }


  def *=(s: Int) { x *= s; y *= s; z *= s }
  def /=(s: Int) { x /= s; y /= s; z /= s }

  def +=(s: Int) { x += s; y += s; z += s }
  def -=(s: Int) { x -= s; y -= s; z -= s }
  
  def %=(s: Int) { x %= s; y %= s; z %= s }
  def >>=(s: Int) = { x >>= s; y >>= s; z >>= s }
  def >>>=(s: Int) = { x >>>= s; y >>>= s; z >>>= s }
  def <<=(s: Int) = { x <<= s; y <<= s; z <<= s }
  def &=(s: Int) = { x &= s; y &= s; z &= s }
  def |=(s: Int) = { x |= s; y |= s; z |= s }
  def ^=(s: Int) = { x ^= s; y ^= s; z ^= s }

  def +=(u: AnyVec3i) { x += u.x; y += u.y; z += u.z }
  def -=(u: AnyVec3i) { x -= u.x; y -= u.y; z -= u.z }
  def *=(u: AnyVec3i) { x *= u.x; y *= u.y; z *= u.z }
  def /=(u: AnyVec3i) { x /= u.x; y /= u.y; z /= u.z }
  def %=(u: AnyVec3i) { x %= u.x; y %= u.y; z %= u.z }
  def >>=(u: AnyVec3i) = { x >>= u.x; y >>= u.y; z >>= u.z }
  def >>>=(u: AnyVec3i) = { x >>>= u.x; y >>>= u.y; z >>>= u.z }
  def <<=(u: AnyVec3i) = { x <<= u.x; y <<= u.y; z <<= u.z }
  def &=(u: AnyVec3i) = { x &= u.x; y &= u.y; z &= u.z }
  def |=(u: AnyVec3i) = { x |= u.x; y |= u.y; z |= u.z }
  def ^=(u: AnyVec3i) = { x ^= u.x; y ^= u.y; z ^= u.z }

  def :=(u: AnyVec3i) { x = u.x; y = u.y; z = u.z }
  def set(x: Int, y: Int, z: Int) { this.x = x; this.y = y; this.z = z }

  def update(i: Int, s: Int) {
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
  override def xy: ConstVec2i = new ConstVec2i(x, y)
  override def xz: ConstVec2i = new ConstVec2i(x, z)
  override def yx: ConstVec2i = new ConstVec2i(y, x)
  override def yz: ConstVec2i = new ConstVec2i(y, z)
  override def zx: ConstVec2i = new ConstVec2i(z, x)
  override def zy: ConstVec2i = new ConstVec2i(z, y)

  override def xyz: ConstVec3i = new ConstVec3i(x, y, z)
  override def xzy: ConstVec3i = new ConstVec3i(x, z, y)
  override def yxz: ConstVec3i = new ConstVec3i(y, x, z)
  override def yzx: ConstVec3i = new ConstVec3i(y, z, x)
  override def zxy: ConstVec3i = new ConstVec3i(z, x, y)
  override def zyx: ConstVec3i = new ConstVec3i(z, y, x)

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


  def xy_=(u: AnyVec2i) { x = u.x; y = u.y }
  def xz_=(u: AnyVec2i) { x = u.x; z = u.y }
  def yx_=(u: AnyVec2i) { y = u.x; x = u.y }
  def yz_=(u: AnyVec2i) { y = u.x; z = u.y }
  def zx_=(u: AnyVec2i) { z = u.x; x = u.y }
  def zy_=(u: AnyVec2i) { z = u.x; y = u.y }

  def xyz_=(u: AnyVec3i) { x = u.x; y = u.y; z = u.z }
  def xzy_=(u: AnyVec3i) { x = u.x; var t = u.z; z = u.y; y = t }
  def yxz_=(u: AnyVec3i) { var t = u.y; y = u.x; x = t; z = u.z }
  def yzx_=(u: AnyVec3i) { var t = u.y; y = u.x; x = u.z; z = t }
  def zxy_=(u: AnyVec3i) { var t = u.z; z = u.x; x = u.y; y = t }
  def zyx_=(u: AnyVec3i) { var t = u.z; z = u.x; x = t; y = u.y }

  def rg_=(u: AnyVec2i) { xy_=(u) }
  def rb_=(u: AnyVec2i) { xz_=(u) }
  def gr_=(u: AnyVec2i) { yx_=(u) }
  def gb_=(u: AnyVec2i) { yz_=(u) }
  def br_=(u: AnyVec2i) { zx_=(u) }
  def bg_=(u: AnyVec2i) { zy_=(u) }

  def rgb_=(u: AnyVec3i) { xyz_=(u) }
  def rbg_=(u: AnyVec3i) { xzy_=(u) }
  def grb_=(u: AnyVec3i) { yxz_=(u) }
  def gbr_=(u: AnyVec3i) { yzx_=(u) }
  def brg_=(u: AnyVec3i) { zxy_=(u) }
  def bgr_=(u: AnyVec3i) { zyx_=(u) }

  def st_=(u: AnyVec2i) { xy_=(u) }
  def sp_=(u: AnyVec2i) { xz_=(u) }
  def ts_=(u: AnyVec2i) { yx_=(u) }
  def tp_=(u: AnyVec2i) { yz_=(u) }
  def ps_=(u: AnyVec2i) { zx_=(u) }
  def pt_=(u: AnyVec2i) { zy_=(u) }

  def stp_=(u: AnyVec3i) { xyz_=(u) }
  def spt_=(u: AnyVec3i) { xzy_=(u) }
  def tsp_=(u: AnyVec3i) { yxz_=(u) }
  def tps_=(u: AnyVec3i) { yzx_=(u) }
  def pst_=(u: AnyVec3i) { zxy_=(u) }
  def pts_=(u: AnyVec3i) { zyx_=(u) }
}

object Vec3i {
  val Zero = new ConstVec3i(0, 0, 0)
  val UnitX = new ConstVec3i(1, 0, 0)
  val UnitY = new ConstVec3i(0, 1, 0)
  val UnitZ = new ConstVec3i(0, 0, 1)
  val One = new ConstVec3i(1, 1, 1)

  def apply(s: Int) = new Vec3i(s, s, s)
  def apply(x: Int, y: Int, z: Int) = new Vec3i(x, y, z)
  def apply(u: Read3[_]) = new Vec3i(u.ix, u.iy, u.iz)
  def apply(u: Read4[_]) = new Vec3i(u.ix, u.iy, u.iz)
  def apply(xy: Read2[_], z: Int) = new Vec3i(xy.ix, xy.iy, z)
  def apply(x: Int, yz: Read2[_]) = new Vec3i(x, yz.ix, yz.iy)

  def unapply(u: AnyVec3i) = Some((u.x, u.y, u.z))

  implicit def toMutable(u: AnyVec3i) = new Vec3i(u.x, u.y, u.z)
}
