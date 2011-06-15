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


/**
 * @author Aleksey Nikiforov (lex)
 */
@SerialVersionUID(8104346712419693669L)
sealed abstract class ReadVec3i extends ProtectedVec3i[Int]
with ReadPropertyRef[ReadVec3i] with Serializable
{

  type Clone <: ReadVec3i
  type Const = ConstVec3i
  def toConst() = ConstVec3i(this)

  private[math] type R2 = ReadVec2i
  private[math] type R3 = ReadVec3i
  private[math] type R4 = ReadVec4i

  private[math] type C2 = ConstVec2i
  private[math] type C3 = ConstVec3i
  private[math] type C4 = ConstVec4i

  protected final def make2(x: Double, y: Double) =
    new ConstVec2i(x.toInt, y.toInt)
  protected final def make3(x: Double, y: Double, z: Double) =
    new ConstVec3i(x.toInt, y.toInt, z.toInt)
  protected final def make4(x: Double, y: Double, z: Double, w: Double) =
    new ConstVec4i(x.toInt, y.toInt, z.toInt, w.toInt)

  private[math] final def bx: Boolean = simplex3d.math.Boolean(x)
  private[math] final def by: Boolean = simplex3d.math.Boolean(y)
  private[math] final def bz: Boolean = simplex3d.math.Boolean(z)

  private[math] final def ix: Int = x
  private[math] final def iy: Int = y
  private[math] final def iz: Int = z

  private[math] final def fx: Float = x
  private[math] final def fy: Float = y
  private[math] final def fz: Float = z

  private[math] final def dx: Double = x
  private[math] final def dy: Double = y
  private[math] final def dz: Double = z


  final def x = px
  final def y = py
  final def z = pz

  final def r = px
  final def g = py
  final def b = pz

  final def s = px
  final def t = py
  final def p = pz


  protected def x_=(s: Int) { throw new UnsupportedOperationException }
  protected def y_=(s: Int) { throw new UnsupportedOperationException }
  protected def z_=(s: Int) { throw new UnsupportedOperationException }

  protected def r_=(s: Int) { throw new UnsupportedOperationException }
  protected def g_=(s: Int) { throw new UnsupportedOperationException }
  protected def b_=(s: Int) { throw new UnsupportedOperationException }

  protected def s_=(s: Int) { throw new UnsupportedOperationException }
  protected def t_=(s: Int) { throw new UnsupportedOperationException }
  protected def p_=(s: Int) { throw new UnsupportedOperationException }

  final def apply(i: Int) :Int = {
    i match {
      case 0 => x
      case 1 => y
      case 2 => z
      case j => throw new IndexOutOfBoundsException(
          "Expected from 0 to 2, got " + j + "."
        )
    }
  }

  final def unary_+() :ReadVec3i = this
  final def unary_-() = new Vec3i(-x, -y, -z)

  final def *(s: Int) = new Vec3i(x * s, y * s, z * s)
  final def /(s: Int) = new Vec3i(x / s, y / s, z / s)
  private[math] final def divByComp(s: Int) = new Vec3i(s / x, s / y, s / z)
  final def +(s: Int) = new Vec3i(x + s, y + s, z + s)
  final def -(s: Int) = new Vec3i(x - s, y - s, z - s)

  final def *(u: inVec3i) = new Vec3i(x * u.x, y * u.y, z * u.z)
  final def /(u: inVec3i) = new Vec3i(x / u.x, y / u.y, z / u.z)
  final def +(u: inVec3i) = new Vec3i(x + u.x, y + u.y, z + u.z)
  final def -(u: inVec3i) = new Vec3i(x - u.x, y - u.y, z - u.z)

  final def unary_~() = new Vec3i(~x, ~y, ~z)

  final def %(s: Int) = new Vec3i(x % s, y % s, z % s)
  private[math] final def remByComp(s: Int) = new Vec3i(s % x, s % y, s % z)
  final def >>(s: Int) = new Vec3i(x >> s, y >> s, z >> s)
  final def >>>(s: Int) = new Vec3i(x >>> s, y >>> s, z >>> s)
  final def <<(s: Int) = new Vec3i(x << s, y << s, z << s)
  final def &(s: Int) = new Vec3i(x & s, y & s, z & s)
  final def |(s: Int) = new Vec3i(x | s, y | s, z | s)
  final def ^(s: Int) = new Vec3i(x ^ s, y ^ s, z ^ s)

  final def %(u: inVec3i) = new Vec3i(x % u.x, y % u.y, z % u.z)
  final def >>(u: inVec3i) = new Vec3i(x >> u.x, y >> u.y, z >> u.z)
  final def >>>(u: inVec3i) = new Vec3i(x >>> u.x, y >>> u.y, z >>> u.z)
  final def <<(u: inVec3i) = new Vec3i(x << u.x, y << u.y, z << u.z)
  final def &(u: inVec3i) = new Vec3i(x & u.x, y & u.y, z & u.z)
  final def |(u: inVec3i) = new Vec3i(x | u.x, y | u.y, z | u.z)
  final def ^(u: inVec3i) = new Vec3i(x ^ u.x, y ^ u.y, z ^ u.z)


  final override def equals(other: Any) :Boolean = {
    other match {
      case u: ReadVec3i => x == u.x && y == u.y && z == u.z
      case u: ReadVec3b => false
      case u: AnyVec3[_] => dx == u.dx && dy == u.dy && dz == u.dz
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
    prefix + "Vec3i" + "(" + x + ", " + y + ", " + z + ")"
  }
}


@SerialVersionUID(8104346712419693669L)
final class ConstVec3i private[math] (cx: Int, cy: Int, cz: Int)
extends ReadVec3i with Immutable with Serializable {
  px = cx; py = cy; pz = cz

  type Clone = ConstVec3i
  override def clone() = this
}


object ConstVec3i {

  def apply(s: Int) = new ConstVec3i(s, s, s)
  def apply(x: Int, y: Int, z: Int) = new ConstVec3i(x, y, z)

  def apply(u: AnyVec3[_]) = new ConstVec3i(u.ix, u.iy, u.iz)
  def apply(u: AnyVec4[_]) = new ConstVec3i(u.ix, u.iy, u.iz)
  def apply(xy: AnyVec2[_], z: Int) = new ConstVec3i(xy.ix, xy.iy, z)
  def apply(x: Int, yz: AnyVec2[_]) = new ConstVec3i(x, yz.ix, yz.iy)

  implicit def toConst(u: ReadVec3i) = apply(u)
}


@SerialVersionUID(8104346712419693669L)
final class Vec3i private[math] (cx: Int, cy: Int, cz: Int)
extends ReadVec3i with Meta with CompositeFormat with Implicits[On]
with PropertyRef[ReadVec3i] with Serializable
{
  px = cx; py = cy; pz = cz

  type Read = ReadVec3i

  type Meta = Vec3i
  type Component = SInt

  type Clone = Vec3i
  override def clone() = Vec3i(this)
  def :=(u: ConstVec3i) { this := u.asInstanceOf[inVec3i] }
  def :=(u: inVec3i) { x = u.x; y = u.y; z = u.z }


  @noinline override def x_=(s: Int) { px = s }
  @noinline override def y_=(s: Int) { py = s }
  @noinline override def z_=(s: Int) { pz = s }

  override def r_=(s: Int) { px = s }
  override def g_=(s: Int) { py = s }
  override def b_=(s: Int) { pz = s }

  override def s_=(s: Int) { px = s }
  override def t_=(s: Int) { py = s }
  override def p_=(s: Int) { pz = s }

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

  def *=(s: Int) { x *= s; y *= s; z *= s }
  def /=(s: Int) { x /= s; y /= s; z /= s }
  def +=(s: Int) { x += s; y += s; z += s }
  def -=(s: Int) { x -= s; y -= s; z -= s }

  def *=(u: inVec3i) { x *= u.x; y *= u.y; z *= u.z }
  def /=(u: inVec3i) { x /= u.x; y /= u.y; z /= u.z }
  def +=(u: inVec3i) { x += u.x; y += u.y; z += u.z }
  def -=(u: inVec3i) { x -= u.x; y -= u.y; z -= u.z }

  def %=(s: Int) { x %= s; y %= s; z %= s }
  def >>=(s: Int) { x >>= s; y >>= s; z >>= s }
  def >>>=(s: Int) { x >>>= s; y >>>= s; z >>>= s }
  def <<=(s: Int) { x <<= s; y <<= s; z <<= s }
  def &=(s: Int) { x &= s; y &= s; z &= s }
  def |=(s: Int) { x |= s; y |= s; z |= s }
  def ^=(s: Int) { x ^= s; y ^= s; z ^= s }

  def %=(u: inVec3i) { x %= u.x; y %= u.y; z %= u.z }
  def >>=(u: inVec3i) { x >>= u.x; y >>= u.y; z >>= u.z }
  def >>>=(u: inVec3i) { x >>>= u.x; y >>>= u.y; z >>>= u.z }
  def <<=(u: inVec3i) { x <<= u.x; y <<= u.y; z <<= u.z }
  def &=(u: inVec3i) { x &= u.x; y &= u.y; z &= u.z }
  def |=(u: inVec3i) { x |= u.x; y |= u.y; z |= u.z }
  def ^=(u: inVec3i) { x ^= u.x; y ^= u.y; z ^= u.z }

  // Swizzling
  override def xy_=(u: inVec2i) { x = u.x; y = u.y }
  override def xz_=(u: inVec2i) { x = u.x; z = u.y }
  override def yx_=(u: inVec2i) { y = u.x; x = u.y }
  override def yz_=(u: inVec2i) { y = u.x; z = u.y }
  override def zx_=(u: inVec2i) { z = u.x; x = u.y }
  override def zy_=(u: inVec2i) { z = u.x; y = u.y }

  override def xyz_=(u: inVec3i) { x = u.x; y = u.y; z = u.z }
  override def xzy_=(u: inVec3i) { x = u.x; var t = u.z; z = u.y; y = t }
  override def yxz_=(u: inVec3i) { var t = u.y; y = u.x; x = t; z = u.z }
  override def yzx_=(u: inVec3i) { var t = u.y; y = u.x; x = u.z; z = t }
  override def zxy_=(u: inVec3i) { var t = u.z; z = u.x; x = u.y; y = t }
  override def zyx_=(u: inVec3i) { var t = u.z; z = u.x; x = t; y = u.y }

  override def rg_=(u: inVec2i) { xy_=(u) }
  override def rb_=(u: inVec2i) { xz_=(u) }
  override def gr_=(u: inVec2i) { yx_=(u) }
  override def gb_=(u: inVec2i) { yz_=(u) }
  override def br_=(u: inVec2i) { zx_=(u) }
  override def bg_=(u: inVec2i) { zy_=(u) }

  override def rgb_=(u: inVec3i) { xyz_=(u) }
  override def rbg_=(u: inVec3i) { xzy_=(u) }
  override def grb_=(u: inVec3i) { yxz_=(u) }
  override def gbr_=(u: inVec3i) { yzx_=(u) }
  override def brg_=(u: inVec3i) { zxy_=(u) }
  override def bgr_=(u: inVec3i) { zyx_=(u) }

  override def st_=(u: inVec2i) { xy_=(u) }
  override def sp_=(u: inVec2i) { xz_=(u) }
  override def ts_=(u: inVec2i) { yx_=(u) }
  override def tp_=(u: inVec2i) { yz_=(u) }
  override def ps_=(u: inVec2i) { zx_=(u) }
  override def pt_=(u: inVec2i) { zy_=(u) }

  override def stp_=(u: inVec3i) { xyz_=(u) }
  override def spt_=(u: inVec3i) { xzy_=(u) }
  override def tsp_=(u: inVec3i) { yxz_=(u) }
  override def tps_=(u: inVec3i) { yzx_=(u) }
  override def pst_=(u: inVec3i) { zxy_=(u) }
  override def pts_=(u: inVec3i) { zyx_=(u) }
}


object Vec3i {
  final val Zero = new ConstVec3i(0, 0, 0)
  final val UnitX = new ConstVec3i(1, 0, 0)
  final val UnitY = new ConstVec3i(0, 1, 0)
  final val UnitZ = new ConstVec3i(0, 0, 1)
  final val One = new ConstVec3i(1, 1, 1)

  final val Manifest = classType[Vec3i](classOf[Vec3i])
  final val ConstManifest = classType[ConstVec3i](classOf[ConstVec3i])
  final val ReadManifest = classType[ReadVec3i](classOf[ReadVec3i])


  def apply(s: Int) = new Vec3i(s, s, s)
  def apply(x: Int, y: Int, z: Int) = new Vec3i(x, y, z)

  def apply(u: AnyVec3[_]) = new Vec3i(u.ix, u.iy, u.iz)
  def apply(u: AnyVec4[_]) = new Vec3i(u.ix, u.iy, u.iz)
  def apply(xy: AnyVec2[_], z: Int) = new Vec3i(xy.ix, xy.iy, z)
  def apply(x: Int, yz: AnyVec2[_]) = new Vec3i(x, yz.ix, yz.iy)

  def unapply(u: ReadVec3i) = Some((u.x, u.y, u.z))
  implicit def toMutable(u: ReadVec3i) = apply(u)
}
