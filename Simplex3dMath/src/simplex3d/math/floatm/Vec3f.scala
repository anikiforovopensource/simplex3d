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
import simplex3d.math.integration.buffer._
import simplex3d.math._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class ReadVec3f extends ProtectedVec3f[Float]
{
  private[math] type R2 = ReadVec2f
  private[math] type R3 = ReadVec3f
  private[math] type R4 = ReadVec4f

  protected final def make2(x: Double, y: Double) =
    new ConstVec2f(x.toFloat, y.toFloat)
  protected final def make3(x: Double, y: Double, z: Double) =
    new ConstVec3f(x.toFloat, y.toFloat, z.toFloat)
  protected final def make4(x: Double, y: Double, z: Double, w: Double) =
    new ConstVec4f(x.toFloat, y.toFloat, z.toFloat, w.toFloat)

  private[math] final def bx: Boolean = bool(x)
  private[math] final def by: Boolean = bool(y)
  private[math] final def bz: Boolean = bool(z)

  private[math] final def ix: Int = x.toInt
  private[math] final def iy: Int = y.toInt
  private[math] final def iz: Int = z.toInt

  private[math] final def fx: Float = x
  private[math] final def fy: Float = y
  private[math] final def fz: Float = z

  private[math] final def dx: Double = x
  private[math] final def dy: Double = y
  private[math] final def dz: Double = z


  final def x = px
  final def y = py
  final def z = pz

  /** Alias for x.
   * @return component x.
   */
  final def r = x

  /** Alias for y.
   * @return component y.
   */
  final def g = y

  /** Alias for z.
   * @return component z.
   */
  final def b = z


  /** Alias for x.
   * @return component x.
   */
  final def s = x

  /** Alias for y.
   * @return component y.
   */
  final def t = y

  /** Alias for z.
   * @return component z.
   */
  final def p = z


  protected def x_=(s: Float) { throw new UnsupportedOperationException }
  protected def y_=(s: Float) { throw new UnsupportedOperationException }
  protected def z_=(s: Float) { throw new UnsupportedOperationException }

  protected def r_=(s: Float) { throw new UnsupportedOperationException }
  protected def g_=(s: Float) { throw new UnsupportedOperationException }
  protected def b_=(s: Float) { throw new UnsupportedOperationException }

  protected def s_=(s: Float) { throw new UnsupportedOperationException }
  protected def t_=(s: Float) { throw new UnsupportedOperationException }
  protected def p_=(s: Float) { throw new UnsupportedOperationException }

  
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

  final def unary_+() :ReadVec3f = this
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

  override def clone() = this

  final override def equals(other: Any) :Boolean = {
    other match {
      case u: ReadVec3b => false
      case u: AnyVec3[_] => dx == u.dx && dy == u.dy && dz == u.dz
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
final class ConstVec3f private[math] (cx: Float, cy: Float, cz: Float)
extends ReadVec3f with Immutable {
  px = cx; py = cy; pz = cz

  override def clone() = this
}

object ConstVec3f {
  def apply(s: Float) = new ConstVec3f(s, s, s)
  /*main factory*/ def apply(x: Float, y: Float, z: Float) = new ConstVec3f(x, y, z)
  def apply(u: AnyVec3[_]) = new ConstVec3f(u.fx, u.fy, u.fz)
  def apply(u: AnyVec4[_]) = new ConstVec3f(u.fx, u.fy, u.fz)
  def apply(xy: AnyVec2[_], z: Float) = new ConstVec3f(xy.fx, xy.fy, z)
  def apply(x: Float, yz: AnyVec2[_]) = new ConstVec3f(x, yz.fx, yz.fy)

  implicit def toConst(u: ReadVec3f) = new ConstVec3f(u.x, u.y, u.z)
}


@serializable @SerialVersionUID(5359695191257934190L)
final class Vec3f private[math] (cx: Float, cy: Float, cz: Float)
extends ReadVec3f with Implicits[On] with Composite
{
  type Element = ReadVec3f
  type Component = Float1

  px = cx; py = cy; pz = cz

  override def x_=(s: Float) { px = s }
  override def y_=(s: Float) { py = s }
  override def z_=(s: Float) { pz = s }

  /** Alias for x.
   */
  override def r_=(s: Float) { x = s }

  /** Alias for y.
   */
  override def g_=(s: Float) { y = s }

  /** Alias for z.
   */
  override def b_=(s: Float) { z = s }


  /** Alias for x.
   */
  override def s_=(s: Float) { x = s }

  /** Alias for y.
   */
  override def t_=(s: Float) { y = s }

  /** Alias for z.
   */
  override def p_=(s: Float) { z = s }


  def *=(s: Float) { x *= s; y *= s; z *= s }
  def /=(s: Float) { val inv = 1/s; x *= inv; y *= inv; z *= inv }

  def +=(s: Float) { x += s; y += s; z += s }
  def -=(s: Float) { x -= s; y -= s; z -= s }

  def +=(u: inVec3f) { x += u.x; y += u.y; z += u.z }
  def -=(u: inVec3f) { x -= u.x; y -= u.y; z -= u.z }
  def *=(u: inVec3f) { x *= u.x; y *= u.y; z *= u.z }
  def /=(u: inVec3f) { x /= u.x; y /= u.y; z /= u.z }

  def *=(m: inMat3f) { this := m.transposeMul(this) }

  override def clone() = Vec3f(this)
  def :=(u: inVec3f) { x = u.x; y = u.y; z = u.z }

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
  override def xy_=(u: inVec2f) { x = u.x; y = u.y }
  override def xz_=(u: inVec2f) { x = u.x; z = u.y }
  override def yx_=(u: inVec2f) { y = u.x; x = u.y }
  override def yz_=(u: inVec2f) { y = u.x; z = u.y }
  override def zx_=(u: inVec2f) { z = u.x; x = u.y }
  override def zy_=(u: inVec2f) { z = u.x; y = u.y }

  override def xyz_=(u: inVec3f) { x = u.x; y = u.y; z = u.z }
  override def xzy_=(u: inVec3f) { x = u.x; var t = u.z; z = u.y; y = t }
  override def yxz_=(u: inVec3f) { var t = u.y; y = u.x; x = t; z = u.z }
  override def yzx_=(u: inVec3f) { var t = u.y; y = u.x; x = u.z; z = t }
  override def zxy_=(u: inVec3f) { var t = u.z; z = u.x; x = u.y; y = t }
  override def zyx_=(u: inVec3f) { var t = u.z; z = u.x; x = t; y = u.y }

  override def rg_=(u: inVec2f) { xy_=(u) }
  override def rb_=(u: inVec2f) { xz_=(u) }
  override def gr_=(u: inVec2f) { yx_=(u) }
  override def gb_=(u: inVec2f) { yz_=(u) }
  override def br_=(u: inVec2f) { zx_=(u) }
  override def bg_=(u: inVec2f) { zy_=(u) }

  override def rgb_=(u: inVec3f) { xyz_=(u) }
  override def rbg_=(u: inVec3f) { xzy_=(u) }
  override def grb_=(u: inVec3f) { yxz_=(u) }
  override def gbr_=(u: inVec3f) { yzx_=(u) }
  override def brg_=(u: inVec3f) { zxy_=(u) }
  override def bgr_=(u: inVec3f) { zyx_=(u) }

  override def st_=(u: inVec2f) { xy_=(u) }
  override def sp_=(u: inVec2f) { xz_=(u) }
  override def ts_=(u: inVec2f) { yx_=(u) }
  override def tp_=(u: inVec2f) { yz_=(u) }
  override def ps_=(u: inVec2f) { zx_=(u) }
  override def pt_=(u: inVec2f) { zy_=(u) }

  override def stp_=(u: inVec3f) { xyz_=(u) }
  override def spt_=(u: inVec3f) { xzy_=(u) }
  override def tsp_=(u: inVec3f) { yxz_=(u) }
  override def tps_=(u: inVec3f) { yzx_=(u) }
  override def pst_=(u: inVec3f) { zxy_=(u) }
  override def pts_=(u: inVec3f) { zyx_=(u) }
}

object Vec3f {
  final val Zero = new ConstVec3f(0, 0, 0)
  final val UnitX = new ConstVec3f(1, 0, 0)
  final val UnitY = new ConstVec3f(0, 1, 0)
  final val UnitZ = new ConstVec3f(0, 0, 1)
  final val One = new ConstVec3f(1, 1, 1)
  final val Manifest = classType[ReadVec3f](classOf[ReadVec3f])

  def apply(s: Float) = new Vec3f(s, s, s)
  /*main factory*/ def apply(x: Float, y: Float, z: Float) = new Vec3f(x, y, z)
  def apply(u: AnyVec3[_]) = new Vec3f(u.fx, u.fy, u.fz)
  def apply(u: AnyVec4[_]) = new Vec3f(u.fx, u.fy, u.fz)
  def apply(xy: AnyVec2[_], z: Float) = new Vec3f(xy.fx, xy.fy, z)
  def apply(x: Float, yz: AnyVec2[_]) = new Vec3f(x, yz.fx, yz.fy)

  def unapply(u: ReadVec3f) = Some((u.x, u.y, u.z))

  implicit def toMutable(u: ReadVec3f) = new Vec3f(u.x, u.y, u.z)
  implicit def castInt(u: AnyVec3[Int]) = new Vec3f(u.fx, u.fy, u.fz)
}
