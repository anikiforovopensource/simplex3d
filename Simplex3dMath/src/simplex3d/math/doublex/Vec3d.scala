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

package simplex3d.math
package doublex

import scala.reflect.ClassManifest._
import simplex3d.integration.data._
import simplex3d.math.doublex.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
@serializable @SerialVersionUID(8104346712419693669L)
sealed abstract class ReadVec3d extends ProtectedVec3d[Double]
{
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

  private[math] final def bx: Boolean = toBool(x)
  private[math] final def by: Boolean = toBool(y)
  private[math] final def bz: Boolean = toBool(z)

  private[math] final def ix: Int = x.toInt
  private[math] final def iy: Int = y.toInt
  private[math] final def iz: Int = z.toInt

  private[math] final def fx: Float = x.toFloat
  private[math] final def fy: Float = y.toFloat
  private[math] final def fz: Float = z.toFloat

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


  protected def x_=(s: Double) { throw new UnsupportedOperationException }
  protected def y_=(s: Double) { throw new UnsupportedOperationException }
  protected def z_=(s: Double) { throw new UnsupportedOperationException }

  protected def r_=(s: Double) { throw new UnsupportedOperationException }
  protected def g_=(s: Double) { throw new UnsupportedOperationException }
  protected def b_=(s: Double) { throw new UnsupportedOperationException }

  protected def s_=(s: Double) { throw new UnsupportedOperationException }
  protected def t_=(s: Double) { throw new UnsupportedOperationException }
  protected def p_=(s: Double) { throw new UnsupportedOperationException }

  
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

  final def unary_+() :ReadVec3d = this
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


@serializable @SerialVersionUID(8104346712419693669L)
final class ConstVec3d private[math] (
  cx: Double, cy: Double, cz: Double
) extends ReadVec3d with Immutable {
  px = cx; py = cy; pz = cz

  override def clone() = this
}

object ConstVec3d {
  def apply(s: Double) = new ConstVec3d(s, s, s)
  /*main factory*/ def apply(x: Double, y: Double, z: Double) = new ConstVec3d(x, y, z)
  def apply(u: AnyVec3[_]) = new ConstVec3d(u.dx, u.dy, u.dz)
  def apply(u: AnyVec4[_]) = new ConstVec3d(u.dx, u.dy, u.dz)
  def apply(xy: AnyVec2[_], z: Double) = new ConstVec3d(xy.dx, xy.dy, z)
  def apply(x: Double, yz: AnyVec2[_]) = new ConstVec3d(x, yz.dx, yz.dy)

  implicit def toConst(u: ReadVec3d) = new ConstVec3d(u.x, u.y, u.z)
}


@serializable @SerialVersionUID(8104346712419693669L)
final class Vec3d private[math] (
  cx: Double, cy: Double, cz: Double
) extends ReadVec3d with Implicits[On] with Composite
{
  type Read = ReadVec3d
  type Const = ConstVec3d
  type Component = RDouble

  px = cx; py = cy; pz = cz

  override def x_=(s: Double) { px = s }
  override def y_=(s: Double) { py = s }
  override def z_=(s: Double) { pz = s }

  /** Alias for x.
   */
  override def r_=(s: Double) { x = s }

  /** Alias for y.
   */
  override def g_=(s: Double) { y = s }

  /** Alias for z.
   */
  override def b_=(s: Double) { z = s }


  /** Alias for x.
   */
  override def s_=(s: Double) { x = s }

  /** Alias for y.
   */
  override def t_=(s: Double) { y = s }

  /** Alias for z.
   */
  override def p_=(s: Double) { z = s }


  def *=(s: Double) { x *= s; y *= s; z *= s }
  def /=(s: Double) { val inv = 1/s; x *= inv; y *= inv; z *= inv }

  def +=(s: Double) { x += s; y += s; z += s }
  def -=(s: Double) { x -= s; y -= s; z -= s }

  def +=(u: inVec3d) { x += u.x; y += u.y; z += u.z }
  def -=(u: inVec3d) { x -= u.x; y -= u.y; z -= u.z }
  def *=(u: inVec3d) { x *= u.x; y *= u.y; z *= u.z }
  def /=(u: inVec3d) { x /= u.x; y /= u.y; z /= u.z }

  def *=(m: inMat3d) { this := m.transposeMul(this) }

  override def clone() = Vec3d(this)
  def :=(u: inVec3d) { x = u.x; y = u.y; z = u.z }

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
  override def xy_=(u: inVec2d) { x = u.x; y = u.y }
  override def xz_=(u: inVec2d) { x = u.x; z = u.y }
  override def yx_=(u: inVec2d) { y = u.x; x = u.y }
  override def yz_=(u: inVec2d) { y = u.x; z = u.y }
  override def zx_=(u: inVec2d) { z = u.x; x = u.y }
  override def zy_=(u: inVec2d) { z = u.x; y = u.y }

  override def xyz_=(u: inVec3d) { x = u.x; y = u.y; z = u.z }
  override def xzy_=(u: inVec3d) { x = u.x; var t = u.z; z = u.y; y = t }
  override def yxz_=(u: inVec3d) { var t = u.y; y = u.x; x = t; z = u.z }
  override def yzx_=(u: inVec3d) { var t = u.y; y = u.x; x = u.z; z = t }
  override def zxy_=(u: inVec3d) { var t = u.z; z = u.x; x = u.y; y = t }
  override def zyx_=(u: inVec3d) { var t = u.z; z = u.x; x = t; y = u.y }

  override def rg_=(u: inVec2d) { xy_=(u) }
  override def rb_=(u: inVec2d) { xz_=(u) }
  override def gr_=(u: inVec2d) { yx_=(u) }
  override def gb_=(u: inVec2d) { yz_=(u) }
  override def br_=(u: inVec2d) { zx_=(u) }
  override def bg_=(u: inVec2d) { zy_=(u) }

  override def rgb_=(u: inVec3d) { xyz_=(u) }
  override def rbg_=(u: inVec3d) { xzy_=(u) }
  override def grb_=(u: inVec3d) { yxz_=(u) }
  override def gbr_=(u: inVec3d) { yzx_=(u) }
  override def brg_=(u: inVec3d) { zxy_=(u) }
  override def bgr_=(u: inVec3d) { zyx_=(u) }

  override def st_=(u: inVec2d) { xy_=(u) }
  override def sp_=(u: inVec2d) { xz_=(u) }
  override def ts_=(u: inVec2d) { yx_=(u) }
  override def tp_=(u: inVec2d) { yz_=(u) }
  override def ps_=(u: inVec2d) { zx_=(u) }
  override def pt_=(u: inVec2d) { zy_=(u) }

  override def stp_=(u: inVec3d) { xyz_=(u) }
  override def spt_=(u: inVec3d) { xzy_=(u) }
  override def tsp_=(u: inVec3d) { yxz_=(u) }
  override def tps_=(u: inVec3d) { yzx_=(u) }
  override def pst_=(u: inVec3d) { zxy_=(u) }
  override def pts_=(u: inVec3d) { zyx_=(u) }
}

object Vec3d {
  final val Zero = new ConstVec3d(0, 0, 0)
  final val UnitX = new ConstVec3d(1, 0, 0)
  final val UnitY = new ConstVec3d(0, 1, 0)
  final val UnitZ = new ConstVec3d(0, 0, 1)
  final val One = new ConstVec3d(1, 1, 1)

  final val Manifest = classType[Vec3d](classOf[Vec3d])
  final val ConstManifest = classType[ConstVec3d](classOf[ConstVec3d])
  final val ReadManifest = classType[ReadVec3d](classOf[ReadVec3d])

  def apply(s: Double) = new Vec3d(s, s, s)
  /*main factory*/ def apply(x: Double, y: Double, z: Double) = new Vec3d(x, y, z)
  def apply(u: AnyVec3[_]) = new Vec3d(u.dx, u.dy, u.dz)
  def apply(u: AnyVec4[_]) = new Vec3d(u.dx, u.dy, u.dz)
  def apply(xy: AnyVec2[_], z: Double) = new Vec3d(xy.dx, xy.dy, z)
  def apply(x: Double, yz: AnyVec2[_]) = new Vec3d(x, yz.dx, yz.dy)

  def unapply(u: ReadVec3d) = Some((u.x, u.y, u.z))

  implicit def toMutable(u: ReadVec3d) = new Vec3d(u.x, u.y, u.z)
  implicit def castInt(u: AnyVec3[Int]) = new Vec3d(u.dx, u.dy, u.dz)
  implicit def castFloat(u: AnyVec3[Float]) = new Vec3d(u.dx, u.dy, u.dz)
}
