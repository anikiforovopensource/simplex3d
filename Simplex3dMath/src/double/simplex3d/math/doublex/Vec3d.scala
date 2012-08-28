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
sealed abstract class ReadVec3d extends ProtectedVec3d[Double]
with Protected with Serializable
{

  type Clone <: ReadVec3d
  def toConst() :ConstVec3d
  
  type Read = ReadVec3d
  type Mutable = Vec3d
  final def readType: Class[Read] = classOf[ReadVec3d]
  final def mutableCopy() = Vec3d(this)

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

  final def r = px
  final def g = py
  final def b = pz

  final def s = px
  final def t = py
  final def p = pz


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
          "Trying to read index (" + j + ") in " + this.getClass.getSimpleName + "."
        )
    }
  }

  final def unary_+() :ReadVec3d = this
  final def unary_-() = new Vec3d(-x, -y, -z)

  final def *(s: Double) = new Vec3d(x * s, y * s, z * s)
  final def /(s: Double) = { val inv = 1/s; new Vec3d(x * inv, y * inv, z * inv) }
  private[math] final def divByComp(s: Double) = new Vec3d(s / x, s / y, s / z)
  final def +(s: Double) = new Vec3d(x + s, y + s, z + s)
  final def -(s: Double) = new Vec3d(x - s, y - s, z - s)

  final def *(u: inVec3d) = new Vec3d(x * u.x, y * u.y, z * u.z)
  final def /(u: inVec3d) = new Vec3d(x / u.x, y / u.y, z / u.z)
  final def +(u: inVec3d) = new Vec3d(x + u.x, y + u.y, z + u.z)
  final def -(u: inVec3d) = new Vec3d(x - u.x, y - u.y, z - u.z)

  final def *(m: inMat2x3d) :Vec2d = m.transposeMult(this)
  final def *(m: inMat3d) :Vec3d = m.transposeMult(this)
  final def *(m: inMat4x3d) :Vec4d = m.transposeMult(this)


  final override def equals(other: Any) :Boolean = {
    other match {
      case u: ReadVec3b => false
      case u: AnyVec3[_] => x == u.dx && y == u.dy && z == u.dz
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
    prefix + "Vec3" + "(" + x + ", " + y + ", " + z + ")"
  }
}


@SerialVersionUID(8104346712419693669L)
final class ConstVec3d private[math] (cx: Double, cy: Double, cz: Double)
extends ReadVec3d with Immutable with Serializable {
  px = cx; py = cy; pz = cz

  type Clone = ConstVec3d
  override def clone() = this
  def toConst() = this
}


object ConstVec3d {

  def apply(s: Double) = new ConstVec3d(s, s, s)
  def apply(x: Double, y: Double, z: Double) = new ConstVec3d(x, y, z)

  def apply(u: AnyVec3[_]) = new ConstVec3d(u.dx, u.dy, u.dz)
  def apply(u: AnyVec4[_]) = new ConstVec3d(u.dx, u.dy, u.dz)
  def apply(xy: AnyVec2[_], z: Double) = new ConstVec3d(xy.dx, xy.dy, z)
  def apply(x: Double, yz: AnyVec2[_]) = new ConstVec3d(x, yz.dx, yz.dy)

  implicit def toConst(u: ReadVec3d) = apply(u)
}


@SerialVersionUID(8104346712419693669L)
final class Vec3d private[math] (cx: Double, cy: Double, cz: Double)
extends ReadVec3d with Accessor with CompositeFormat
with Accessible with Serializable
{
  px = cx; py = cy; pz = cz

  private[math] def this() { this(0, 0, 0) }
  
  type Clone = Vec3d

  type Const = ConstVec3d
  type Accessor = Vec3d
  type Component = RDouble

  override def clone() = Vec3d(this)
  def toConst() = ConstVec3d(this)
  def :=(u: inVec3d) { x = u.x; y = u.y; z = u.z }


  @noinline override def x_=(s: Double) { px = s }
  @noinline override def y_=(s: Double) { py = s }
  @noinline override def z_=(s: Double) { pz = s }

  override def r_=(s: Double) { px = s }
  override def g_=(s: Double) { py = s }
  override def b_=(s: Double) { pz = s }

  override def s_=(s: Double) { px = s }
  override def t_=(s: Double) { py = s }
  override def p_=(s: Double) { pz = s }

  def update(i: Int, s: Double) {
    i match {
      case 0 => x = s
      case 1 => y = s
      case 2 => z = s
      case j => throw new IndexOutOfBoundsException(
          "Trying to update index (" + j + ") in " + this.getClass.getSimpleName + "."
        )
    }
  }

  def *=(s: Double) { x *= s; y *= s; z *= s }
  def /=(s: Double) { val inv = 1/s; x *= inv; y *= inv; z *= inv }
  def +=(s: Double) { x += s; y += s; z += s }
  def -=(s: Double) { x -= s; y -= s; z -= s }

  def *=(u: inVec3d) { x *= u.x; y *= u.y; z *= u.z }
  def /=(u: inVec3d) { x /= u.x; y /= u.y; z /= u.z }
  def +=(u: inVec3d) { x += u.x; y += u.y; z += u.z }
  def -=(u: inVec3d) { x -= u.x; y -= u.y; z -= u.z }

  def *=(m: inMat3d) { this := m.transposeMult(this) }

  // @SwizzlingStart
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
  // @SwizzlingEnd
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
  def apply(x: Double, y: Double, z: Double) = new Vec3d(x, y, z)

  def apply(u: AnyVec3[_]) = new Vec3d(u.dx, u.dy, u.dz)
  def apply(u: AnyVec4[_]) = new Vec3d(u.dx, u.dy, u.dz)
  def apply(xy: AnyVec2[_], z: Double) = new Vec3d(xy.dx, xy.dy, z)
  def apply(x: Double, yz: AnyVec2[_]) = new Vec3d(x, yz.dx, yz.dy)

  def unapply(u: ReadVec3d) = Some((u.x, u.y, u.z))
}
