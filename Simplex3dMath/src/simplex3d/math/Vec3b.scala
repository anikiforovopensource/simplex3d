/*
 * Simplex3d, BaseMath module
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

package simplex3d.math


/** The <code>AnyVec3b</code> class represents Boolean 3-dimensional vectors,
 * either constant or mutable.
 * <p>
 *   Boolean vectors do not contain many useful methods. You can operate on them
 *   using <code>BaseMath.any(bvec)</code>, <code>BaseMath.all(bvec)</code>,
 *   and <code>BaseMath.not(bvec)</code>.
 * </p>
 * <p>
 *   Boolean vectors are produced by relational functions in IntMath, FloatMath,
 *   and DoubleMath:
 *   <ul>
 *     <li><code>lessThan(vec1, vec2)</code></li>
 *     <li><code>lessThanEqual(vec1, vec2)</code></li>
 *     <li><code>greaterThan(vec1, vec2)</code></li>
 *     <li><code>greaterThanEqual(vec1, vec2)</code></li>
 *     <li><code>equal(vec1, vec2)</code></li>
 *     <li><code>notEqual(vec1, vec2)</code></li>
 *   </ul>
 * </p>
 *
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyVec3b extends Read3[Boolean] {

  private[math] type R2 = ConstVec2b
  private[math] type R3 = ConstVec3b
  private[math] type R4 = ConstVec4b
  
  protected final def make2(x: Boolean, y: Boolean) =
    new ConstVec2b(x, y)
  protected final def make3(x: Boolean, y: Boolean, z: Boolean) =
    new ConstVec3b(x, y, z)
  protected final def make4(x: Boolean, y: Boolean, z: Boolean, w: Boolean) =
    new ConstVec4b(x, y, z, w)

  private[math] final def bx: Boolean = x
  private[math] final def by: Boolean = y
  private[math] final def bz: Boolean = z

  private[math] final def ix: Int = simplex3d.math.int(x)
  private[math] final def iy: Int = simplex3d.math.int(y)
  private[math] final def iz: Int = simplex3d.math.int(z)

  private[math] final def fx: Float = simplex3d.math.float(x)
  private[math] final def fy: Float = simplex3d.math.float(y)
  private[math] final def fz: Float = simplex3d.math.float(z)

  private[math] final def dx: Double = simplex3d.math.double(x)
  private[math] final def dy: Double = simplex3d.math.double(y)
  private[math] final def dz: Double = simplex3d.math.double(z)


  def x: Boolean
  def y: Boolean
  def z: Boolean

  /** Alias for x.
   * @return component x.
   */
  def r = x

  /** Alias for y.
   * @return component y.
   */
  def g = y

  /** Alias for z.
   * @return component z.
   */
  def b = z


  /** Alias for x.
   * @return component x.
   */
  def s = x

  /** Alias for y.
   * @return component y.
   */
  def t = y

  /** Alias for z.
   * @return component z.
   */
  def p = z


  /** Read a component using sequence notation.
   * @param i index of the component (0 -> x, 1 -> y, 2 -> z).
   * @return component with index i.
   * @exception IndexOutOfBoundsException if i is outside the range of [0, 2].
   */
  final def apply(i: Int) :Boolean = {
    i match {
      case 0 => x
      case 1 => y
      case 2 => z
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 2, got " + j
        )
    }
  }

  /** Component-based equality.
   * <p>
   *   Two vectors are equal if all of their components are equal.
   * </p>
   * @param u a vector for comparision.
   * @return true if all the components are equal, false otherwise.
   */
  final def ==(u: inVec3b) :Boolean = {
    if (u eq null) false
    else x == u.x && y == u.y && z == u.z
  }

  /** Component-based equality inverse.
   * <p>
   *   Two vectors are non-equal if any of their components are non-equal.
   * </p>
   * @param u a vector for comparision.
   * @return true if any of the components are not equal, false otherwise.
   */
  final def !=(u: inVec3b) :Boolean = !(this == u)

  final override def equals(other: Any) :Boolean = {
    other match {
      case u: inVec3b => this == u
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


/** The <code>ConstVec3b</code> class represents constant Boolean 3-dimensional
 * vectors.
 * <p>
 *   Constant objects cannot be modified after creation. This makes them a good
 *   choise for sharing data in multithreaded context. While the constant
 *   objects cannot be modified themselves, a mutable reference to a constant
 *   object can be rassigned. To ensure that your value never changes you should
 *   use a constant assigned to val: <code> val c = constObject</code>
 * </p>
 * <p>
 *   All the mathematical and logical operations return mutable objects. To
 *   obtain an immutable object you can use an explicit cast
 *   <code>val c = ConstVec3(mutable)</code> or implicit cast
 *   <code>val c: ConstVec3 = mutable</code>.
 * </p>
 * <p>
 *   Methods that return a part of a bigger structure as vector should return
 *   constant objects to prevent errors when a vector is modified but the
 *   changes are not propagated to the original structure. For example, this
 *   approach is used for matrix column accessors.
 * </p>
 *
 * @author Aleksey Nikiforov (lex)
 */
@serializable @SerialVersionUID(5359695191257934190L)
final class ConstVec3b private[math] (
  val x: Boolean, val y: Boolean, val z: Boolean
) extends AnyVec3b with Immutable


/** The companion object <code>ConstVec3b</code> that contains factory methods.
 * <p>
 *   To keep the code consistent all the constructors are hidden. Use the
 *   corresponding companion objects as factories to create new instances.
 * </p>
 *
 * @author Aleksey Nikiforov (lex)
 */
object ConstVec3b {

  /** Makes a new instance of ConstVec3b from the specified values.
   * @param x component x.
   * @param y component y.
   * @param z component z.
   * @return a new instance of ConstVec3b with components initialized
   *         to the arguments.
   */
  /* main factory */ def apply(x: Boolean, y: Boolean, z: Boolean) =
    new ConstVec3b(x, y, z)

  /** Makes a new instance of ConstVec3b from a 3-dimensional vector.
   * @param u any 3-dimensional vector.
   * @return a new instance of ConstVec3b with components initialized
   *         to the components of u casted as Boolean.
   */
  def apply(u: Read3[_]) = new ConstVec3b(u.bx, u.by, u.bz)

  implicit def toConst(u: AnyVec3b) = new ConstVec3b(u.x, u.y, u.z)
}


/** The <code>Vec3b</code> class represents mutable Boolean 3-dimensional
 * vectors.
 * <p>
 *   Boolean vectors do not contain many useful methods. You can operate on them
 *   using <code>BaseMath.any(bvec)</code>, <code>BaseMath.all(bvec)</code>,
 *   and <code>BaseMath.not(bvec)</code>.
 * </p>
 * <p>
 *   Boolean vectors are produced by relational functions in IntMath, FloatMath,
 *   and DoubleMath:
 *   <ul>
 *     <li><code>lessThan(vec1, vec2)</code></li>
 *     <li><code>lessThanEqual(vec1, vec2)</code></li>
 *     <li><code>greaterThan(vec1, vec2)</code></li>
 *     <li><code>greaterThanEqual(vec1, vec2)</code></li>
 *     <li><code>equal(vec1, vec2)</code></li>
 *     <li><code>notEqual(vec1, vec2)</code></li>
 *   </ul>
 * </p>
 *
 * @author Aleksey Nikiforov (lex)
 */
@serializable @SerialVersionUID(5359695191257934190L)
final class Vec3b private[math] (var x: Boolean, var y: Boolean, var z: Boolean)
extends AnyVec3b with Mutable with Implicits[On]
{
  /** Alias for x.
   * @return component x.
   */
  override def r = x

  /** Alias for y.
   * @return component y.
   */
  override def g = y

  /** Alias for z.
   * @return component z.
   */
  override def b = z


  /** Alias for x.
   * @return component x.
   */
  override def s = x

  /** Alias for y.
   * @return component y.
   */
  override def t = y

  /** Alias for z.
   * @return component z.
   */
  override def p = z


  /** Alias for x.
   * @return component x.
   */
  def r_=(r: Boolean) { x = r }

  /** Alias for y.
   * @return component y.
   */
  def g_=(g: Boolean) { y = g }

  /** Alias for z.
   * @return component z.
   */
  def b_=(b: Boolean) { z = b }


  /** Alias for x.
   * @return component x.
   */
  def s_=(s: Boolean) { x = s }

  /** Alias for y.
   * @return component y.
   */
  def t_=(t: Boolean) { y = t }

  /** Alias for z.
   * @return component z.
   */
  def p_=(p: Boolean) { z = p }


  /** Set vector components to values from another vector.
   * @param u 3-dimensional Boolean vector.
   */
  def :=(u: inVec3b) { x = u.x; y = u.y; z = u.z }

  /** Set vector components to the specified values.
   * @param x component x.
   * @param y component y.
   * @param z component z.
   */
  def set(x: Boolean, y: Boolean, z: Boolean) {
    this.x = x; this.y = y; this.z = z
  }

  /** Set a component using sequence notation.
   * @param i index of the component (0 -> x, 1 -> y, 2 -> z).
   * @param s new component value.
   * @exception IndexOutOfBoundsException if i is outside the range of [0, 2].
   */
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

  // Swizzling
  override def xy: ConstVec2b = new ConstVec2b(x, y)
  override def xz: ConstVec2b = new ConstVec2b(x, z)
  override def yx: ConstVec2b = new ConstVec2b(y, x)
  override def yz: ConstVec2b = new ConstVec2b(y, z)
  override def zx: ConstVec2b = new ConstVec2b(z, x)
  override def zy: ConstVec2b = new ConstVec2b(z, y)

  override def xyz: ConstVec3b = new ConstVec3b(x, y, z)
  override def xzy: ConstVec3b = new ConstVec3b(x, z, y)
  override def yxz: ConstVec3b = new ConstVec3b(y, x, z)
  override def yzx: ConstVec3b = new ConstVec3b(y, z, x)
  override def zxy: ConstVec3b = new ConstVec3b(z, x, y)
  override def zyx: ConstVec3b = new ConstVec3b(z, y, x)

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


  def xy_=(u: inVec2b) { x = u.x; y = u.y }
  def xz_=(u: inVec2b) { x = u.x; z = u.y }
  def yx_=(u: inVec2b) { y = u.x; x = u.y }
  def yz_=(u: inVec2b) { y = u.x; z = u.y }
  def zx_=(u: inVec2b) { z = u.x; x = u.y }
  def zy_=(u: inVec2b) { z = u.x; y = u.y }

  def xyz_=(u: inVec3b) { x = u.x; y = u.y; z = u.z }
  def xzy_=(u: inVec3b) { x = u.x; var t = u.z; z = u.y; y = t }
  def yxz_=(u: inVec3b) { var t = u.y; y = u.x; x = t; z = u.z }
  def yzx_=(u: inVec3b) { var t = u.y; y = u.x; x = u.z; z = t }
  def zxy_=(u: inVec3b) { var t = u.z; z = u.x; x = u.y; y = t }
  def zyx_=(u: inVec3b) { var t = u.z; z = u.x; x = t; y = u.y }

  def rg_=(u: inVec2b) { xy_=(u) }
  def rb_=(u: inVec2b) { xz_=(u) }
  def gr_=(u: inVec2b) { yx_=(u) }
  def gb_=(u: inVec2b) { yz_=(u) }
  def br_=(u: inVec2b) { zx_=(u) }
  def bg_=(u: inVec2b) { zy_=(u) }

  def rgb_=(u: inVec3b) { xyz_=(u) }
  def rbg_=(u: inVec3b) { xzy_=(u) }
  def grb_=(u: inVec3b) { yxz_=(u) }
  def gbr_=(u: inVec3b) { yzx_=(u) }
  def brg_=(u: inVec3b) { zxy_=(u) }
  def bgr_=(u: inVec3b) { zyx_=(u) }

  def st_=(u: inVec2b) { xy_=(u) }
  def sp_=(u: inVec2b) { xz_=(u) }
  def ts_=(u: inVec2b) { yx_=(u) }
  def tp_=(u: inVec2b) { yz_=(u) }
  def ps_=(u: inVec2b) { zx_=(u) }
  def pt_=(u: inVec2b) { zy_=(u) }

  def stp_=(u: inVec3b) { xyz_=(u) }
  def spt_=(u: inVec3b) { xzy_=(u) }
  def tsp_=(u: inVec3b) { yxz_=(u) }
  def tps_=(u: inVec3b) { yzx_=(u) }
  def pst_=(u: inVec3b) { zxy_=(u) }
  def pts_=(u: inVec3b) { zyx_=(u) }
}


/** The companion object <code>Vec3b</code> that contains factory methods
 * and common constant.
 * <p>
 *   To keep the code consistent all the constructors are hidden. Use the
 *   corresponding companion objects as factories to create new instances.
 * </p>
 *
 * @author Aleksey Nikiforov (lex)
 */
object Vec3b {
  val True = new ConstVec3b(true, true, true)
  val False = new ConstVec3b(false, false, false)

  /** Makes a new instance of Vec3b with all the components initialized
   * to the specified value.
   *
   * @param s value for all components.
   * @return a new instance of Vec3b with all the components initialized
   *         to the specified value.
   */
  def apply(s: Boolean) = new Vec3b(s, s, s)

  /** Makes a new instance of Vec3b from the specified values.
   * @param x component x.
   * @param y component y.
   * @param z component z.
   * @return a new instance of Vec3b with components initialized
   *         to the arguments.
   */
  /* main factory */ def apply(x: Boolean, y: Boolean, z: Boolean) = new Vec3b(x, y, z)

  /** Makes a new instance of Vec3b from a 3-dimensional vector.
   * @param u any 3-dimensional vector.
   * @return a new instance of Vec3b with components initialized
   *         to the components of u casted as Boolean.
   */
  def apply(u: Read3[_]) = new Vec3b(u.bx, u.by, u.bz)

  /** Makes a new instance of Vec3b from the first three components
   * of a 4-dimensional vector.
   *
   * @param u any 4-dimensional vector.
   * @return a new instance of Vec3b with components initialized
   *         to the first three components of u casted as Boolean.
   */
  def apply(u: Read4[_]) = new Vec3b(u.bx, u.by, u.bz)

  /** Makes a new instance of Vec3b from values extracted from the specified
   * arguments.
   *
   * @param xy components x and y as any 2-dimentional vector.
   * @param z component z.
   * @return a new instance of Vec3b with components initialized
   *         to x and y components of xy casted as Boolean
   *         and the specified value z.
   */
  def apply(xy: Read2[_], z: Boolean) = new Vec3b(xy.bx, xy.by, z)
  
  /** Makes a new instance of Vec3b from values extracted from the specified
   * arguments.
   *
   * @param x component x.
   * @param yz components y and z as any 2-dimentional vector.
   * @return a new instance of Vec3b with components initialized
   *         to the specified value x
   *         and x and y components of yz casted as Boolean.
   */
  def apply(x: Boolean, yz: Read2[_]) = new Vec3b(x, yz.bx, yz.by)

  def unapply(u: AnyVec3b) = Some((u.x, u.y, u.z))

  implicit def toMutable(u: AnyVec3b) = new Vec3b(u.x, u.y, u.z)
}
