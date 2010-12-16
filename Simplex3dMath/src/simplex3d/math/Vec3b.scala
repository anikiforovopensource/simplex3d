/*
 * Simplex3d, CoreMath module
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

import scala.reflect.ClassManifest._
import simplex3d.math.integration.buffer._
import simplex3d.math.CoreMath._


/** The <code>ReadVec3b</code> class represents Boolean 3-dimensional vectors,
 * either constant or mutable.
 * <p>
 *   Boolean vectors do not contain many useful methods. You can operate on them
 *   using <code>BaseMath.any(bvec)</code>, <code>BaseMath.all(bvec)</code>,
 *   and <code>BaseMath.not(bvec)</code>.
 * </p>
 * <p>
 *   Boolean vectors are produced by relational functions in CoreMath, FloatMath,
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
@serializable @SerialVersionUID(8104346712419693669L)
sealed abstract class ReadVec3b extends ProtectedVec3b[Boolean]
{
  private[math] type R2 = ReadVec2b
  private[math] type R3 = ReadVec3b
  private[math] type R4 = ReadVec4b

  private[math] type C2 = ConstVec2b
  private[math] type C3 = ConstVec3b
  private[math] type C4 = ConstVec4b
  
  protected final def make2(x: Double, y: Double) =
    new ConstVec2b(bool(x), bool(y))
  protected final def make3(x: Double, y: Double, z: Double) =
    new ConstVec3b(bool(x), bool(y), bool(z))
  protected final def make4(x: Double, y: Double, z: Double, w: Double) =
    new ConstVec4b(bool(x), bool(y), bool(z), bool(w))

  private[math] final def bx: Boolean = x
  private[math] final def by: Boolean = y
  private[math] final def bz: Boolean = z

  private[math] final def ix: Int = int(x)
  private[math] final def iy: Int = int(y)
  private[math] final def iz: Int = int(z)

  private[math] final def fx: Float = float(x)
  private[math] final def fy: Float = float(y)
  private[math] final def fz: Float = float(z)

  private[math] final def dx: Double = double(x)
  private[math] final def dy: Double = double(y)
  private[math] final def dz: Double = double(z)


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


  protected def x_=(s: Boolean) { throw new UnsupportedOperationException }
  protected def y_=(s: Boolean) { throw new UnsupportedOperationException }
  protected def z_=(s: Boolean) { throw new UnsupportedOperationException }

  protected def r_=(s: Boolean) { throw new UnsupportedOperationException }
  protected def g_=(s: Boolean) { throw new UnsupportedOperationException }
  protected def b_=(s: Boolean) { throw new UnsupportedOperationException }

  protected def s_=(s: Boolean) { throw new UnsupportedOperationException }
  protected def t_=(s: Boolean) { throw new UnsupportedOperationException }
  protected def p_=(s: Boolean) { throw new UnsupportedOperationException }


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

  override def clone() = this

  final override def equals(other: Any) :Boolean = {
    other match {
      case u: ReadVec3b => x == u.x && y == u.y && z == u.z
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
@serializable @SerialVersionUID(8104346712419693669L)
final class ConstVec3b private[math] (
  cx: Boolean, cy: Boolean, cz: Boolean
) extends ReadVec3b with Immutable {
  px = cx; py = cy; pz = cz

  override def clone() = this
}


/** The companion object <code>ConstVec3b</code> that contains factory methods.
 * <p>
 *   To keep the code consistent all the constructors are hidden. Use the
 *   corresponding companion objects as factories to create new instances.
 * </p>
 *
 * @author Aleksey Nikiforov (lex)
 */
object ConstVec3b {

  /** Makes a new instance of ConstVec3b with all the components initialized
   * to the specified value.
   *
   * @param s value for all components.
   * @return a new instance of ConstVec3b with all the components initialized
   *         to the specified value.
   */
  def apply(s: Boolean) = new ConstVec3b(s, s, s)
  
  /** Makes a new instance of ConstVec3b from the specified values.
   * @param x component x.
   * @param y component y.
   * @param z component z.
   * @return a new instance of ConstVec3b with components initialized
   *         to the arguments.
   */
  /*main factory*/ def apply(x: Boolean, y: Boolean, z: Boolean) =
    new ConstVec3b(x, y, z)

  /** Makes a new instance of ConstVec3b from a 3-dimensional vector.
   * @param u any 3-dimensional vector.
   * @return a new instance of ConstVec3b with components initialized
   *         to the components of u converted to Boolean.
   */
  def apply(u: AnyVec3[_]) = new ConstVec3b(u.bx, u.by, u.bz)

  /** Makes a new instance of ConstVec3b from the first three components
   * of a 4-dimensional vector.
   *
   * @param u any 4-dimensional vector.
   * @return a new instance of ConstVec3b with components initialized
   *         to the first three components of u converted to Boolean.
   */
  def apply(u: AnyVec4[_]) = new ConstVec3b(u.bx, u.by, u.bz)

  /** Makes a new instance of ConstVec3b from values extracted from the specified
   * arguments.
   *
   * @param xy components x and y as any 2-dimentional vector.
   * @param z component z.
   * @return a new instance of ConstVec3b with components initialized
   *         to x and y components of xy converted to Boolean
   *         and the specified value z.
   */
  def apply(xy: AnyVec2[_], z: Boolean) = new ConstVec3b(xy.bx, xy.by, z)

  /** Makes a new instance of ConstVec3b from values extracted from the specified
   * arguments.
   *
   * @param x component x.
   * @param yz components y and z as any 2-dimentional vector.
   * @return a new instance of ConstVec3b with components initialized
   *         to the specified value x
   *         and x and y components of yz converted to Boolean.
   */
  def apply(x: Boolean, yz: AnyVec2[_]) = new ConstVec3b(x, yz.bx, yz.by)

  implicit def toConst(u: ReadVec3b) = new ConstVec3b(u.x, u.y, u.z)
}


/** The <code>Vec3b</code> class represents mutable Boolean 3-dimensional
 * vectors.
 * <p>
 *   Boolean vectors do not contain many useful methods. You can operate on them
 *   using <code>BaseMath.any(bvec)</code>, <code>BaseMath.all(bvec)</code>,
 *   and <code>BaseMath.not(bvec)</code>.
 * </p>
 * <p>
 *   Boolean vectors are produced by relational functions in CoreMath, FloatMath,
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
@serializable @SerialVersionUID(8104346712419693669L)
final class Vec3b private[math] (cx: Boolean, cy: Boolean, cz: Boolean)
extends ReadVec3b with Implicits[On] with Composite
{
  type Read = ReadVec3b
  type Const = ConstVec3b
  type Component = Bool

  px = cx; py = cy; pz = cz

  @noinline override def x_=(s: Boolean) { px = s }
  @noinline override def y_=(s: Boolean) { py = s }
  @noinline override def z_=(s: Boolean) { pz = s }

  /** Alias for x.
   */
  override def r_=(s: Boolean) { x = s }

  /** Alias for y.
   */
  override def g_=(s: Boolean) { y = s }

  /** Alias for z.
   */
  override def b_=(s: Boolean) { z = s }


  /** Alias for x.
   */
  override def s_=(s: Boolean) { x = s }

  /** Alias for y.
   */
  override def t_=(s: Boolean) { y = s }

  /** Alias for z.
   */
  override def p_=(s: Boolean) { z = s }

  override def clone() = Vec3b(this)

  /** Set vector components to values from another vector.
   * @param u 3-dimensional Boolean vector.
   */
  def :=(u: inVec3b) { x = u.x; y = u.y; z = u.z }

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
  final val True = new ConstVec3b(true, true, true)
  final val False = new ConstVec3b(false, false, false)

  final val Manifest = classType[Vec3b](classOf[Vec3b])
  final val ConstManifest = classType[ConstVec3b](classOf[ConstVec3b])
  final val ReadManifest = classType[ReadVec3b](classOf[ReadVec3b])

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
  /*main factory*/ def apply(x: Boolean, y: Boolean, z: Boolean) = new Vec3b(x, y, z)

  /** Makes a new instance of Vec3b from a 3-dimensional vector.
   * @param u any 3-dimensional vector.
   * @return a new instance of Vec3b with components initialized
   *         to the components of u converted to Boolean.
   */
  def apply(u: AnyVec3[_]) = new Vec3b(u.bx, u.by, u.bz)

  /** Makes a new instance of Vec3b from the first three components
   * of a 4-dimensional vector.
   *
   * @param u any 4-dimensional vector.
   * @return a new instance of Vec3b with components initialized
   *         to the first three components of u converted to Boolean.
   */
  def apply(u: AnyVec4[_]) = new Vec3b(u.bx, u.by, u.bz)

  /** Makes a new instance of Vec3b from values extracted from the specified
   * arguments.
   *
   * @param xy components x and y as any 2-dimentional vector.
   * @param z component z.
   * @return a new instance of Vec3b with components initialized
   *         to x and y components of xy converted to Boolean
   *         and the specified value z.
   */
  def apply(xy: AnyVec2[_], z: Boolean) = new Vec3b(xy.bx, xy.by, z)
  
  /** Makes a new instance of Vec3b from values extracted from the specified
   * arguments.
   *
   * @param x component x.
   * @param yz components y and z as any 2-dimentional vector.
   * @return a new instance of Vec3b with components initialized
   *         to the specified value x
   *         and x and y components of yz converted to Boolean.
   */
  def apply(x: Boolean, yz: AnyVec2[_]) = new Vec3b(x, yz.bx, yz.by)

  def unapply(u: ReadVec3b) = Some((u.x, u.y, u.z))

  implicit def toMutable(u: ReadVec3b) = new Vec3b(u.x, u.y, u.z)
}
