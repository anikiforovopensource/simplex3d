/*
 * Simplex3d, CoreMath module
 * Copyright (C) 2009-2011, Simplex3d Team
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
import simplex3d.integration.data._
import simplex3d.math.CommonMath._


/** The <code>ReadVec4b</code> class represents Boolean 4-dimensional vectors,
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
sealed abstract class ReadVec4b extends ProtectedVec4b[Boolean]
{
  private[math] type R2 = ReadVec2b
  private[math] type R3 = ReadVec3b
  private[math] type R4 = ReadVec4b

  private[math] type C2 = ConstVec2b
  private[math] type C3 = ConstVec3b
  private[math] type C4 = ConstVec4b
  
  protected final def make2(x: Double, y: Double) =
    new ConstVec2b(Bool(x), Bool(y))
  protected final def make3(x: Double, y: Double, z: Double) =
    new ConstVec3b(Bool(x), Bool(y), Bool(z))
  protected final def make4(x: Double, y: Double, z: Double, w: Double) =
    new ConstVec4b(Bool(x), Bool(y), Bool(z), Bool(w))

  private[math] final def bx: Boolean = x
  private[math] final def by: Boolean = y
  private[math] final def bz: Boolean = z
  private[math] final def bw: Boolean = w

  private[math] final def ix: Int = Int(x)
  private[math] final def iy: Int = Int(y)
  private[math] final def iz: Int = Int(z)
  private[math] final def iw: Int = Int(w)

  private[math] final def fx: Float = Float(x)
  private[math] final def fy: Float = Float(y)
  private[math] final def fz: Float = Float(z)
  private[math] final def fw: Float = Float(w)

  private[math] final def dx: Double = Double(x)
  private[math] final def dy: Double = Double(y)
  private[math] final def dz: Double = Double(z)
  private[math] final def dw: Double = Double(w)


  final def x = px
  final def y = py
  final def z = pz
  final def w = pw

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

  /** Alias for w.
   * @return component w.
   */
  final def a = w


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

  /** Alias for w.
   * @return component w.
   */
  final def q = w


  protected def x_=(s: Boolean) { throw new UnsupportedOperationException }
  protected def y_=(s: Boolean) { throw new UnsupportedOperationException }
  protected def z_=(s: Boolean) { throw new UnsupportedOperationException }
  protected def w_=(s: Boolean) { throw new UnsupportedOperationException }

  protected def r_=(s: Boolean) { throw new UnsupportedOperationException }
  protected def g_=(s: Boolean) { throw new UnsupportedOperationException }
  protected def b_=(s: Boolean) { throw new UnsupportedOperationException }
  protected def a_=(s: Boolean) { throw new UnsupportedOperationException }

  protected def s_=(s: Boolean) { throw new UnsupportedOperationException }
  protected def t_=(s: Boolean) { throw new UnsupportedOperationException }
  protected def p_=(s: Boolean) { throw new UnsupportedOperationException }
  protected def q_=(s: Boolean) { throw new UnsupportedOperationException }


  /** Read a component using sequence notation.
   * @param i index of the component (0 -> x, 1 -> y, 2 -> z, 3 -> w).
   * @return component with index i.
   * @exception IndexOutOfBoundsException if i is outside the range of [0, 3].
   */
  final def apply(i: Int) :Boolean = {
    i match {
      case 0 => x
      case 1 => y
      case 2 => z
      case 3 => w
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 3, got " + j
        )
    }
  }

  override def clone() = this

  final override def equals(other: Any) :Boolean = {
    other match {
      case u: ReadVec4b => x == u.x && y == u.y && z == u.z && w == u.w
      case _ => false
    }
  }

  final override def hashCode() :Int = {
    41 * (
      41 * (
        41 * (
          41 + x.hashCode
        ) + y.hashCode
      ) + z.hashCode
    ) + w.hashCode
  }
  
  final override def toString() :String = {
    this.getClass.getSimpleName + "(" + x + ", " + y + ", " + z + ", " + w + ")"
  }
}


/** The <code>ConstVec4b</code> class represents constant Boolean 4-dimensional
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
 *   <code>val c = ConstVec4(mutable)</code> or implicit cast
 *   <code>val c: ConstVec4 = mutable</code>.
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
final class ConstVec4b private[math] (
  cx: Boolean, cy: Boolean, cz: Boolean, cw: Boolean
) extends ReadVec4b with Immutable {
  px = cx; py = cy; pz = cz; pw = cw

  override def clone() = this
}


/** The companion object <code>ConstVec4b</code> that contains factory methods.
 * <p>
 *   To keep the code consistent all the constructors are hidden. Use the
 *   corresponding companion objects as factories to create new instances.
 * </p>
 *
 * @author Aleksey Nikiforov (lex)
 */
object ConstVec4b {

  /** Makes a new instance of ConstVec4b with all the components initialized
   * to the specified value.
   *
   * @param s value for all components.
   * @return a new instance of ConstVec4b with all the components initialized
   *         to the specified value.
   */
  def apply(s: Boolean) = new ConstVec4b(s, s, s, s)
  
  /** Makes a new instance of ConstVec4b from the specified values.
   * @param x component x.
   * @param y component y.
   * @param z component z.
   * @param w component w.
   * @return a new instance of ConstVec4b with components initialized
   *         to the arguments.
   */
  /*main factory*/ def apply(x: Boolean, y: Boolean, z: Boolean, w: Boolean) =
    new ConstVec4b(x, y, z, w)

  /** Makes a new instance of ConstVec4b from a 4-dimensional vector.
   * @param u any 4-dimensional vector.
   * @return a new instance of ConstVec4b with components initialized
   *         to the components of u converted to Boolean.
   */
  def apply(u: AnyVec4[_]) = new ConstVec4b(u.bx, u.by, u.bz, u.bw)

  /** Makes a new instance of ConstVec4b from values extracted from the specified
   * arguments.
   *
   * @param xy components x and y as any 2-dimentional vector.
   * @param z component z.
   * @param w component w.
   * @return a new instance of ConstVec4b with components initialized
   *         to x and y components of xy converted to Boolean
   *         and the specified values z and w.
   */
  def apply(xy: AnyVec2[_], z: Boolean, w: Boolean) =
    new ConstVec4b(xy.bx, xy.by, z, w)

  /** Makes a new instance of ConstVec4b from values extracted from the specified
   * arguments.
   *
   * @param x component x.
   * @param yz components y and z as any 2-dimentional vector.
   * @param w component w.
   * @return a new instance of ConstVec4b with components initialized
   *         to the specified value x,
   *         x and y components of yz converted to Boolean,
   *         and the specified value w.
   */
  def apply(x: Boolean, yz: AnyVec2[_], w: Boolean) =
    new ConstVec4b(x, yz.bx, yz.by, w)

  /** Makes a new instance of ConstVec4b from values extracted from the specified
   * arguments.
   *
   * @param x component x.
   * @param y component y.
   * @param zw components z and w as any 2-dimentional vector.
   * @return a new instance of ConstVec4b with components initialized
   *         to the specified values x and y
   *         and x and y components of zw converted to Boolean.
   */
  def apply(x: Boolean, y: Boolean, zw: AnyVec2[_]) =
    new ConstVec4b(x, y, zw.bx, zw.by)

  /** Makes a new instance of ConstVec4b from values extracted from the specified
   * arguments.
   *
   * @param xy components x and y as any 2-dimentional vector.
   * @param zw components z and w as any 2-dimentional vector.
   * @return a new instance of ConstVec4b with components initialized
   *         to x and y components of xy converted to Boolean
   *         and x and y components of zw converted to Boolean.
   */
  def apply(xy: AnyVec2[_], zw: AnyVec2[_]) =
    new ConstVec4b(xy.bx, xy.by, zw.bx, zw.by)

  /** Makes a new instance of ConstVec4b from values extracted from the specified
   * arguments.
   *
   * @param xyz components x, y, and z as any 3-dimentional vector.
   * @param w component w.
   * @return a new instance of ConstVec4b with components initialized
   *         to x, y, and z components of xyz converted to Boolean
   *         and the specified value w.
   */
  def apply(xyz: AnyVec3[_], w: Boolean) =
    new ConstVec4b(xyz.bx, xyz.by, xyz.bz, w)

  /** Makes a new instance of ConstVec4b from values extracted from the specified
   * arguments.
   *
   * @param x component x.
   * @param yzw components y, z, and w as any 3-dimentional vector.
   * @return a new instance of ConstVec4b with components initialized
   *         to the specified value x
   *         and x, y, and z components of yzw converted to Boolean.
   */
  def apply(x: Boolean, yzw: AnyVec3[_]) =
    new ConstVec4b(x, yzw.bx, yzw.by, yzw.bz)

  /** Makes a new instance of ConstVec4b from values extracted from the argument
   * matrix.
   *
   * @param m any 2x2 matrix.
   * @return a new instance of ConstVec4b with components initialized
   *         to m00, m10, m01, and m11 components of m converted to Boolean.
   */
  def apply(m: AnyMat2x2[_]) =
    new ConstVec4b(Bool(m.f00), Bool(m.f10), Bool(m.f01), Bool(m.f11))

  /** Makes a new instance of ConstVec4b from quaternion.
   * @param q any quaternion.
   * @return a new instance of ConstVec4b with components initialized
   *         to b, c, d, and a components of q converted to Boolean.
   */
  def apply(q: AnyQuat4[_]) =
    new ConstVec4b(Bool(q.fb), Bool(q.fc), Bool(q.fd), Bool(q.fa))

  implicit def toConst(u: ReadVec4b) = new ConstVec4b(u.x, u.y, u.z, u.w)
}


/** The <code>Vec4b</code> class represents mutable Boolean 4-dimensional
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
final class Vec4b private[math] (
  cx: Boolean, cy: Boolean, cz: Boolean, cw: Boolean
) extends ReadVec4b with Implicits[On] with Composite
{
  type Read = ReadVec4b
  type Const = ConstVec4b
  type Component = Bool

  px = cx; py = cy; pz = cz; pw = cw

  @noinline override def x_=(s: Boolean) { px = s }
  @noinline override def y_=(s: Boolean) { py = s }
  @noinline override def z_=(s: Boolean) { pz = s }
  @noinline override def w_=(s: Boolean) { pw = s }

  /** Alias for x.
   */
  override def r_=(s: Boolean) { x = s }

  /** Alias for y.
   */
  override def g_=(s: Boolean) { y = s }

  /** Alias for z.
   */
  override def b_=(s: Boolean) { z = s }

  /** Alias for w.
   */
  override def a_=(s: Boolean) { w = s }


  /** Alias for x.
   */
  override def s_=(s: Boolean) { x = s }

  /** Alias for y.
   */
  override def t_=(s: Boolean) { y = s }

  /** Alias for z.
   */
  override def p_=(s: Boolean) { z = s }

  /** Alias for w.
   */
  override def q_=(s: Boolean) { w = s }

  override def clone() = Vec4b(this)

  /** Set vector components to values from another vector.
   * @param u 4-dimensional Boolean vector.
   */
  def :=(u: inVec4b) { x = u.x; y = u.y; z = u.z; w = u.w }

  /** Set a component using sequence notation.
   * @param i index of the component (0 -> x, 1 -> y, 2 -> z, 3 -> w).
   * @param s new component value.
   * @exception IndexOutOfBoundsException if i is outside the range of [0, 3].
   */
  def update(i: Int, s: Boolean) {
    i match {
      case 0 => x = s
      case 1 => y = s
      case 2 => z = s
      case 3 => w = s
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 3, got " + j
        )
    }
  }

  // Swizzling
  override def xy_=(u: inVec2b) { x = u.x; y = u.y }
  override def xz_=(u: inVec2b) { x = u.x; z = u.y }
  override def xw_=(u: inVec2b) { x = u.x; w = u.y }
  override def yx_=(u: inVec2b) { y = u.x; x = u.y }
  override def yz_=(u: inVec2b) { y = u.x; z = u.y }
  override def yw_=(u: inVec2b) { y = u.x; w = u.y }
  override def zx_=(u: inVec2b) { z = u.x; x = u.y }
  override def zy_=(u: inVec2b) { z = u.x; y = u.y }
  override def zw_=(u: inVec2b) { z = u.x; w = u.y }
  override def wx_=(u: inVec2b) { w = u.x; x = u.y }
  override def wy_=(u: inVec2b) { w = u.x; y = u.y }
  override def wz_=(u: inVec2b) { w = u.x; z = u.y }

  override def xyz_=(u: inVec3b) { x = u.x; y = u.y; z = u.z }
  override def xyw_=(u: inVec3b) { x = u.x; y = u.y; w = u.z }
  override def xzy_=(u: inVec3b) { x = u.x; z = u.y; y = u.z }
  override def xzw_=(u: inVec3b) { x = u.x; z = u.y; w = u.z }
  override def xwy_=(u: inVec3b) { x = u.x; w = u.y; y = u.z }
  override def xwz_=(u: inVec3b) { x = u.x; w = u.y; z = u.z }
  override def yxz_=(u: inVec3b) { y = u.x; x = u.y; z = u.z }
  override def yxw_=(u: inVec3b) { y = u.x; x = u.y; w = u.z }
  override def yzx_=(u: inVec3b) { y = u.x; z = u.y; x = u.z }
  override def yzw_=(u: inVec3b) { y = u.x; z = u.y; w = u.z }
  override def ywx_=(u: inVec3b) { y = u.x; w = u.y; x = u.z }
  override def ywz_=(u: inVec3b) { y = u.x; w = u.y; z = u.z }
  override def zxy_=(u: inVec3b) { z = u.x; x = u.y; y = u.z }
  override def zxw_=(u: inVec3b) { z = u.x; x = u.y; w = u.z }
  override def zyx_=(u: inVec3b) { z = u.x; y = u.y; x = u.z }
  override def zyw_=(u: inVec3b) { z = u.x; y = u.y; w = u.z }
  override def zwx_=(u: inVec3b) { z = u.x; w = u.y; x = u.z }
  override def zwy_=(u: inVec3b) { z = u.x; w = u.y; y = u.z }
  override def wxy_=(u: inVec3b) { w = u.x; x = u.y; y = u.z }
  override def wxz_=(u: inVec3b) { w = u.x; x = u.y; z = u.z }
  override def wyx_=(u: inVec3b) { w = u.x; y = u.y; x = u.z }
  override def wyz_=(u: inVec3b) { w = u.x; y = u.y; z = u.z }
  override def wzx_=(u: inVec3b) { w = u.x; z = u.y; x = u.z }
  override def wzy_=(u: inVec3b) { w = u.x; z = u.y; y = u.z }

  override def xyzw_=(u: inVec4b) { x = u.x; y = u.y; z = u.z; w = u.w }
  override def xywz_=(u: inVec4b) { x = u.x; y = u.y; var t = u.w; w = u.z; z = t }
  override def xzyw_=(u: inVec4b) { x = u.x; var t = u.z; z = u.y; y = t; w = u.w }
  override def xzwy_=(u: inVec4b) { x = u.x; var t = u.z; z = u.y; y = u.w; w = t }
  override def xwyz_=(u: inVec4b) { x = u.x; var t = u.w; w = u.y; y = u.z; z = t }
  override def xwzy_=(u: inVec4b) { x = u.x; var t = u.w; w = u.y; y = t; z = u.z }
  override def yxzw_=(u: inVec4b) { var t = u.y; y = u.x; x = t; z = u.z; w = u.w }
  override def yxwz_=(u: inVec4b) { var t = u.y; y = u.x; x = t; t = u.w; w = u.z; z=t }
  override def yzxw_=(u: inVec4b) { var t = u.y; y = u.x; x = u.z; z = t; w = u.w }
  override def yzwx_=(u: inVec4b) { var t = u.y; y = u.x; x = u.w; w = u.z; z = t }
  override def ywxz_=(u: inVec4b) { var t = u.y; y = u.x; x = u.z; z = u.w; w = t }
  override def ywzx_=(u: inVec4b) { var t = u.y; y = u.x; x = u.w; w = t; z = u.z }
  override def zxyw_=(u: inVec4b) { var t = u.z; z = u.x; x = u.y; y = t; w = u.w }
  override def zxwy_=(u: inVec4b) { var t = u.z; z = u.x; x = u.y; y = u.w; w = t }
  override def zyxw_=(u: inVec4b) { var t = u.z; z = u.x; x = t; y = u.y; w = u.w }
  override def zywx_=(u: inVec4b) { var t = u.z; z = u.x; x = u.w; w = t; y = u.y }
  override def zwxy_=(u: inVec4b) { var t = u.z; z = u.x; x = t; t = u.w; w = u.y; y=t }
  override def zwyx_=(u: inVec4b) { var t = u.z; z = u.x; x = u.w; w = u.y; y = t }
  override def wxyz_=(u: inVec4b) { var t = u.w; w = u.x; x = u.y; y = u.z; z = t }
  override def wxzy_=(u: inVec4b) { var t = u.w; w = u.x; x = u.y; y = t; z = u.z }
  override def wyxz_=(u: inVec4b) { var t = u.w; w = u.x; x = u.z; z = t; y = u.y }
  override def wyzx_=(u: inVec4b) { var t = u.w; w = u.x; x = t; y = u.y; z = u.z }
  override def wzxy_=(u: inVec4b) { var t = u.w; w = u.x; x = u.z; z = u.y; y = t }
  override def wzyx_=(u: inVec4b) { var t = u.w; w = u.x; x = t; t = u.z; z = u.y; y=t }

  override def rg_=(u: inVec2b) { xy_=(u) }
  override def rb_=(u: inVec2b) { xz_=(u) }
  override def ra_=(u: inVec2b) { xw_=(u) }
  override def gr_=(u: inVec2b) { yx_=(u) }
  override def gb_=(u: inVec2b) { yz_=(u) }
  override def ga_=(u: inVec2b) { yw_=(u) }
  override def br_=(u: inVec2b) { zx_=(u) }
  override def bg_=(u: inVec2b) { zy_=(u) }
  override def ba_=(u: inVec2b) { zw_=(u) }
  override def ar_=(u: inVec2b) { wx_=(u) }
  override def ag_=(u: inVec2b) { wy_=(u) }
  override def ab_=(u: inVec2b) { wz_=(u) }

  override def rgb_=(u: inVec3b) { xyz_=(u) }
  override def rga_=(u: inVec3b) { xyw_=(u) }
  override def rbg_=(u: inVec3b) { xzy_=(u) }
  override def rba_=(u: inVec3b) { xzw_=(u) }
  override def rag_=(u: inVec3b) { xwy_=(u) }
  override def rab_=(u: inVec3b) { xwz_=(u) }
  override def grb_=(u: inVec3b) { yxz_=(u) }
  override def gra_=(u: inVec3b) { yxw_=(u) }
  override def gbr_=(u: inVec3b) { yzx_=(u) }
  override def gba_=(u: inVec3b) { yzw_=(u) }
  override def gar_=(u: inVec3b) { ywx_=(u) }
  override def gab_=(u: inVec3b) { ywz_=(u) }
  override def brg_=(u: inVec3b) { zxy_=(u) }
  override def bra_=(u: inVec3b) { zxw_=(u) }
  override def bgr_=(u: inVec3b) { zyx_=(u) }
  override def bga_=(u: inVec3b) { zyw_=(u) }
  override def bar_=(u: inVec3b) { zwx_=(u) }
  override def bag_=(u: inVec3b) { zwy_=(u) }
  override def arg_=(u: inVec3b) { wxy_=(u) }
  override def arb_=(u: inVec3b) { wxz_=(u) }
  override def agr_=(u: inVec3b) { wyx_=(u) }
  override def agb_=(u: inVec3b) { wyz_=(u) }
  override def abr_=(u: inVec3b) { wzx_=(u) }
  override def abg_=(u: inVec3b) { wzy_=(u) }

  override def rgba_=(u: inVec4b) { xyzw_=(u) }
  override def rgab_=(u: inVec4b) { xywz_=(u) }
  override def rbga_=(u: inVec4b) { xzyw_=(u) }
  override def rbag_=(u: inVec4b) { xzwy_=(u) }
  override def ragb_=(u: inVec4b) { xwyz_=(u) }
  override def rabg_=(u: inVec4b) { xwzy_=(u) }
  override def grba_=(u: inVec4b) { yxzw_=(u) }
  override def grab_=(u: inVec4b) { yxwz_=(u) }
  override def gbra_=(u: inVec4b) { yzxw_=(u) }
  override def gbar_=(u: inVec4b) { yzwx_=(u) }
  override def garb_=(u: inVec4b) { ywxz_=(u) }
  override def gabr_=(u: inVec4b) { ywzx_=(u) }
  override def brga_=(u: inVec4b) { zxyw_=(u) }
  override def brag_=(u: inVec4b) { zxwy_=(u) }
  override def bgra_=(u: inVec4b) { zyxw_=(u) }
  override def bgar_=(u: inVec4b) { zywx_=(u) }
  override def barg_=(u: inVec4b) { zwxy_=(u) }
  override def bagr_=(u: inVec4b) { zwyx_=(u) }
  override def argb_=(u: inVec4b) { wxyz_=(u) }
  override def arbg_=(u: inVec4b) { wxzy_=(u) }
  override def agrb_=(u: inVec4b) { wyxz_=(u) }
  override def agbr_=(u: inVec4b) { wyzx_=(u) }
  override def abrg_=(u: inVec4b) { wzxy_=(u) }
  override def abgr_=(u: inVec4b) { wzyx_=(u) }

  override def st_=(u: inVec2b) { xy_=(u) }
  override def sp_=(u: inVec2b) { xz_=(u) }
  override def sq_=(u: inVec2b) { xw_=(u) }
  override def ts_=(u: inVec2b) { yx_=(u) }
  override def tp_=(u: inVec2b) { yz_=(u) }
  override def tq_=(u: inVec2b) { yw_=(u) }
  override def ps_=(u: inVec2b) { zx_=(u) }
  override def pt_=(u: inVec2b) { zy_=(u) }
  override def pq_=(u: inVec2b) { zw_=(u) }
  override def qs_=(u: inVec2b) { wx_=(u) }
  override def qt_=(u: inVec2b) { wy_=(u) }
  override def qp_=(u: inVec2b) { wz_=(u) }

  override def stp_=(u: inVec3b) { xyz_=(u) }
  override def stq_=(u: inVec3b) { xyw_=(u) }
  override def spt_=(u: inVec3b) { xzy_=(u) }
  override def spq_=(u: inVec3b) { xzw_=(u) }
  override def sqt_=(u: inVec3b) { xwy_=(u) }
  override def sqp_=(u: inVec3b) { xwz_=(u) }
  override def tsp_=(u: inVec3b) { yxz_=(u) }
  override def tsq_=(u: inVec3b) { yxw_=(u) }
  override def tps_=(u: inVec3b) { yzx_=(u) }
  override def tpq_=(u: inVec3b) { yzw_=(u) }
  override def tqs_=(u: inVec3b) { ywx_=(u) }
  override def tqp_=(u: inVec3b) { ywz_=(u) }
  override def pst_=(u: inVec3b) { zxy_=(u) }
  override def psq_=(u: inVec3b) { zxw_=(u) }
  override def pts_=(u: inVec3b) { zyx_=(u) }
  override def ptq_=(u: inVec3b) { zyw_=(u) }
  override def pqs_=(u: inVec3b) { zwx_=(u) }
  override def pqt_=(u: inVec3b) { zwy_=(u) }
  override def qst_=(u: inVec3b) { wxy_=(u) }
  override def qsp_=(u: inVec3b) { wxz_=(u) }
  override def qts_=(u: inVec3b) { wyx_=(u) }
  override def qtp_=(u: inVec3b) { wyz_=(u) }
  override def qps_=(u: inVec3b) { wzx_=(u) }
  override def qpt_=(u: inVec3b) { wzy_=(u) }

  override def stpq_=(u: inVec4b) { xyzw_=(u) }
  override def stqp_=(u: inVec4b) { xywz_=(u) }
  override def sptq_=(u: inVec4b) { xzyw_=(u) }
  override def spqt_=(u: inVec4b) { xzwy_=(u) }
  override def sqtp_=(u: inVec4b) { xwyz_=(u) }
  override def sqpt_=(u: inVec4b) { xwzy_=(u) }
  override def tspq_=(u: inVec4b) { yxzw_=(u) }
  override def tsqp_=(u: inVec4b) { yxwz_=(u) }
  override def tpsq_=(u: inVec4b) { yzxw_=(u) }
  override def tpqs_=(u: inVec4b) { yzwx_=(u) }
  override def tqsp_=(u: inVec4b) { ywxz_=(u) }
  override def tqps_=(u: inVec4b) { ywzx_=(u) }
  override def pstq_=(u: inVec4b) { zxyw_=(u) }
  override def psqt_=(u: inVec4b) { zxwy_=(u) }
  override def ptsq_=(u: inVec4b) { zyxw_=(u) }
  override def ptqs_=(u: inVec4b) { zywx_=(u) }
  override def pqst_=(u: inVec4b) { zwxy_=(u) }
  override def pqts_=(u: inVec4b) { zwyx_=(u) }
  override def qstp_=(u: inVec4b) { wxyz_=(u) }
  override def qspt_=(u: inVec4b) { wxzy_=(u) }
  override def qtsp_=(u: inVec4b) { wyxz_=(u) }
  override def qtps_=(u: inVec4b) { wyzx_=(u) }
  override def qpst_=(u: inVec4b) { wzxy_=(u) }
  override def qpts_=(u: inVec4b) { wzyx_=(u) }
}


/** The companion object <code>Vec4b</code> that contains factory methods
 * and common constant.
 * <p>
 *   To keep the code consistent all the constructors are hidden. Use the
 *   corresponding companion objects as factories to create new instances.
 * </p>
 *
 * @author Aleksey Nikiforov (lex)
 */
object Vec4b {
  final val True = new ConstVec4b(true, true, true, true)
  final val False = new ConstVec4b(false, false, false, false)

  final val Manifest = classType[Vec4b](classOf[Vec4b])
  final val ConstManifest = classType[ConstVec4b](classOf[ConstVec4b])
  final val ReadManifest = classType[ReadVec4b](classOf[ReadVec4b])

  /** Makes a new instance of Vec4b with all the components initialized
   * to the specified value.
   *
   * @param s value for all components.
   * @return a new instance of Vec4b with all the components initialized
   *         to the specified value.
   */
  def apply(s: Boolean) = new Vec4b(s, s, s, s)

  /** Makes a new instance of Vec4b from the specified values.
   * @param x component x.
   * @param y component y.
   * @param z component z.
   * @param w component w.
   * @return a new instance of Vec4b with components initialized
   *         to the arguments.
   */
  /*main factory*/ def apply(x: Boolean, y: Boolean, z: Boolean, w: Boolean) =
    new Vec4b(x, y, z, w)

  /** Makes a new instance of Vec4b from a 4-dimensional vector.
   * @param u any 4-dimensional vector.
   * @return a new instance of Vec4b with components initialized
   *         to the components of u converted to Boolean.
   */
  def apply(u: AnyVec4[_]) =
    new Vec4b(u.bx, u.by, u.bz, u.bw)

  /** Makes a new instance of Vec4b from values extracted from the specified
   * arguments.
   *
   * @param xy components x and y as any 2-dimentional vector.
   * @param z component z.
   * @param w component w.
   * @return a new instance of Vec4b with components initialized
   *         to x and y components of xy converted to Boolean
   *         and the specified values z and w.
   */
  def apply(xy: AnyVec2[_], z: Boolean, w: Boolean) =
    new Vec4b(xy.bx, xy.by, z, w)

  /** Makes a new instance of Vec4b from values extracted from the specified
   * arguments.
   *
   * @param x component x.
   * @param yz components y and z as any 2-dimentional vector.
   * @param w component w.
   * @return a new instance of Vec4b with components initialized
   *         to the specified value x,
   *         x and y components of yz converted to Boolean,
   *         and the specified value w.
   */
  def apply(x: Boolean, yz: AnyVec2[_], w: Boolean) =
    new Vec4b(x, yz.bx, yz.by, w)

  /** Makes a new instance of Vec4b from values extracted from the specified
   * arguments.
   *
   * @param x component x.
   * @param y component y.
   * @param zw components z and w as any 2-dimentional vector.
   * @return a new instance of Vec4b with components initialized
   *         to the specified values x and y
   *         and x and y components of zw converted to Boolean.
   */
  def apply(x: Boolean, y: Boolean, zw: AnyVec2[_]) =
    new Vec4b(x, y, zw.bx, zw.by)

  /** Makes a new instance of Vec4b from values extracted from the specified
   * arguments.
   *
   * @param xy components x and y as any 2-dimentional vector.
   * @param zw components z and w as any 2-dimentional vector.
   * @return a new instance of Vec4b with components initialized
   *         to x and y components of xy converted to Boolean
   *         and x and y components of zw converted to Boolean.
   */
  def apply(xy: AnyVec2[_], zw: AnyVec2[_]) =
    new Vec4b(xy.bx, xy.by, zw.bx, zw.by)

  /** Makes a new instance of Vec4b from values extracted from the specified
   * arguments.
   *
   * @param xyz components x, y, and z as any 3-dimentional vector.
   * @param w component w.
   * @return a new instance of Vec4b with components initialized
   *         to x, y, and z components of xyz converted to Boolean
   *         and the specified value w.
   */
  def apply(xyz: AnyVec3[_], w: Boolean) =
    new Vec4b(xyz.bx, xyz.by, xyz.bz, w)

  /** Makes a new instance of Vec4b from values extracted from the specified
   * arguments.
   *
   * @param x component x.
   * @param yzw components y, z, and w as any 3-dimentional vector.
   * @return a new instance of Vec4b with components initialized
   *         to the specified value x
   *         and x, y, and z components of yzw converted to Boolean.
   */
  def apply(x: Boolean, yzw: AnyVec3[_]) =
    new Vec4b(x, yzw.bx, yzw.by, yzw.bz)

  /** Makes a new instance of Vec4b from values extracted from the argument
   * matrix.
   *
   * @param m any 2x2 matrix.
   * @return a new instance of Vec4b with components initialized
   *         to m00, m10, m01, and m11 components of m converted to Boolean.
   */
  def apply(m: AnyMat2x2[_]) =
    new Vec4b(Bool(m.f00), Bool(m.f10), Bool(m.f01), Bool(m.f11))

  /** Makes a new instance of Vec4b from quaternion.
   * @param q any quaternion.
   * @return a new instance of Vec4b with components initialized
   *         to b, c, d, and a components of q converted to Boolean.
   */
  def apply(q: AnyQuat4[_]) =
    new Vec4b(Bool(q.fb), Bool(q.fc), Bool(q.fd), Bool(q.fa))

  def unapply(u: ReadVec4b) = Some((u.x, u.y, u.z, u.w))

  implicit def toMutable(u: ReadVec4b) = new Vec4b(u.x, u.y, u.z, u.w)
}
