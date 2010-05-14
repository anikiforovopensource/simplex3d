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


/** The <code>AnyVec4b</code> class represents Boolean 4-dimensional vectors,
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
sealed abstract class AnyVec4b extends Read4[Boolean] {

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
  private[math] final def bw: Boolean = w

  private[math] final def ix: Int = simplex3d.math.int(x)
  private[math] final def iy: Int = simplex3d.math.int(y)
  private[math] final def iz: Int = simplex3d.math.int(z)
  private[math] final def iw: Int = simplex3d.math.int(w)

  private[math] final def fx: Float = simplex3d.math.float(x)
  private[math] final def fy: Float = simplex3d.math.float(y)
  private[math] final def fz: Float = simplex3d.math.float(z)
  private[math] final def fw: Float = simplex3d.math.float(w)

  private[math] final def dx: Double = simplex3d.math.double(x)
  private[math] final def dy: Double = simplex3d.math.double(y)
  private[math] final def dz: Double = simplex3d.math.double(z)
  private[math] final def dw: Double = simplex3d.math.double(w)


  def x: Boolean
  def y: Boolean
  def z: Boolean
  def w: Boolean

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

  /** Alias for w.
   * @return component w.
   */
  def a = w


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

  /** Alias for w.
   * @return component w.
   */
  def q = w


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

  /** Component-based equality.
   * <p>
   *   Two vectors are equal if all of their components are equal.
   * </p>
   * @param u a vector for comparision.
   * @return true if all the components are equal, false otherwise.
   */
  final def ==(u: inVec4b) :Boolean = {
    if (u eq null) false
    else x == u.x && y == u.y && z == u.z && w == u.w
  }

  /** Component-based equality inverse.
   * <p>
   *   Two vectors are non-equal if any of their components are non-equal.
   * </p>
   * @param u a vector for comparision.
   * @return true if any of the components are not equal, false otherwise.
   */
  final def !=(u: inVec4b) :Boolean = !(this == u)

  final override def equals(other: Any) :Boolean = {
    other match {
      case u: inVec4b => this == u
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
@serializable @SerialVersionUID(5359695191257934190L)
final class ConstVec4b private[math] (
  val x: Boolean, val y: Boolean, val z: Boolean, val w: Boolean
) extends AnyVec4b with Immutable


/** The companion object <code>ConstVec4b</code> that contains factory methods.
 * <p>
 *   To keep the code consistent all the constructors are hidden. Use the
 *   corresponding companion objects as factories to create new instances.
 * </p>
 *
 * @author Aleksey Nikiforov (lex)
 */
object ConstVec4b {

  /** Makes a new instance of ConstVec4b from the specified values.
   * @param x component x.
   * @param y component y.
   * @param z component z.
   * @param w component w.
   * @return a new instance of ConstVec4b with components initialized
   *         to the arguments.
   */
  /* main factory */ def apply(x: Boolean, y: Boolean, z: Boolean, w: Boolean) =
    new ConstVec4b(x, y, z, w)

  /** Makes a new instance of ConstVec4b from a 4-dimensional vector.
   * @param u any 4-dimensional vector.
   * @return a new instance of ConstVec4b with components initialized
   *         to the components of u casted as Boolean.
   */
  def apply(u: Read4[_]) = new ConstVec4b(u.bx, u.by, u.bz, u.bw)

  implicit def toConst(u: AnyVec4b) = new ConstVec4b(u.x, u.y, u.z, u.w)
}


/** The <code>Vec4b</code> class represents mutable Boolean 4-dimensional
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
final class Vec4b private[math] (
  var x: Boolean, var y: Boolean, var z: Boolean, var w: Boolean
) extends AnyVec4b with Mutable with Implicits[On]
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

  /** Alias for w.
   * @return component w.
   */
  override def a = w


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

  /** Alias for w.
   * @return component w.
   */
  override def q = w


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

  /** Alias for w.
   * @return component w.
   */
  def a_=(a: Boolean) { w = a }


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

  /** Alias for w.
   * @return component w.
   */
  def q_=(q: Boolean) { w = q }


  /** Set vector components to values from another vector.
   * @param u 4-dimensional Boolean vector.
   */
  def :=(u: inVec4b) { x = u.x; y = u.y; z = u.z; w = u.w }

  /** Set vector components to the specified values.
   * @param x component x.
   * @param y component y.
   * @param z component z.
   * @param w component w.
   */
  def set(x: Boolean, y: Boolean, z: Boolean, w: Boolean) {
    this.x = x; this.y = y; this.z = z; this.w = w
  }

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
  override def xy: ConstVec2b = new ConstVec2b(x, y)
  override def xz: ConstVec2b = new ConstVec2b(x, z)
  override def xw: ConstVec2b = new ConstVec2b(x, w)
  override def yx: ConstVec2b = new ConstVec2b(y, x)
  override def yz: ConstVec2b = new ConstVec2b(y, z)
  override def yw: ConstVec2b = new ConstVec2b(y, w)
  override def zx: ConstVec2b = new ConstVec2b(z, x)
  override def zy: ConstVec2b = new ConstVec2b(z, y)
  override def zw: ConstVec2b = new ConstVec2b(z, w)
  override def wx: ConstVec2b = new ConstVec2b(w, x)
  override def wy: ConstVec2b = new ConstVec2b(w, y)
  override def wz: ConstVec2b = new ConstVec2b(w, z)

  override def xyz: ConstVec3b = new ConstVec3b(x, y, z)
  override def xyw: ConstVec3b = new ConstVec3b(x, y, w)
  override def xzy: ConstVec3b = new ConstVec3b(x, z, y)
  override def xzw: ConstVec3b = new ConstVec3b(x, z, w)
  override def xwy: ConstVec3b = new ConstVec3b(x, w, y)
  override def xwz: ConstVec3b = new ConstVec3b(x, w, z)
  override def yxz: ConstVec3b = new ConstVec3b(y, x, z)
  override def yxw: ConstVec3b = new ConstVec3b(y, x, w)
  override def yzx: ConstVec3b = new ConstVec3b(y, z, x)
  override def yzw: ConstVec3b = new ConstVec3b(y, z, w)
  override def ywx: ConstVec3b = new ConstVec3b(y, w, x)
  override def ywz: ConstVec3b = new ConstVec3b(y, w, z)
  override def zxy: ConstVec3b = new ConstVec3b(z, x, y)
  override def zxw: ConstVec3b = new ConstVec3b(z, x, w)
  override def zyx: ConstVec3b = new ConstVec3b(z, y, x)
  override def zyw: ConstVec3b = new ConstVec3b(z, y, w)
  override def zwx: ConstVec3b = new ConstVec3b(z, w, x)
  override def zwy: ConstVec3b = new ConstVec3b(z, w, y)
  override def wxy: ConstVec3b = new ConstVec3b(w, x, y)
  override def wxz: ConstVec3b = new ConstVec3b(w, x, z)
  override def wyx: ConstVec3b = new ConstVec3b(w, y, x)
  override def wyz: ConstVec3b = new ConstVec3b(w, y, z)
  override def wzx: ConstVec3b = new ConstVec3b(w, z, x)
  override def wzy: ConstVec3b = new ConstVec3b(w, z, y)

  override def xyzw: ConstVec4b = new ConstVec4b(x, y, z, w)
  override def xywz: ConstVec4b = new ConstVec4b(x, y, w, z)
  override def xzyw: ConstVec4b = new ConstVec4b(x, z, y, w)
  override def xzwy: ConstVec4b = new ConstVec4b(x, z, w, y)
  override def xwyz: ConstVec4b = new ConstVec4b(x, w, y, z)
  override def xwzy: ConstVec4b = new ConstVec4b(x, w, z, y)
  override def yxzw: ConstVec4b = new ConstVec4b(y, x, z, w)
  override def yxwz: ConstVec4b = new ConstVec4b(y, x, w, z)
  override def yzxw: ConstVec4b = new ConstVec4b(y, z, x, w)
  override def yzwx: ConstVec4b = new ConstVec4b(y, z, w, x)
  override def ywxz: ConstVec4b = new ConstVec4b(y, w, x, z)
  override def ywzx: ConstVec4b = new ConstVec4b(y, w, z, x)
  override def zxyw: ConstVec4b = new ConstVec4b(z, x, y, w)
  override def zxwy: ConstVec4b = new ConstVec4b(z, x, w, y)
  override def zyxw: ConstVec4b = new ConstVec4b(z, y, x, w)
  override def zywx: ConstVec4b = new ConstVec4b(z, y, w, x)
  override def zwxy: ConstVec4b = new ConstVec4b(z, w, x, y)
  override def zwyx: ConstVec4b = new ConstVec4b(z, w, y, x)
  override def wxyz: ConstVec4b = new ConstVec4b(w, x, y, z)
  override def wxzy: ConstVec4b = new ConstVec4b(w, x, z, y)
  override def wyxz: ConstVec4b = new ConstVec4b(w, y, x, z)
  override def wyzx: ConstVec4b = new ConstVec4b(w, y, z, x)
  override def wzxy: ConstVec4b = new ConstVec4b(w, z, x, y)
  override def wzyx: ConstVec4b = new ConstVec4b(w, z, y, x)

  override def rg = xy
  override def rb = xz
  override def ra = xw
  override def gr = yx
  override def gb = yz
  override def ga = yw
  override def br = zx
  override def bg = zy
  override def ba = zw
  override def ar = wx
  override def ag = wy
  override def ab = wz

  override def rgb = xyz
  override def rga = xyw
  override def rbg = xzy
  override def rba = xzw
  override def rag = xwy
  override def rab = xwz
  override def grb = yxz
  override def gra = yxw
  override def gbr = yzx
  override def gba = yzw
  override def gar = ywx
  override def gab = ywz
  override def brg = zxy
  override def bra = zxw
  override def bgr = zyx
  override def bga = zyw
  override def bar = zwx
  override def bag = zwy
  override def arg = wxy
  override def arb = wxz
  override def agr = wyx
  override def agb = wyz
  override def abr = wzx
  override def abg = wzy

  override def rgba = xyzw
  override def rgab = xywz
  override def rbga = xzyw
  override def rbag = xzwy
  override def ragb = xwyz
  override def rabg = xwzy
  override def grba = yxzw
  override def grab = yxwz
  override def gbra = yzxw
  override def gbar = yzwx
  override def garb = ywxz
  override def gabr = ywzx
  override def brga = zxyw
  override def brag = zxwy
  override def bgra = zyxw
  override def bgar = zywx
  override def barg = zwxy
  override def bagr = zwyx
  override def argb = wxyz
  override def arbg = wxzy
  override def agrb = wyxz
  override def agbr = wyzx
  override def abrg = wzxy
  override def abgr = wzyx

  override def st = xy
  override def sp = xz
  override def sq = xw
  override def ts = yx
  override def tp = yz
  override def tq = yw
  override def ps = zx
  override def pt = zy
  override def pq = zw
  override def qs = wx
  override def qt = wy
  override def qp = wz

  override def stp = xyz
  override def stq = xyw
  override def spt = xzy
  override def spq = xzw
  override def sqt = xwy
  override def sqp = xwz
  override def tsp = yxz
  override def tsq = yxw
  override def tps = yzx
  override def tpq = yzw
  override def tqs = ywx
  override def tqp = ywz
  override def pst = zxy
  override def psq = zxw
  override def pts = zyx
  override def ptq = zyw
  override def pqs = zwx
  override def pqt = zwy
  override def qst = wxy
  override def qsp = wxz
  override def qts = wyx
  override def qtp = wyz
  override def qps = wzx
  override def qpt = wzy

  override def stpq = xyzw
  override def stqp = xywz
  override def sptq = xzyw
  override def spqt = xzwy
  override def sqtp = xwyz
  override def sqpt = xwzy
  override def tspq = yxzw
  override def tsqp = yxwz
  override def tpsq = yzxw
  override def tpqs = yzwx
  override def tqsp = ywxz
  override def tqps = ywzx
  override def pstq = zxyw
  override def psqt = zxwy
  override def ptsq = zyxw
  override def ptqs = zywx
  override def pqst = zwxy
  override def pqts = zwyx
  override def qstp = wxyz
  override def qspt = wxzy
  override def qtsp = wyxz
  override def qtps = wyzx
  override def qpst = wzxy
  override def qpts = wzyx


  def xy_=(u: inVec2b) { x = u.x; y = u.y }
  def xz_=(u: inVec2b) { x = u.x; z = u.y }
  def xw_=(u: inVec2b) { x = u.x; w = u.y }
  def yx_=(u: inVec2b) { y = u.x; x = u.y }
  def yz_=(u: inVec2b) { y = u.x; z = u.y }
  def yw_=(u: inVec2b) { y = u.x; w = u.y }
  def zx_=(u: inVec2b) { z = u.x; x = u.y }
  def zy_=(u: inVec2b) { z = u.x; y = u.y }
  def zw_=(u: inVec2b) { z = u.x; w = u.y }
  def wx_=(u: inVec2b) { w = u.x; x = u.y }
  def wy_=(u: inVec2b) { w = u.x; y = u.y }
  def wz_=(u: inVec2b) { w = u.x; z = u.y }

  def xyz_=(u: inVec3b) { x = u.x; y = u.y; z = u.z }
  def xyw_=(u: inVec3b) { x = u.x; y = u.y; w = u.z }
  def xzy_=(u: inVec3b) { x = u.x; z = u.y; y = u.z }
  def xzw_=(u: inVec3b) { x = u.x; z = u.y; w = u.z }
  def xwy_=(u: inVec3b) { x = u.x; w = u.y; y = u.z }
  def xwz_=(u: inVec3b) { x = u.x; w = u.y; z = u.z }
  def yxz_=(u: inVec3b) { y = u.x; x = u.y; z = u.z }
  def yxw_=(u: inVec3b) { y = u.x; x = u.y; w = u.z }
  def yzx_=(u: inVec3b) { y = u.x; z = u.y; x = u.z }
  def yzw_=(u: inVec3b) { y = u.x; z = u.y; w = u.z }
  def ywx_=(u: inVec3b) { y = u.x; w = u.y; x = u.z }
  def ywz_=(u: inVec3b) { y = u.x; w = u.y; z = u.z }
  def zxy_=(u: inVec3b) { z = u.x; x = u.y; y = u.z }
  def zxw_=(u: inVec3b) { z = u.x; x = u.y; w = u.z }
  def zyx_=(u: inVec3b) { z = u.x; y = u.y; x = u.z }
  def zyw_=(u: inVec3b) { z = u.x; y = u.y; w = u.z }
  def zwx_=(u: inVec3b) { z = u.x; w = u.y; x = u.z }
  def zwy_=(u: inVec3b) { z = u.x; w = u.y; y = u.z }
  def wxy_=(u: inVec3b) { w = u.x; x = u.y; y = u.z }
  def wxz_=(u: inVec3b) { w = u.x; x = u.y; z = u.z }
  def wyx_=(u: inVec3b) { w = u.x; y = u.y; x = u.z }
  def wyz_=(u: inVec3b) { w = u.x; y = u.y; z = u.z }
  def wzx_=(u: inVec3b) { w = u.x; z = u.y; x = u.z }
  def wzy_=(u: inVec3b) { w = u.x; z = u.y; y = u.z }

  def xyzw_=(u: inVec4b) { x = u.x; y = u.y; z = u.z; w = u.w }
  def xywz_=(u: inVec4b) { x = u.x; y = u.y; var t = u.w; w = u.z; z = t }
  def xzyw_=(u: inVec4b) { x = u.x; var t = u.z; z = u.y; y = t; w = u.w }
  def xzwy_=(u: inVec4b) { x = u.x; var t = u.z; z = u.y; y = u.w; w = t }
  def xwyz_=(u: inVec4b) { x = u.x; var t = u.w; w = u.y; y = u.z; z = t }
  def xwzy_=(u: inVec4b) { x = u.x; var t = u.w; w = u.y; y = t; z = u.z }
  def yxzw_=(u: inVec4b) { var t = u.y; y = u.x; x = t; z = u.z; w = u.w }
  def yxwz_=(u: inVec4b) { var t = u.y; y = u.x; x = t; t = u.w; w = u.z; z=t }
  def yzxw_=(u: inVec4b) { var t = u.y; y = u.x; x = u.z; z = t; w = u.w }
  def yzwx_=(u: inVec4b) { var t = u.y; y = u.x; x = u.w; w = u.z; z = t }
  def ywxz_=(u: inVec4b) { var t = u.y; y = u.x; x = u.z; z = u.w; w = t }
  def ywzx_=(u: inVec4b) { var t = u.y; y = u.x; x = u.w; w = t; z = u.z }
  def zxyw_=(u: inVec4b) { var t = u.z; z = u.x; x = u.y; y = t; w = u.w }
  def zxwy_=(u: inVec4b) { var t = u.z; z = u.x; x = u.y; y = u.w; w = t }
  def zyxw_=(u: inVec4b) { var t = u.z; z = u.x; x = t; y = u.y; w = u.w }
  def zywx_=(u: inVec4b) { var t = u.z; z = u.x; x = u.w; w = t; y = u.y }
  def zwxy_=(u: inVec4b) { var t = u.z; z = u.x; x = t; t = u.w; w = u.y; y=t }
  def zwyx_=(u: inVec4b) { var t = u.z; z = u.x; x = u.w; w = u.y; y = t }
  def wxyz_=(u: inVec4b) { var t = u.w; w = u.x; x = u.y; y = u.z; z = t }
  def wxzy_=(u: inVec4b) { var t = u.w; w = u.x; x = u.y; y = t; z = u.z }
  def wyxz_=(u: inVec4b) { var t = u.w; w = u.x; x = u.z; z = t; y = u.y }
  def wyzx_=(u: inVec4b) { var t = u.w; w = u.x; x = t; y = u.y; z = u.z }
  def wzxy_=(u: inVec4b) { var t = u.w; w = u.x; x = u.z; z = u.y; y = t }
  def wzyx_=(u: inVec4b) { var t = u.w; w = u.x; x = t; t = u.z; z = u.y; y=t }

  def rg_=(u: inVec2b) { xy_=(u) }
  def rb_=(u: inVec2b) { xz_=(u) }
  def ra_=(u: inVec2b) { xw_=(u) }
  def gr_=(u: inVec2b) { yx_=(u) }
  def gb_=(u: inVec2b) { yz_=(u) }
  def ga_=(u: inVec2b) { yw_=(u) }
  def br_=(u: inVec2b) { zx_=(u) }
  def bg_=(u: inVec2b) { zy_=(u) }
  def ba_=(u: inVec2b) { zw_=(u) }
  def ar_=(u: inVec2b) { wx_=(u) }
  def ag_=(u: inVec2b) { wy_=(u) }
  def ab_=(u: inVec2b) { wz_=(u) }

  def rgb_=(u: inVec3b) { xyz_=(u) }
  def rga_=(u: inVec3b) { xyw_=(u) }
  def rbg_=(u: inVec3b) { xzy_=(u) }
  def rba_=(u: inVec3b) { xzw_=(u) }
  def rag_=(u: inVec3b) { xwy_=(u) }
  def rab_=(u: inVec3b) { xwz_=(u) }
  def grb_=(u: inVec3b) { yxz_=(u) }
  def gra_=(u: inVec3b) { yxw_=(u) }
  def gbr_=(u: inVec3b) { yzx_=(u) }
  def gba_=(u: inVec3b) { yzw_=(u) }
  def gar_=(u: inVec3b) { ywx_=(u) }
  def gab_=(u: inVec3b) { ywz_=(u) }
  def brg_=(u: inVec3b) { zxy_=(u) }
  def bra_=(u: inVec3b) { zxw_=(u) }
  def bgr_=(u: inVec3b) { zyx_=(u) }
  def bga_=(u: inVec3b) { zyw_=(u) }
  def bar_=(u: inVec3b) { zwx_=(u) }
  def bag_=(u: inVec3b) { zwy_=(u) }
  def arg_=(u: inVec3b) { wxy_=(u) }
  def arb_=(u: inVec3b) { wxz_=(u) }
  def agr_=(u: inVec3b) { wyx_=(u) }
  def agb_=(u: inVec3b) { wyz_=(u) }
  def abr_=(u: inVec3b) { wzx_=(u) }
  def abg_=(u: inVec3b) { wzy_=(u) }

  def rgba_=(u: inVec4b) { xyzw_=(u) }
  def rgab_=(u: inVec4b) { xywz_=(u) }
  def rbga_=(u: inVec4b) { xzyw_=(u) }
  def rbag_=(u: inVec4b) { xzwy_=(u) }
  def ragb_=(u: inVec4b) { xwyz_=(u) }
  def rabg_=(u: inVec4b) { xwzy_=(u) }
  def grba_=(u: inVec4b) { yxzw_=(u) }
  def grab_=(u: inVec4b) { yxwz_=(u) }
  def gbra_=(u: inVec4b) { yzxw_=(u) }
  def gbar_=(u: inVec4b) { yzwx_=(u) }
  def garb_=(u: inVec4b) { ywxz_=(u) }
  def gabr_=(u: inVec4b) { ywzx_=(u) }
  def brga_=(u: inVec4b) { zxyw_=(u) }
  def brag_=(u: inVec4b) { zxwy_=(u) }
  def bgra_=(u: inVec4b) { zyxw_=(u) }
  def bgar_=(u: inVec4b) { zywx_=(u) }
  def barg_=(u: inVec4b) { zwxy_=(u) }
  def bagr_=(u: inVec4b) { zwyx_=(u) }
  def argb_=(u: inVec4b) { wxyz_=(u) }
  def arbg_=(u: inVec4b) { wxzy_=(u) }
  def agrb_=(u: inVec4b) { wyxz_=(u) }
  def agbr_=(u: inVec4b) { wyzx_=(u) }
  def abrg_=(u: inVec4b) { wzxy_=(u) }
  def abgr_=(u: inVec4b) { wzyx_=(u) }

  def st_=(u: inVec2b) { xy_=(u) }
  def sp_=(u: inVec2b) { xz_=(u) }
  def sq_=(u: inVec2b) { xw_=(u) }
  def ts_=(u: inVec2b) { yx_=(u) }
  def tp_=(u: inVec2b) { yz_=(u) }
  def tq_=(u: inVec2b) { yw_=(u) }
  def ps_=(u: inVec2b) { zx_=(u) }
  def pt_=(u: inVec2b) { zy_=(u) }
  def pq_=(u: inVec2b) { zw_=(u) }
  def qs_=(u: inVec2b) { wx_=(u) }
  def qt_=(u: inVec2b) { wy_=(u) }
  def qp_=(u: inVec2b) { wz_=(u) }

  def stp_=(u: inVec3b) { xyz_=(u) }
  def stq_=(u: inVec3b) { xyw_=(u) }
  def spt_=(u: inVec3b) { xzy_=(u) }
  def spq_=(u: inVec3b) { xzw_=(u) }
  def sqt_=(u: inVec3b) { xwy_=(u) }
  def sqp_=(u: inVec3b) { xwz_=(u) }
  def tsp_=(u: inVec3b) { yxz_=(u) }
  def tsq_=(u: inVec3b) { yxw_=(u) }
  def tps_=(u: inVec3b) { yzx_=(u) }
  def tpq_=(u: inVec3b) { yzw_=(u) }
  def tqs_=(u: inVec3b) { ywx_=(u) }
  def tqp_=(u: inVec3b) { ywz_=(u) }
  def pst_=(u: inVec3b) { zxy_=(u) }
  def psq_=(u: inVec3b) { zxw_=(u) }
  def pts_=(u: inVec3b) { zyx_=(u) }
  def ptq_=(u: inVec3b) { zyw_=(u) }
  def pqs_=(u: inVec3b) { zwx_=(u) }
  def pqt_=(u: inVec3b) { zwy_=(u) }
  def qst_=(u: inVec3b) { wxy_=(u) }
  def qsp_=(u: inVec3b) { wxz_=(u) }
  def qts_=(u: inVec3b) { wyx_=(u) }
  def qtp_=(u: inVec3b) { wyz_=(u) }
  def qps_=(u: inVec3b) { wzx_=(u) }
  def qpt_=(u: inVec3b) { wzy_=(u) }

  def stpq_=(u: inVec4b) { xyzw_=(u) }
  def stqp_=(u: inVec4b) { xywz_=(u) }
  def sptq_=(u: inVec4b) { xzyw_=(u) }
  def spqt_=(u: inVec4b) { xzwy_=(u) }
  def sqtp_=(u: inVec4b) { xwyz_=(u) }
  def sqpt_=(u: inVec4b) { xwzy_=(u) }
  def tspq_=(u: inVec4b) { yxzw_=(u) }
  def tsqp_=(u: inVec4b) { yxwz_=(u) }
  def tpsq_=(u: inVec4b) { yzxw_=(u) }
  def tpqs_=(u: inVec4b) { yzwx_=(u) }
  def tqsp_=(u: inVec4b) { ywxz_=(u) }
  def tqps_=(u: inVec4b) { ywzx_=(u) }
  def pstq_=(u: inVec4b) { zxyw_=(u) }
  def psqt_=(u: inVec4b) { zxwy_=(u) }
  def ptsq_=(u: inVec4b) { zyxw_=(u) }
  def ptqs_=(u: inVec4b) { zywx_=(u) }
  def pqst_=(u: inVec4b) { zwxy_=(u) }
  def pqts_=(u: inVec4b) { zwyx_=(u) }
  def qstp_=(u: inVec4b) { wxyz_=(u) }
  def qspt_=(u: inVec4b) { wxzy_=(u) }
  def qtsp_=(u: inVec4b) { wyxz_=(u) }
  def qtps_=(u: inVec4b) { wyzx_=(u) }
  def qpst_=(u: inVec4b) { wzxy_=(u) }
  def qpts_=(u: inVec4b) { wzyx_=(u) }
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
  val True = new ConstVec4b(true, true, true, true)
  val False = new ConstVec4b(false, false, false, false)

  /** Makes a new instance of Vec4b with all the components initialized
   * to the specified value.
   *
   * @param s value for all components.
   * @return a new instance of Vec4b with all the components initialized
   *         to the specified value.
   */
  def apply(s: Boolean) =
    new Vec4b(s, s, s, s)

  /** Makes a new instance of Vec4b from the specified values.
   * @param x component x.
   * @param y component y.
   * @param z component z.
   * @param w component w.
   * @return a new instance of Vec4b with components initialized
   *         to the arguments.
   */
  /* main factory */ def apply(x: Boolean, y: Boolean, z: Boolean, w: Boolean) =
    new Vec4b(x, y, z, w)

  /** Makes a new instance of Vec4b from a 4-dimensional vector.
   * @param u any 4-dimensional vector.
   * @return a new instance of Vec4b with components initialized
   *         to the components of u casted as Boolean.
   */
  def apply(u: Read4[_]) =
    new Vec4b(u.bx, u.by, u.bz, u.bw)

  /** Makes a new instance of Vec4b from values extracted from the specified
   * arguments.
   *
   * @param xy components x and y as any 2-dimentional vector.
   * @param z component z.
   * @param w component w.
   * @return a new instance of Vec4b with components initialized
   *         to x and y components of xy casted as Boolean
   *         and the specified values z and w.
   */
  def apply(xy: Read2[_], z: Boolean, w: Boolean) =
    new Vec4b(xy.bx, xy.by, z, w)

  /** Makes a new instance of Vec4b from values extracted from the specified
   * arguments.
   *
   * @param x component x.
   * @param yz components y and z as any 2-dimentional vector.
   * @param w component w.
   * @return a new instance of Vec4b with components initialized
   *         to the specified value x,
   *         x and y components of yz casted as Boolean,
   *         and the specified value w.
   */
  def apply(x: Boolean, yz: Read2[_], w: Boolean) =
    new Vec4b(x, yz.bx, yz.by, w)

  /** Makes a new instance of Vec4b from values extracted from the specified
   * arguments.
   *
   * @param x component x.
   * @param y component y.
   * @param zw components z and w as any 2-dimentional vector.
   * @return a new instance of Vec4b with components initialized
   *         to the specified values x and y
   *         and x and y components of zw casted as Boolean.
   */
  def apply(x: Boolean, y: Boolean, zw: Read2[_]) =
    new Vec4b(x, y, zw.bx, zw.by)

  /** Makes a new instance of Vec4b from values extracted from the specified
   * arguments.
   *
   * @param xy components x and y as any 2-dimentional vector.
   * @param zw components z and w as any 2-dimentional vector.
   * @return a new instance of Vec4b with components initialized
   *         to x and y components of xy casted as Boolean
   *         and x and y components of zw casted as Boolean.
   */
  def apply(xy: Read2[_], zw: Read2[_]) =
    new Vec4b(xy.bx, xy.by, zw.bx, zw.by)

  /** Makes a new instance of Vec4b from values extracted from the specified
   * arguments.
   *
   * @param xyz components x, y, and z as any 3-dimentional vector.
   * @param w component w.
   * @return a new instance of Vec4b with components initialized
   *         to x, y, and z components of xyz casted as Boolean
   *         and the specified value w.
   */
  def apply(xyz: Read3[_], w: Boolean) =
    new Vec4b(xyz.bx, xyz.by, xyz.bz, w)

  /** Makes a new instance of Vec4b from values extracted from the specified
   * arguments.
   *
   * @param x component x.
   * @param yzw components y, z, and w as any 3-dimentional vector.
   * @return a new instance of Vec4b with components initialized
   *         to the specified value x
   *         and x, y, and z components of yzw casted as Boolean.
   */
  def apply(x: Boolean, yzw: Read3[_]) =
    new Vec4b(x, yzw.bx, yzw.by, yzw.bz)

  /** Makes a new instance of Vec4b from values extracted from the argument
   * matrix.
   *
   * @param m any 2x2 matrix.
   * @return a new instance of Vec4b with components initialized
   *         to m00, m10, m01, and m11 components of m casted as Boolean.
   */
  def apply(m: Read2x2[_]) =
    new Vec4b(bool(m.f00), bool(m.f10), bool(m.f01), bool(m.f11))

  def unapply(u: AnyVec4b) = Some((u.x, u.y, u.z, u.w))

  implicit def toMutable(u: AnyVec4b) = new Vec4b(u.x, u.y, u.z, u.w)
}
