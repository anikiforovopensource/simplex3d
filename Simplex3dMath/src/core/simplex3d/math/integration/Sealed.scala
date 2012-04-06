/*
 * Simplex3dMath - Core Module
 * Copyright (C) 2010-2011, Aleksey Nikiforov
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

import java.nio
import scala.reflect.ClassManifest.{classType}


package integration {
  
  /** <code>Format</code> indicates format.
   *
   * @author Aleksey Nikiforov (lex)
   */
  sealed trait Format {
    type Accessor <: simplex3d.math.integration.Accessor
    type Component <: PrimitiveFormat
  }

  /** <code>PrimitiveFormat</code>.
   *
   * @author Aleksey Nikiforov (lex)
   */
  sealed trait PrimitiveFormat extends Format {
    type Accessor <: simplex3d.math.integration.Accessor { type Read <: AnyVal }
  }

  object PrimitiveFormat {
    final val Bool = classType[Bool](classOf[Bool])
    final val SInt = classType[SInt](classOf[SInt])
    final val RFloat = classType[RFloat](classOf[RFloat])
    final val RDouble = classType[RDouble](classOf[RDouble])
  }


  /** <code>CompositeFormat</code>.
   *
   * @author Aleksey Nikiforov (lex)
   */
  trait CompositeFormat extends Format {
    type Accessor <: simplex3d.math.integration.Accessor { type Read <: AnyRef }
  }
  
  trait CompressedFormat extends Format {
    type Accessor <: simplex3d.math.integration.Accessor
    type Component = Nothing
  }


  sealed trait Bool extends Accessor with PrimitiveFormat {
    type Read = Boolean
    type Const = Read

    type Accessor = Bool
    type Component = Bool
  }


  sealed trait Raw {
    type Array <: AnyRef // Scalac cannot handle the bound "<: Array[_]"
    type Buffer <: nio.Buffer
  }

  sealed trait Tangible extends Raw
  sealed trait TangibleInt extends Tangible
  sealed trait TangibleIndex extends TangibleInt with Unsigned
  sealed trait TangibleFloat extends Tangible
  sealed trait TangibleDouble extends Tangible

  sealed trait Integral extends Raw

  sealed trait Signed extends Integral
  sealed trait Unsigned extends Integral


  sealed trait RawByte extends Integral {
    type Array = scala.Array[Byte]
    type Buffer = nio.ByteBuffer
  }

  sealed trait SByte extends RawByte with Signed
  with TangibleInt with TangibleFloat with TangibleDouble

  sealed trait UByte extends RawByte with Unsigned
  with TangibleIndex with TangibleFloat with TangibleDouble


  sealed trait RawShort extends Integral

  sealed trait SShort extends RawShort with Signed
  with TangibleInt with TangibleFloat with TangibleDouble {
    type Array = scala.Array[Short]
    type Buffer = nio.ShortBuffer
  }

  sealed trait UShort extends RawShort with Unsigned
  with TangibleIndex with TangibleFloat with TangibleDouble {
    type Array = scala.Array[Char]
    type Buffer = nio.CharBuffer
  }


  sealed trait RawInt extends Integral {
    type Array = scala.Array[Int]
    type Buffer = nio.IntBuffer
  }

  sealed trait SInt extends types.AnyVec[Int] with Accessor with PrimitiveFormat with RawInt with Signed
  with TangibleInt with TangibleFloat with TangibleDouble {
    type Read = Int
    type Const = Read

    type Accessor = SInt
    type Component = SInt
  }

  sealed trait UInt extends RawInt with Unsigned
  with TangibleIndex with TangibleFloat with TangibleDouble


  sealed trait FloatingPoint extends Raw

  /** System floating point: either float or double.
   */
  sealed trait SysFP extends FloatingPoint

  /** Hafl-float, in other words 16-bit float.
   */
  sealed trait HFloat extends FloatingPoint
  with TangibleFloat with TangibleDouble {
    type Array = scala.Array[Short]
    type Buffer = nio.ShortBuffer
  }

  /** Raw Float. 32-bit float.
   */
  sealed trait RFloat extends types.AnyVec[Float] with Accessor with PrimitiveFormat with SysFP
  with TangibleFloat with TangibleDouble {
    type Read = Float
    type Const = Read

    type Accessor = RFloat
    type Component = RFloat

    type Array = scala.Array[Float]
    type Buffer = nio.FloatBuffer
  }

  /** Raw Double. 64-bit float.
   */
  sealed trait RDouble extends types.AnyVec[Double] with Accessor with PrimitiveFormat with SysFP
  with TangibleDouble {
    type Read = Double
    type Const = Read

    type Accessor = RDouble
    type Component = RDouble

    type Array = scala.Array[Double]
    type Buffer = nio.DoubleBuffer
  }
}


package types {
  
  /** <code>MathType</code> is a supertype of all the math objects.
   *
   * @author Aleksey Nikiforov (lex)
   */
  @SerialVersionUID(8104346712419693669L)
  sealed trait MathType extends Binding with Serializable with Cloneable[MathType]


  /** <code>AnyQuat4</code> is a supertype of all the quaternions.
   * <p>
   *   There are double and float quaternions.
   * </p>
   *
   * @author Aleksey Nikiforov (lex)
   */
  abstract class AnyQuat4[P] private[math] () extends MathType with VectorLike {
    override def clone() :AnyQuat4[P] = throw new UnsupportedOperationException()
    
    def apply(i: Int) :P

    private[math] def fa: Float
    private[math] def fb: Float
    private[math] def fc: Float
    private[math] def fd: Float

    private[math] def da: Double
    private[math] def db: Double
    private[math] def dc: Double
    private[math] def dd: Double
  }

  /** <code>AnyMat</code> is a supertype of all the matrices.
   * <p>
   *   There are double and float matrices.
   * </p>
   *
   * @author Aleksey Nikiforov (lex)
   */
  sealed abstract class AnyMat[P] private[math] () extends MathType {
    override def clone() :AnyMat[P] = throw new UnsupportedOperationException()
    
    def columns: Int
    def rows: Int
    def apply(c: Int, r: Int) :P

    private[math] def f00: Float
    private[math] def f01: Float
    private[math] def f02: Float = 0
    private[math] def f03: Float = 0

    private[math] def f10: Float
    private[math] def f11: Float
    private[math] def f12: Float = 0
    private[math] def f13: Float = 0

    private[math] def f20: Float = 0
    private[math] def f21: Float = 0
    private[math] def f22: Float = 1
    private[math] def f23: Float = 0

    private[math] def f30: Float = 0
    private[math] def f31: Float = 0
    private[math] def f32: Float = 0
    private[math] def f33: Float = 1


    private[math] def d00: Double
    private[math] def d01: Double
    private[math] def d02: Double = 0
    private[math] def d03: Double = 0

    private[math] def d10: Double
    private[math] def d11: Double
    private[math] def d12: Double = 0
    private[math] def d13: Double = 0

    private[math] def d20: Double = 0
    private[math] def d21: Double = 0
    private[math] def d22: Double = 1
    private[math] def d23: Double = 0

    private[math] def d30: Double = 0
    private[math] def d31: Double = 0
    private[math] def d32: Double = 0
    private[math] def d33: Double = 1
  }

  /** <code>AnyMat2</code> is a supertype of all the 2x2 matrices.
   * <p>
   *   There are double and float matrices.
   * </p>
   *
   * @author Aleksey Nikiforov (lex)
   */
  abstract class AnyMat2[P] private[math] () extends AnyMat[P] {
    override def clone() :AnyMat2[P] = throw new UnsupportedOperationException()
    
    final def columns = 2
    final def rows = 2
  }

  /** <code>AnyMat2x3</code> is a supertype of all the 2x3 matrices.
   * <p>
   *   There are double and float matrices.
   * </p>
   *
   * @author Aleksey Nikiforov (lex)
   */
  abstract class AnyMat2x3[P] private[math] () extends AnyMat[P] {
    override def clone() :AnyMat2x3[P] = throw new UnsupportedOperationException()
    
    final def columns = 2
    final def rows = 3
  }

  /** <code>AnyMat2x4</code> is a supertype of all the 2x4 matrices.
   * <p>
   *   There are double and float matrices.
   * </p>
   *
   * @author Aleksey Nikiforov (lex)
   */
  abstract class AnyMat2x4[P] private[math] () extends AnyMat[P] {
    override def clone() :AnyMat2x4[P] = throw new UnsupportedOperationException()
    
    final def columns = 2
    final def rows = 4
  }

  /** <code>AnyMat3x2</code> is a supertype of all the 3x2 matrices.
   * <p>
   *   There are double and float matrices.
   * </p>
   *
   * @author Aleksey Nikiforov (lex)
   */
  abstract class AnyMat3x2[P] private[math] () extends AnyMat[P] {
    override def clone() :AnyMat3x2[P] = throw new UnsupportedOperationException()
    
    final def columns = 3
    final def rows = 2
  }

  /** <code>AnyMat3</code> is a supertype of all the 3x3 matrices.
   * <p>
   *   There are double and float matrices.
   * </p>
   *
   * @author Aleksey Nikiforov (lex)
   */
  abstract class AnyMat3[P] private[math] () extends AnyMat[P] {
    override def clone() :AnyMat3[P] = throw new UnsupportedOperationException()
    
    final def columns = 3
    final def rows = 3
  }

  /** <code>AnyMat3x4</code> is a supertype of all the 3x4 matrices.
   * <p>
   *   There are double and float matrices.
   * </p>
   *
   * @author Aleksey Nikiforov (lex)
   */
  abstract class AnyMat3x4[P] private[math] () extends AnyMat[P] {
    override def clone() :AnyMat3x4[P] = throw new UnsupportedOperationException()
    
    final def columns = 3
    final def rows = 4
  }

  /** <code>AnyMat4x2</code> is a supertype of all the 4x2 matrices.
   * <p>
   *   There are double and float matrices.
   * </p>
   *
   * @author Aleksey Nikiforov (lex)
   */
  abstract class AnyMat4x2[P] private[math] () extends AnyMat[P] {
    override def clone() :AnyMat4x2[P] = throw new UnsupportedOperationException()
    
    final def columns = 4
    final def rows = 2
  }

  /** <code>AnyMat4x3</code> is a supertype of all the 4x3 matrices.
   * <p>
   *   There are double and float matrices.
   * </p>
   *
   * @author Aleksey Nikiforov (lex)
   */
  abstract class AnyMat4x3[P] private[math] () extends AnyMat[P] {
    override def clone() :AnyMat4x3[P] = throw new UnsupportedOperationException()

    final def columns = 4
    final def rows = 3
  }

  /** <code>AnyMat4</code> is a supertype of all the 4x4 matrices.
   * <p>
   *   There are double and float matrices.
   * </p>
   *
   * @author Aleksey Nikiforov (lex)
   */
  abstract class AnyMat4[P] private[math] () extends AnyMat[P] {
    override def clone() :AnyMat4[P] = throw new UnsupportedOperationException()
    
    final def columns = 4
    final def rows = 4
  }


  /** <code>AnyVec</code> is a base class for all vectors.
   *
   * @author Aleksey Nikiforov (lex)
   */
  sealed trait AnyVec[P] extends MathType with VectorLike {
    override def clone() :AnyVec[P] = throw new UnsupportedOperationException()
    
    def components: Int
    def apply(i: Int) :P

    private[math] def bx: Boolean
    private[math] def ix: Int
    private[math] def fx: Float
    private[math] def dx: Double
  }


  /** Instead of AnyVec1[P].
   *
   * @author Aleksey Nikiforov (lex)
   */
  abstract class PrimitiveRef[P] private[math] () extends AnyVec[P] with Cloneable[PrimitiveRef[P]] {
    final def components = 1
  }


  /** <code>AnyVec2</code> is a supertype of all the 2-dimensional vectors.
   * <p>
   *   There are double, float, int, and boolean vectors.
   * </p>
   *
   * @author Aleksey Nikiforov (lex)
   */
  abstract class AnyVec2[P] private[math] () extends
  /* @SwizzlingStart */ VecImpl234[P] with /* @SwizzlingEnd */
  AnyVec[P] {
    
    override def clone() :AnyVec2[P] = throw new UnsupportedOperationException()

    final def components = 2

    private[math] def bx: Boolean
    private[math] def by: Boolean

    private[math] def ix: Int
    private[math] def iy: Int

    private[math] def fx: Float
    private[math] def fy: Float

    private[math] def dx: Double
    private[math] def dy: Double
  }


  /** <code>AnyVec3</code> is a supertype of all the 3-dimensional vectors.
   * <p>
   *   There are double, float, int, and boolean vectors.
   * </p>
   *
   * @author Aleksey Nikiforov (lex)
   */
  abstract class AnyVec3[P] private[math] () extends
  /* @SwizzlingStart */ VecImpl34[P] with /* @SwizzlingEnd */
  AnyVec[P] {
    
    override def clone() :AnyVec3[P] = throw new UnsupportedOperationException()

    final def components = 3

    private[math] def bx: Boolean
    private[math] def by: Boolean
    private[math] def bz: Boolean

    private[math] def ix: Int
    private[math] def iy: Int
    private[math] def iz: Int

    private[math] def fx: Float
    private[math] def fy: Float
    private[math] def fz: Float

    private[math] def dx: Double
    private[math] def dy: Double
    private[math] def dz: Double
  }


  /** <code>AnyVec4</code> is a supertype of all the 4-dimensional vectors.
   * <p>
   *   There are double, float, int, and boolean vectors.
   * </p>
   *
   * @author Aleksey Nikiforov (lex)
   */
  abstract class AnyVec4[P] private[math] () extends
  /* @SwizzlingStart */ VecImpl34[P] with /* @SwizzlingEnd */
  AnyVec[P] {
    
    override def clone() :AnyVec4[P] = throw new UnsupportedOperationException()

    final def components = 4

    private[math] def bx: Boolean
    private[math] def by: Boolean
    private[math] def bz: Boolean
    private[math] def bw: Boolean

    private[math] def ix: Int
    private[math] def iy: Int
    private[math] def iz: Int
    private[math] def iw: Int

    private[math] def fx: Float
    private[math] def fy: Float
    private[math] def fz: Float
    private[math] def fw: Float

    private[math] def dx: Double
    private[math] def dy: Double
    private[math] def dz: Double
    private[math] def dw: Double


    // @SwizzlingStart
    final def xw: C2 = make2(dx, dw)
    final def yw: C2 = make2(dy, dw)
    final def zw: C2 = make2(dz, dw)
    final def wx: C2 = make2(dw, dx)
    final def wy: C2 = make2(dw, dy)
    final def wz: C2 = make2(dw, dz)
    final def ww: C2 = make2(dw, dw)

    final def xxw: C3 = make3(dx, dx, dw)
    final def xyw: C3 = make3(dx, dy, dw)
    final def xzw: C3 = make3(dx, dz, dw)
    final def xwx: C3 = make3(dx, dw, dx)
    final def xwy: C3 = make3(dx, dw, dy)
    final def xwz: C3 = make3(dx, dw, dz)
    final def xww: C3 = make3(dx, dw, dw)
    final def yxw: C3 = make3(dy, dx, dw)
    final def yyw: C3 = make3(dy, dy, dw)
    final def yzw: C3 = make3(dy, dz, dw)
    final def ywx: C3 = make3(dy, dw, dx)
    final def ywy: C3 = make3(dy, dw, dy)
    final def ywz: C3 = make3(dy, dw, dz)
    final def yww: C3 = make3(dy, dw, dw)
    final def zxw: C3 = make3(dz, dx, dw)
    final def zyw: C3 = make3(dz, dy, dw)
    final def zzw: C3 = make3(dz, dz, dw)
    final def zwx: C3 = make3(dz, dw, dx)
    final def zwy: C3 = make3(dz, dw, dy)
    final def zwz: C3 = make3(dz, dw, dz)
    final def zww: C3 = make3(dz, dw, dw)
    final def wxx: C3 = make3(dw, dx, dx)
    final def wxy: C3 = make3(dw, dx, dy)
    final def wxz: C3 = make3(dw, dx, dz)
    final def wxw: C3 = make3(dw, dx, dw)
    final def wyx: C3 = make3(dw, dy, dx)
    final def wyy: C3 = make3(dw, dy, dy)
    final def wyz: C3 = make3(dw, dy, dz)
    final def wyw: C3 = make3(dw, dy, dw)
    final def wzx: C3 = make3(dw, dz, dx)
    final def wzy: C3 = make3(dw, dz, dy)
    final def wzz: C3 = make3(dw, dz, dz)
    final def wzw: C3 = make3(dw, dz, dw)
    final def wwx: C3 = make3(dw, dw, dx)
    final def wwy: C3 = make3(dw, dw, dy)
    final def wwz: C3 = make3(dw, dw, dz)
    final def www: C3 = make3(dw, dw, dw)

    final def xxxw: C4 = make4(dx, dx, dx, dw)
    final def xxyw: C4 = make4(dx, dx, dy, dw)
    final def xxzw: C4 = make4(dx, dx, dz, dw)
    final def xxwx: C4 = make4(dx, dx, dw, dx)
    final def xxwy: C4 = make4(dx, dx, dw, dy)
    final def xxwz: C4 = make4(dx, dx, dw, dz)
    final def xxww: C4 = make4(dx, dx, dw, dw)
    final def xyxw: C4 = make4(dx, dy, dx, dw)
    final def xyyw: C4 = make4(dx, dy, dy, dw)
    final def xyzw: C4 = make4(dx, dy, dz, dw)
    final def xywx: C4 = make4(dx, dy, dw, dx)
    final def xywy: C4 = make4(dx, dy, dw, dy)
    final def xywz: C4 = make4(dx, dy, dw, dz)
    final def xyww: C4 = make4(dx, dy, dw, dw)
    final def xzxw: C4 = make4(dx, dz, dx, dw)
    final def xzyw: C4 = make4(dx, dz, dy, dw)
    final def xzzw: C4 = make4(dx, dz, dz, dw)
    final def xzwx: C4 = make4(dx, dz, dw, dx)
    final def xzwy: C4 = make4(dx, dz, dw, dy)
    final def xzwz: C4 = make4(dx, dz, dw, dz)
    final def xzww: C4 = make4(dx, dz, dw, dw)
    final def xwxx: C4 = make4(dx, dw, dx, dx)
    final def xwxy: C4 = make4(dx, dw, dx, dy)
    final def xwxz: C4 = make4(dx, dw, dx, dz)
    final def xwxw: C4 = make4(dx, dw, dx, dw)
    final def xwyx: C4 = make4(dx, dw, dy, dx)
    final def xwyy: C4 = make4(dx, dw, dy, dy)
    final def xwyz: C4 = make4(dx, dw, dy, dz)
    final def xwyw: C4 = make4(dx, dw, dy, dw)
    final def xwzx: C4 = make4(dx, dw, dz, dx)
    final def xwzy: C4 = make4(dx, dw, dz, dy)
    final def xwzz: C4 = make4(dx, dw, dz, dz)
    final def xwzw: C4 = make4(dx, dw, dz, dw)
    final def xwwx: C4 = make4(dx, dw, dw, dx)
    final def xwwy: C4 = make4(dx, dw, dw, dy)
    final def xwwz: C4 = make4(dx, dw, dw, dz)
    final def xwww: C4 = make4(dx, dw, dw, dw)
    final def yxxw: C4 = make4(dy, dx, dx, dw)
    final def yxyw: C4 = make4(dy, dx, dy, dw)
    final def yxzw: C4 = make4(dy, dx, dz, dw)
    final def yxwx: C4 = make4(dy, dx, dw, dx)
    final def yxwy: C4 = make4(dy, dx, dw, dy)
    final def yxwz: C4 = make4(dy, dx, dw, dz)
    final def yxww: C4 = make4(dy, dx, dw, dw)
    final def yyxw: C4 = make4(dy, dy, dx, dw)
    final def yyyw: C4 = make4(dy, dy, dy, dw)
    final def yyzw: C4 = make4(dy, dy, dz, dw)
    final def yywx: C4 = make4(dy, dy, dw, dx)
    final def yywy: C4 = make4(dy, dy, dw, dy)
    final def yywz: C4 = make4(dy, dy, dw, dz)
    final def yyww: C4 = make4(dy, dy, dw, dw)
    final def yzxw: C4 = make4(dy, dz, dx, dw)
    final def yzyw: C4 = make4(dy, dz, dy, dw)
    final def yzzw: C4 = make4(dy, dz, dz, dw)
    final def yzwx: C4 = make4(dy, dz, dw, dx)
    final def yzwy: C4 = make4(dy, dz, dw, dy)
    final def yzwz: C4 = make4(dy, dz, dw, dz)
    final def yzww: C4 = make4(dy, dz, dw, dw)
    final def ywxx: C4 = make4(dy, dw, dx, dx)
    final def ywxy: C4 = make4(dy, dw, dx, dy)
    final def ywxz: C4 = make4(dy, dw, dx, dz)
    final def ywxw: C4 = make4(dy, dw, dx, dw)
    final def ywyx: C4 = make4(dy, dw, dy, dx)
    final def ywyy: C4 = make4(dy, dw, dy, dy)
    final def ywyz: C4 = make4(dy, dw, dy, dz)
    final def ywyw: C4 = make4(dy, dw, dy, dw)
    final def ywzx: C4 = make4(dy, dw, dz, dx)
    final def ywzy: C4 = make4(dy, dw, dz, dy)
    final def ywzz: C4 = make4(dy, dw, dz, dz)
    final def ywzw: C4 = make4(dy, dw, dz, dw)
    final def ywwx: C4 = make4(dy, dw, dw, dx)
    final def ywwy: C4 = make4(dy, dw, dw, dy)
    final def ywwz: C4 = make4(dy, dw, dw, dz)
    final def ywww: C4 = make4(dy, dw, dw, dw)
    final def zxxw: C4 = make4(dz, dx, dx, dw)
    final def zxyw: C4 = make4(dz, dx, dy, dw)
    final def zxzw: C4 = make4(dz, dx, dz, dw)
    final def zxwx: C4 = make4(dz, dx, dw, dx)
    final def zxwy: C4 = make4(dz, dx, dw, dy)
    final def zxwz: C4 = make4(dz, dx, dw, dz)
    final def zxww: C4 = make4(dz, dx, dw, dw)
    final def zyxw: C4 = make4(dz, dy, dx, dw)
    final def zyyw: C4 = make4(dz, dy, dy, dw)
    final def zyzw: C4 = make4(dz, dy, dz, dw)
    final def zywx: C4 = make4(dz, dy, dw, dx)
    final def zywy: C4 = make4(dz, dy, dw, dy)
    final def zywz: C4 = make4(dz, dy, dw, dz)
    final def zyww: C4 = make4(dz, dy, dw, dw)
    final def zzxw: C4 = make4(dz, dz, dx, dw)
    final def zzyw: C4 = make4(dz, dz, dy, dw)
    final def zzzw: C4 = make4(dz, dz, dz, dw)
    final def zzwx: C4 = make4(dz, dz, dw, dx)
    final def zzwy: C4 = make4(dz, dz, dw, dy)
    final def zzwz: C4 = make4(dz, dz, dw, dz)
    final def zzww: C4 = make4(dz, dz, dw, dw)
    final def zwxx: C4 = make4(dz, dw, dx, dx)
    final def zwxy: C4 = make4(dz, dw, dx, dy)
    final def zwxz: C4 = make4(dz, dw, dx, dz)
    final def zwxw: C4 = make4(dz, dw, dx, dw)
    final def zwyx: C4 = make4(dz, dw, dy, dx)
    final def zwyy: C4 = make4(dz, dw, dy, dy)
    final def zwyz: C4 = make4(dz, dw, dy, dz)
    final def zwyw: C4 = make4(dz, dw, dy, dw)
    final def zwzx: C4 = make4(dz, dw, dz, dx)
    final def zwzy: C4 = make4(dz, dw, dz, dy)
    final def zwzz: C4 = make4(dz, dw, dz, dz)
    final def zwzw: C4 = make4(dz, dw, dz, dw)
    final def zwwx: C4 = make4(dz, dw, dw, dx)
    final def zwwy: C4 = make4(dz, dw, dw, dy)
    final def zwwz: C4 = make4(dz, dw, dw, dz)
    final def zwww: C4 = make4(dz, dw, dw, dw)
    final def wxxx: C4 = make4(dw, dx, dx, dx)
    final def wxxy: C4 = make4(dw, dx, dx, dy)
    final def wxxz: C4 = make4(dw, dx, dx, dz)
    final def wxxw: C4 = make4(dw, dx, dx, dw)
    final def wxyx: C4 = make4(dw, dx, dy, dx)
    final def wxyy: C4 = make4(dw, dx, dy, dy)
    final def wxyz: C4 = make4(dw, dx, dy, dz)
    final def wxyw: C4 = make4(dw, dx, dy, dw)
    final def wxzx: C4 = make4(dw, dx, dz, dx)
    final def wxzy: C4 = make4(dw, dx, dz, dy)
    final def wxzz: C4 = make4(dw, dx, dz, dz)
    final def wxzw: C4 = make4(dw, dx, dz, dw)
    final def wxwx: C4 = make4(dw, dx, dw, dx)
    final def wxwy: C4 = make4(dw, dx, dw, dy)
    final def wxwz: C4 = make4(dw, dx, dw, dz)
    final def wxww: C4 = make4(dw, dx, dw, dw)
    final def wyxx: C4 = make4(dw, dy, dx, dx)
    final def wyxy: C4 = make4(dw, dy, dx, dy)
    final def wyxz: C4 = make4(dw, dy, dx, dz)
    final def wyxw: C4 = make4(dw, dy, dx, dw)
    final def wyyx: C4 = make4(dw, dy, dy, dx)
    final def wyyy: C4 = make4(dw, dy, dy, dy)
    final def wyyz: C4 = make4(dw, dy, dy, dz)
    final def wyyw: C4 = make4(dw, dy, dy, dw)
    final def wyzx: C4 = make4(dw, dy, dz, dx)
    final def wyzy: C4 = make4(dw, dy, dz, dy)
    final def wyzz: C4 = make4(dw, dy, dz, dz)
    final def wyzw: C4 = make4(dw, dy, dz, dw)
    final def wywx: C4 = make4(dw, dy, dw, dx)
    final def wywy: C4 = make4(dw, dy, dw, dy)
    final def wywz: C4 = make4(dw, dy, dw, dz)
    final def wyww: C4 = make4(dw, dy, dw, dw)
    final def wzxx: C4 = make4(dw, dz, dx, dx)
    final def wzxy: C4 = make4(dw, dz, dx, dy)
    final def wzxz: C4 = make4(dw, dz, dx, dz)
    final def wzxw: C4 = make4(dw, dz, dx, dw)
    final def wzyx: C4 = make4(dw, dz, dy, dx)
    final def wzyy: C4 = make4(dw, dz, dy, dy)
    final def wzyz: C4 = make4(dw, dz, dy, dz)
    final def wzyw: C4 = make4(dw, dz, dy, dw)
    final def wzzx: C4 = make4(dw, dz, dz, dx)
    final def wzzy: C4 = make4(dw, dz, dz, dy)
    final def wzzz: C4 = make4(dw, dz, dz, dz)
    final def wzzw: C4 = make4(dw, dz, dz, dw)
    final def wzwx: C4 = make4(dw, dz, dw, dx)
    final def wzwy: C4 = make4(dw, dz, dw, dy)
    final def wzwz: C4 = make4(dw, dz, dw, dz)
    final def wzww: C4 = make4(dw, dz, dw, dw)
    final def wwxx: C4 = make4(dw, dw, dx, dx)
    final def wwxy: C4 = make4(dw, dw, dx, dy)
    final def wwxz: C4 = make4(dw, dw, dx, dz)
    final def wwxw: C4 = make4(dw, dw, dx, dw)
    final def wwyx: C4 = make4(dw, dw, dy, dx)
    final def wwyy: C4 = make4(dw, dw, dy, dy)
    final def wwyz: C4 = make4(dw, dw, dy, dz)
    final def wwyw: C4 = make4(dw, dw, dy, dw)
    final def wwzx: C4 = make4(dw, dw, dz, dx)
    final def wwzy: C4 = make4(dw, dw, dz, dy)
    final def wwzz: C4 = make4(dw, dw, dz, dz)
    final def wwzw: C4 = make4(dw, dw, dz, dw)
    final def wwwx: C4 = make4(dw, dw, dw, dx)
    final def wwwy: C4 = make4(dw, dw, dw, dy)
    final def wwwz: C4 = make4(dw, dw, dw, dz)
    final def wwww: C4 = make4(dw, dw, dw, dw)

    final def ra = xw
    final def ga = yw
    final def ba = zw
    final def ar = wx
    final def ag = wy
    final def ab = wz
    final def aa = ww

    final def rra = xxw
    final def rga = xyw
    final def rba = xzw
    final def rar = xwx
    final def rag = xwy
    final def rab = xwz
    final def raa = xww
    final def gra = yxw
    final def gga = yyw
    final def gba = yzw
    final def gar = ywx
    final def gag = ywy
    final def gab = ywz
    final def gaa = yww
    final def bra = zxw
    final def bga = zyw
    final def bba = zzw
    final def bar = zwx
    final def bag = zwy
    final def bab = zwz
    final def baa = zww
    final def arr = wxx
    final def arg = wxy
    final def arb = wxz
    final def ara = wxw
    final def agr = wyx
    final def agg = wyy
    final def agb = wyz
    final def aga = wyw
    final def abr = wzx
    final def abg = wzy
    final def abb = wzz
    final def aba = wzw
    final def aar = wwx
    final def aag = wwy
    final def aab = wwz
    final def aaa = www

    final def rrra = xxxw
    final def rrga = xxyw
    final def rrba = xxzw
    final def rrar = xxwx
    final def rrag = xxwy
    final def rrab = xxwz
    final def rraa = xxww
    final def rgra = xyxw
    final def rgga = xyyw
    final def rgba = xyzw
    final def rgar = xywx
    final def rgag = xywy
    final def rgab = xywz
    final def rgaa = xyww
    final def rbra = xzxw
    final def rbga = xzyw
    final def rbba = xzzw
    final def rbar = xzwx
    final def rbag = xzwy
    final def rbab = xzwz
    final def rbaa = xzww
    final def rarr = xwxx
    final def rarg = xwxy
    final def rarb = xwxz
    final def rara = xwxw
    final def ragr = xwyx
    final def ragg = xwyy
    final def ragb = xwyz
    final def raga = xwyw
    final def rabr = xwzx
    final def rabg = xwzy
    final def rabb = xwzz
    final def raba = xwzw
    final def raar = xwwx
    final def raag = xwwy
    final def raab = xwwz
    final def raaa = xwww
    final def grra = yxxw
    final def grga = yxyw
    final def grba = yxzw
    final def grar = yxwx
    final def grag = yxwy
    final def grab = yxwz
    final def graa = yxww
    final def ggra = yyxw
    final def ggga = yyyw
    final def ggba = yyzw
    final def ggar = yywx
    final def ggag = yywy
    final def ggab = yywz
    final def ggaa = yyww
    final def gbra = yzxw
    final def gbga = yzyw
    final def gbba = yzzw
    final def gbar = yzwx
    final def gbag = yzwy
    final def gbab = yzwz
    final def gbaa = yzww
    final def garr = ywxx
    final def garg = ywxy
    final def garb = ywxz
    final def gara = ywxw
    final def gagr = ywyx
    final def gagg = ywyy
    final def gagb = ywyz
    final def gaga = ywyw
    final def gabr = ywzx
    final def gabg = ywzy
    final def gabb = ywzz
    final def gaba = ywzw
    final def gaar = ywwx
    final def gaag = ywwy
    final def gaab = ywwz
    final def gaaa = ywww
    final def brra = zxxw
    final def brga = zxyw
    final def brba = zxzw
    final def brar = zxwx
    final def brag = zxwy
    final def brab = zxwz
    final def braa = zxww
    final def bgra = zyxw
    final def bgga = zyyw
    final def bgba = zyzw
    final def bgar = zywx
    final def bgag = zywy
    final def bgab = zywz
    final def bgaa = zyww
    final def bbra = zzxw
    final def bbga = zzyw
    final def bbba = zzzw
    final def bbar = zzwx
    final def bbag = zzwy
    final def bbab = zzwz
    final def bbaa = zzww
    final def barr = zwxx
    final def barg = zwxy
    final def barb = zwxz
    final def bara = zwxw
    final def bagr = zwyx
    final def bagg = zwyy
    final def bagb = zwyz
    final def baga = zwyw
    final def babr = zwzx
    final def babg = zwzy
    final def babb = zwzz
    final def baba = zwzw
    final def baar = zwwx
    final def baag = zwwy
    final def baab = zwwz
    final def baaa = zwww
    final def arrr = wxxx
    final def arrg = wxxy
    final def arrb = wxxz
    final def arra = wxxw
    final def argr = wxyx
    final def argg = wxyy
    final def argb = wxyz
    final def arga = wxyw
    final def arbr = wxzx
    final def arbg = wxzy
    final def arbb = wxzz
    final def arba = wxzw
    final def arar = wxwx
    final def arag = wxwy
    final def arab = wxwz
    final def araa = wxww
    final def agrr = wyxx
    final def agrg = wyxy
    final def agrb = wyxz
    final def agra = wyxw
    final def aggr = wyyx
    final def aggg = wyyy
    final def aggb = wyyz
    final def agga = wyyw
    final def agbr = wyzx
    final def agbg = wyzy
    final def agbb = wyzz
    final def agba = wyzw
    final def agar = wywx
    final def agag = wywy
    final def agab = wywz
    final def agaa = wyww
    final def abrr = wzxx
    final def abrg = wzxy
    final def abrb = wzxz
    final def abra = wzxw
    final def abgr = wzyx
    final def abgg = wzyy
    final def abgb = wzyz
    final def abga = wzyw
    final def abbr = wzzx
    final def abbg = wzzy
    final def abbb = wzzz
    final def abba = wzzw
    final def abar = wzwx
    final def abag = wzwy
    final def abab = wzwz
    final def abaa = wzww
    final def aarr = wwxx
    final def aarg = wwxy
    final def aarb = wwxz
    final def aara = wwxw
    final def aagr = wwyx
    final def aagg = wwyy
    final def aagb = wwyz
    final def aaga = wwyw
    final def aabr = wwzx
    final def aabg = wwzy
    final def aabb = wwzz
    final def aaba = wwzw
    final def aaar = wwwx
    final def aaag = wwwy
    final def aaab = wwwz
    final def aaaa = wwww

    final def sq = xw
    final def tq = yw
    final def pq = zw
    final def qs = wx
    final def qt = wy
    final def qp = wz
    final def qq = ww

    final def ssq = xxw
    final def stq = xyw
    final def spq = xzw
    final def sqs = xwx
    final def sqt = xwy
    final def sqp = xwz
    final def sqq = xww
    final def tsq = yxw
    final def ttq = yyw
    final def tpq = yzw
    final def tqs = ywx
    final def tqt = ywy
    final def tqp = ywz
    final def tqq = yww
    final def psq = zxw
    final def ptq = zyw
    final def ppq = zzw
    final def pqs = zwx
    final def pqt = zwy
    final def pqp = zwz
    final def pqq = zww
    final def qss = wxx
    final def qst = wxy
    final def qsp = wxz
    final def qsq = wxw
    final def qts = wyx
    final def qtt = wyy
    final def qtp = wyz
    final def qtq = wyw
    final def qps = wzx
    final def qpt = wzy
    final def qpp = wzz
    final def qpq = wzw
    final def qqs = wwx
    final def qqt = wwy
    final def qqp = wwz
    final def qqq = www

    final def sssq = xxxw
    final def sstq = xxyw
    final def sspq = xxzw
    final def ssqs = xxwx
    final def ssqt = xxwy
    final def ssqp = xxwz
    final def ssqq = xxww
    final def stsq = xyxw
    final def sttq = xyyw
    final def stpq = xyzw
    final def stqs = xywx
    final def stqt = xywy
    final def stqp = xywz
    final def stqq = xyww
    final def spsq = xzxw
    final def sptq = xzyw
    final def sppq = xzzw
    final def spqs = xzwx
    final def spqt = xzwy
    final def spqp = xzwz
    final def spqq = xzww
    final def sqss = xwxx
    final def sqst = xwxy
    final def sqsp = xwxz
    final def sqsq = xwxw
    final def sqts = xwyx
    final def sqtt = xwyy
    final def sqtp = xwyz
    final def sqtq = xwyw
    final def sqps = xwzx
    final def sqpt = xwzy
    final def sqpp = xwzz
    final def sqpq = xwzw
    final def sqqs = xwwx
    final def sqqt = xwwy
    final def sqqp = xwwz
    final def sqqq = xwww
    final def tssq = yxxw
    final def tstq = yxyw
    final def tspq = yxzw
    final def tsqs = yxwx
    final def tsqt = yxwy
    final def tsqp = yxwz
    final def tsqq = yxww
    final def ttsq = yyxw
    final def tttq = yyyw
    final def ttpq = yyzw
    final def ttqs = yywx
    final def ttqt = yywy
    final def ttqp = yywz
    final def ttqq = yyww
    final def tpsq = yzxw
    final def tptq = yzyw
    final def tppq = yzzw
    final def tpqs = yzwx
    final def tpqt = yzwy
    final def tpqp = yzwz
    final def tpqq = yzww
    final def tqss = ywxx
    final def tqst = ywxy
    final def tqsp = ywxz
    final def tqsq = ywxw
    final def tqts = ywyx
    final def tqtt = ywyy
    final def tqtp = ywyz
    final def tqtq = ywyw
    final def tqps = ywzx
    final def tqpt = ywzy
    final def tqpp = ywzz
    final def tqpq = ywzw
    final def tqqs = ywwx
    final def tqqt = ywwy
    final def tqqp = ywwz
    final def tqqq = ywww
    final def pssq = zxxw
    final def pstq = zxyw
    final def pspq = zxzw
    final def psqs = zxwx
    final def psqt = zxwy
    final def psqp = zxwz
    final def psqq = zxww
    final def ptsq = zyxw
    final def pttq = zyyw
    final def ptpq = zyzw
    final def ptqs = zywx
    final def ptqt = zywy
    final def ptqp = zywz
    final def ptqq = zyww
    final def ppsq = zzxw
    final def pptq = zzyw
    final def pppq = zzzw
    final def ppqs = zzwx
    final def ppqt = zzwy
    final def ppqp = zzwz
    final def ppqq = zzww
    final def pqss = zwxx
    final def pqst = zwxy
    final def pqsp = zwxz
    final def pqsq = zwxw
    final def pqts = zwyx
    final def pqtt = zwyy
    final def pqtp = zwyz
    final def pqtq = zwyw
    final def pqps = zwzx
    final def pqpt = zwzy
    final def pqpp = zwzz
    final def pqpq = zwzw
    final def pqqs = zwwx
    final def pqqt = zwwy
    final def pqqp = zwwz
    final def pqqq = zwww
    final def qsss = wxxx
    final def qsst = wxxy
    final def qssp = wxxz
    final def qssq = wxxw
    final def qsts = wxyx
    final def qstt = wxyy
    final def qstp = wxyz
    final def qstq = wxyw
    final def qsps = wxzx
    final def qspt = wxzy
    final def qspp = wxzz
    final def qspq = wxzw
    final def qsqs = wxwx
    final def qsqt = wxwy
    final def qsqp = wxwz
    final def qsqq = wxww
    final def qtss = wyxx
    final def qtst = wyxy
    final def qtsp = wyxz
    final def qtsq = wyxw
    final def qtts = wyyx
    final def qttt = wyyy
    final def qttp = wyyz
    final def qttq = wyyw
    final def qtps = wyzx
    final def qtpt = wyzy
    final def qtpp = wyzz
    final def qtpq = wyzw
    final def qtqs = wywx
    final def qtqt = wywy
    final def qtqp = wywz
    final def qtqq = wyww
    final def qpss = wzxx
    final def qpst = wzxy
    final def qpsp = wzxz
    final def qpsq = wzxw
    final def qpts = wzyx
    final def qptt = wzyy
    final def qptp = wzyz
    final def qptq = wzyw
    final def qpps = wzzx
    final def qppt = wzzy
    final def qppp = wzzz
    final def qppq = wzzw
    final def qpqs = wzwx
    final def qpqt = wzwy
    final def qpqp = wzwz
    final def qpqq = wzww
    final def qqss = wwxx
    final def qqst = wwxy
    final def qqsp = wwxz
    final def qqsq = wwxw
    final def qqts = wwyx
    final def qqtt = wwyy
    final def qqtp = wwyz
    final def qqtq = wwyw
    final def qqps = wwzx
    final def qqpt = wwzy
    final def qqpp = wwzz
    final def qqpq = wwzw
    final def qqqs = wwwx
    final def qqqt = wwwy
    final def qqqp = wwwz
    final def qqqq = wwww


    protected def xw_=(u: R2) { throw new UnsupportedOperationException }
    protected def yw_=(u: R2) { throw new UnsupportedOperationException }
    protected def zw_=(u: R2) { throw new UnsupportedOperationException }
    protected def wx_=(u: R2) { throw new UnsupportedOperationException }
    protected def wy_=(u: R2) { throw new UnsupportedOperationException }
    protected def wz_=(u: R2) { throw new UnsupportedOperationException }

    protected def xyw_=(u: R3) { throw new UnsupportedOperationException }
    protected def xzw_=(u: R3) { throw new UnsupportedOperationException }
    protected def xwy_=(u: R3) { throw new UnsupportedOperationException }
    protected def xwz_=(u: R3) { throw new UnsupportedOperationException }
    protected def yxw_=(u: R3) { throw new UnsupportedOperationException }
    protected def yzw_=(u: R3) { throw new UnsupportedOperationException }
    protected def ywx_=(u: R3) { throw new UnsupportedOperationException }
    protected def ywz_=(u: R3) { throw new UnsupportedOperationException }
    protected def zxw_=(u: R3) { throw new UnsupportedOperationException }
    protected def zyw_=(u: R3) { throw new UnsupportedOperationException }
    protected def zwx_=(u: R3) { throw new UnsupportedOperationException }
    protected def zwy_=(u: R3) { throw new UnsupportedOperationException }
    protected def wxy_=(u: R3) { throw new UnsupportedOperationException }
    protected def wxz_=(u: R3) { throw new UnsupportedOperationException }
    protected def wyx_=(u: R3) { throw new UnsupportedOperationException }
    protected def wyz_=(u: R3) { throw new UnsupportedOperationException }
    protected def wzx_=(u: R3) { throw new UnsupportedOperationException }
    protected def wzy_=(u: R3) { throw new UnsupportedOperationException }

    protected def xyzw_=(u: R4) { throw new UnsupportedOperationException }
    protected def xywz_=(u: R4) { throw new UnsupportedOperationException }
    protected def xzyw_=(u: R4) { throw new UnsupportedOperationException }
    protected def xzwy_=(u: R4) { throw new UnsupportedOperationException }
    protected def xwyz_=(u: R4) { throw new UnsupportedOperationException }
    protected def xwzy_=(u: R4) { throw new UnsupportedOperationException }
    protected def yxzw_=(u: R4) { throw new UnsupportedOperationException }
    protected def yxwz_=(u: R4) { throw new UnsupportedOperationException }
    protected def yzxw_=(u: R4) { throw new UnsupportedOperationException }
    protected def yzwx_=(u: R4) { throw new UnsupportedOperationException }
    protected def ywxz_=(u: R4) { throw new UnsupportedOperationException }
    protected def ywzx_=(u: R4) { throw new UnsupportedOperationException }
    protected def zxyw_=(u: R4) { throw new UnsupportedOperationException }
    protected def zxwy_=(u: R4) { throw new UnsupportedOperationException }
    protected def zyxw_=(u: R4) { throw new UnsupportedOperationException }
    protected def zywx_=(u: R4) { throw new UnsupportedOperationException }
    protected def zwxy_=(u: R4) { throw new UnsupportedOperationException }
    protected def zwyx_=(u: R4) { throw new UnsupportedOperationException }
    protected def wxyz_=(u: R4) { throw new UnsupportedOperationException }
    protected def wxzy_=(u: R4) { throw new UnsupportedOperationException }
    protected def wyxz_=(u: R4) { throw new UnsupportedOperationException }
    protected def wyzx_=(u: R4) { throw new UnsupportedOperationException }
    protected def wzxy_=(u: R4) { throw new UnsupportedOperationException }
    protected def wzyx_=(u: R4) { throw new UnsupportedOperationException }

    protected def ra_=(u: R2) { throw new UnsupportedOperationException }
    protected def ga_=(u: R2) { throw new UnsupportedOperationException }
    protected def ba_=(u: R2) { throw new UnsupportedOperationException }
    protected def ar_=(u: R2) { throw new UnsupportedOperationException }
    protected def ag_=(u: R2) { throw new UnsupportedOperationException }
    protected def ab_=(u: R2) { throw new UnsupportedOperationException }

    protected def rga_=(u: R3) { throw new UnsupportedOperationException }
    protected def rba_=(u: R3) { throw new UnsupportedOperationException }
    protected def rag_=(u: R3) { throw new UnsupportedOperationException }
    protected def rab_=(u: R3) { throw new UnsupportedOperationException }
    protected def gra_=(u: R3) { throw new UnsupportedOperationException }
    protected def gba_=(u: R3) { throw new UnsupportedOperationException }
    protected def gar_=(u: R3) { throw new UnsupportedOperationException }
    protected def gab_=(u: R3) { throw new UnsupportedOperationException }
    protected def bra_=(u: R3) { throw new UnsupportedOperationException }
    protected def bga_=(u: R3) { throw new UnsupportedOperationException }
    protected def bar_=(u: R3) { throw new UnsupportedOperationException }
    protected def bag_=(u: R3) { throw new UnsupportedOperationException }
    protected def arg_=(u: R3) { throw new UnsupportedOperationException }
    protected def arb_=(u: R3) { throw new UnsupportedOperationException }
    protected def agr_=(u: R3) { throw new UnsupportedOperationException }
    protected def agb_=(u: R3) { throw new UnsupportedOperationException }
    protected def abr_=(u: R3) { throw new UnsupportedOperationException }
    protected def abg_=(u: R3) { throw new UnsupportedOperationException }

    protected def rgba_=(u: R4) { throw new UnsupportedOperationException }
    protected def rgab_=(u: R4) { throw new UnsupportedOperationException }
    protected def rbga_=(u: R4) { throw new UnsupportedOperationException }
    protected def rbag_=(u: R4) { throw new UnsupportedOperationException }
    protected def ragb_=(u: R4) { throw new UnsupportedOperationException }
    protected def rabg_=(u: R4) { throw new UnsupportedOperationException }
    protected def grba_=(u: R4) { throw new UnsupportedOperationException }
    protected def grab_=(u: R4) { throw new UnsupportedOperationException }
    protected def gbra_=(u: R4) { throw new UnsupportedOperationException }
    protected def gbar_=(u: R4) { throw new UnsupportedOperationException }
    protected def garb_=(u: R4) { throw new UnsupportedOperationException }
    protected def gabr_=(u: R4) { throw new UnsupportedOperationException }
    protected def brga_=(u: R4) { throw new UnsupportedOperationException }
    protected def brag_=(u: R4) { throw new UnsupportedOperationException }
    protected def bgra_=(u: R4) { throw new UnsupportedOperationException }
    protected def bgar_=(u: R4) { throw new UnsupportedOperationException }
    protected def barg_=(u: R4) { throw new UnsupportedOperationException }
    protected def bagr_=(u: R4) { throw new UnsupportedOperationException }
    protected def argb_=(u: R4) { throw new UnsupportedOperationException }
    protected def arbg_=(u: R4) { throw new UnsupportedOperationException }
    protected def agrb_=(u: R4) { throw new UnsupportedOperationException }
    protected def agbr_=(u: R4) { throw new UnsupportedOperationException }
    protected def abrg_=(u: R4) { throw new UnsupportedOperationException }
    protected def abgr_=(u: R4) { throw new UnsupportedOperationException }

    protected def sq_=(u: R2) { throw new UnsupportedOperationException }
    protected def tq_=(u: R2) { throw new UnsupportedOperationException }
    protected def pq_=(u: R2) { throw new UnsupportedOperationException }
    protected def qs_=(u: R2) { throw new UnsupportedOperationException }
    protected def qt_=(u: R2) { throw new UnsupportedOperationException }
    protected def qp_=(u: R2) { throw new UnsupportedOperationException }

    protected def stq_=(u: R3) { throw new UnsupportedOperationException }
    protected def spq_=(u: R3) { throw new UnsupportedOperationException }
    protected def sqt_=(u: R3) { throw new UnsupportedOperationException }
    protected def sqp_=(u: R3) { throw new UnsupportedOperationException }
    protected def tsq_=(u: R3) { throw new UnsupportedOperationException }
    protected def tpq_=(u: R3) { throw new UnsupportedOperationException }
    protected def tqs_=(u: R3) { throw new UnsupportedOperationException }
    protected def tqp_=(u: R3) { throw new UnsupportedOperationException }
    protected def psq_=(u: R3) { throw new UnsupportedOperationException }
    protected def ptq_=(u: R3) { throw new UnsupportedOperationException }
    protected def pqs_=(u: R3) { throw new UnsupportedOperationException }
    protected def pqt_=(u: R3) { throw new UnsupportedOperationException }
    protected def qst_=(u: R3) { throw new UnsupportedOperationException }
    protected def qsp_=(u: R3) { throw new UnsupportedOperationException }
    protected def qts_=(u: R3) { throw new UnsupportedOperationException }
    protected def qtp_=(u: R3) { throw new UnsupportedOperationException }
    protected def qps_=(u: R3) { throw new UnsupportedOperationException }
    protected def qpt_=(u: R3) { throw new UnsupportedOperationException }

    protected def stpq_=(u: R4) { throw new UnsupportedOperationException }
    protected def stqp_=(u: R4) { throw new UnsupportedOperationException }
    protected def sptq_=(u: R4) { throw new UnsupportedOperationException }
    protected def spqt_=(u: R4) { throw new UnsupportedOperationException }
    protected def sqtp_=(u: R4) { throw new UnsupportedOperationException }
    protected def sqpt_=(u: R4) { throw new UnsupportedOperationException }
    protected def tspq_=(u: R4) { throw new UnsupportedOperationException }
    protected def tsqp_=(u: R4) { throw new UnsupportedOperationException }
    protected def tpsq_=(u: R4) { throw new UnsupportedOperationException }
    protected def tpqs_=(u: R4) { throw new UnsupportedOperationException }
    protected def tqsp_=(u: R4) { throw new UnsupportedOperationException }
    protected def tqps_=(u: R4) { throw new UnsupportedOperationException }
    protected def pstq_=(u: R4) { throw new UnsupportedOperationException }
    protected def psqt_=(u: R4) { throw new UnsupportedOperationException }
    protected def ptsq_=(u: R4) { throw new UnsupportedOperationException }
    protected def ptqs_=(u: R4) { throw new UnsupportedOperationException }
    protected def pqst_=(u: R4) { throw new UnsupportedOperationException }
    protected def pqts_=(u: R4) { throw new UnsupportedOperationException }
    protected def qstp_=(u: R4) { throw new UnsupportedOperationException }
    protected def qspt_=(u: R4) { throw new UnsupportedOperationException }
    protected def qtsp_=(u: R4) { throw new UnsupportedOperationException }
    protected def qtps_=(u: R4) { throw new UnsupportedOperationException }
    protected def qpst_=(u: R4) { throw new UnsupportedOperationException }
    protected def qpts_=(u: R4) { throw new UnsupportedOperationException }
    // @SwizzlingEnd
  }
}
