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

import simplex3d.math.BaseMath._


/** <code>Read2</code> is a superclass of all the 2-dimensional vectors.
 * <p>
 *   There are double, float, int, and boolean vectors.
 * </p>
 *
 * @author Aleksey Nikiforov (lex)
 */
private[math] abstract class Read2[T] extends Swizzle2Read[T] {
  final override def size = 2

  private[math] def bx: Boolean
  private[math] def by: Boolean

  private[math] def ix: Int
  private[math] def iy: Int

  private[math] def fx: Float
  private[math] def fy: Float

  private[math] def dx: Double
  private[math] def dy: Double
}

/** <code>Read3</code> is a superclass of all the 3-dimensional vectors.
 * <p>
 *   There are double, float, int, and boolean vectors.
 * </p>
 *
 * @author Aleksey Nikiforov (lex)
 */
private[math] abstract class Read3[T] extends Swizzle3Read[T] {
  final override def size = 3

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

/** <code>Read4</code> is a superclass of all the 4-dimensional vectors.
 * <p>
 *   There are double, float, int, and boolean vectors.
 * </p>
 *
 * @author Aleksey Nikiforov (lex)
 */
private[math] abstract class Read4[T] extends Swizzle4Read[T] {
  final override def size = 4

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
}

/** <code>ReadQ</code> is a superclass of all the quaternions.
 * <p>
 *   There are double and float quaternions.
 * </p>
 *
 * @author Aleksey Nikiforov (lex)
 */
private[math] abstract class ReadQ[T] extends AnyQuat[T] {
  final override def size = 4

  private[math] def fa: Float
  private[math] def fb: Float
  private[math] def fc: Float
  private[math] def fd: Float

  private[math] def da: Double
  private[math] def db: Double
  private[math] def dc: Double
  private[math] def dd: Double
}

/** <code>Read2x2</code> is a superclass of all the 2x2 matrices.
 * <p>
 *   There are double and float matrices.
 * </p>
 *
 * @author Aleksey Nikiforov (lex)
 */
private[math] abstract class Read2x2[V <: ConstVec[_]] extends AnyMat[V] {
  final override def size :Int = 2

  private[math] def f00: Float; private[math] def f10: Float
  private[math] def f01: Float; private[math] def f11: Float

  private[math] def d00: Double; private[math] def d10: Double
  private[math] def d01: Double; private[math] def d11: Double
}

/** <code>Read2x3</code> is a superclass of all the 2x3 matrices.
 * <p>
 *   There are double and float matrices.
 * </p>
 *
 * @author Aleksey Nikiforov (lex)
 */
private[math] abstract class Read2x3[V <: ConstVec[_]] extends AnyMat[V] {
  final override def size :Int = 3

  private[math] def f00: Float; private[math] def f10: Float
  private[math] def f01: Float; private[math] def f11: Float
  private[math] def f02: Float; private[math] def f12: Float

  private[math] def d00: Double; private[math] def d10: Double
  private[math] def d01: Double; private[math] def d11: Double
  private[math] def d02: Double; private[math] def d12: Double
}

/** <code>Read2x4</code> is a superclass of all the 2x4 matrices.
 * <p>
 *   There are double and float matrices.
 * </p>
 *
 * @author Aleksey Nikiforov (lex)
 */
private[math] abstract class Read2x4[V <: ConstVec[_]] extends AnyMat[V] {
  final override def size :Int = 4

  private[math] def f00: Float; private[math] def f10: Float
  private[math] def f01: Float; private[math] def f11: Float
  private[math] def f02: Float; private[math] def f12: Float
  private[math] def f03: Float; private[math] def f13: Float

  private[math] def d00: Double; private[math] def d10: Double
  private[math] def d01: Double; private[math] def d11: Double
  private[math] def d02: Double; private[math] def d12: Double
  private[math] def d03: Double; private[math] def d13: Double
}

/** <code>Read3x2</code> is a superclass of all the 3x2 matrices.
 * <p>
 *   There are double and float matrices.
 * </p>
 *
 * @author Aleksey Nikiforov (lex)
 */
private[math] abstract class Read3x2[V <: ConstVec[_]] extends AnyMat[V] {
  final override def size :Int = 2

  private[math] def f00: Float
  private[math] def f10: Float
  private[math] def f20: Float

  private[math] def f01: Float
  private[math] def f11: Float
  private[math] def f21: Float


  private[math] def d00: Double
  private[math] def d10: Double
  private[math] def d20: Double

  private[math] def d01: Double
  private[math] def d11: Double
  private[math] def d21: Double
}

/** <code>Read3x3</code> is a superclass of all the 3x3 matrices.
 * <p>
 *   There are double and float matrices.
 * </p>
 *
 * @author Aleksey Nikiforov (lex)
 */
private[math] abstract class Read3x3[V <: ConstVec[_]] extends AnyMat[V] {
  final override def size :Int = 3

  private[math] def f00: Float
  private[math] def f10: Float
  private[math] def f20: Float

  private[math] def f01: Float
  private[math] def f11: Float
  private[math] def f21: Float

  private[math] def f02: Float
  private[math] def f12: Float
  private[math] def f22: Float


  private[math] def d00: Double
  private[math] def d10: Double
  private[math] def d20: Double

  private[math] def d01: Double
  private[math] def d11: Double
  private[math] def d21: Double

  private[math] def d02: Double
  private[math] def d12: Double
  private[math] def d22: Double
}

/** <code>Read3x4</code> is a superclass of all the 3x4 matrices.
 * <p>
 *   There are double and float matrices.
 * </p>
 *
 * @author Aleksey Nikiforov (lex)
 */
private[math] abstract class Read3x4[V <: ConstVec[_]] extends AnyMat[V] {
  final override def size :Int = 4

  private[math] def f00: Float
  private[math] def f10: Float
  private[math] def f20: Float

  private[math] def f01: Float
  private[math] def f11: Float
  private[math] def f21: Float

  private[math] def f02: Float
  private[math] def f12: Float
  private[math] def f22: Float

  private[math] def f03: Float
  private[math] def f13: Float
  private[math] def f23: Float


  private[math] def d00: Double
  private[math] def d10: Double
  private[math] def d20: Double

  private[math] def d01: Double
  private[math] def d11: Double
  private[math] def d21: Double

  private[math] def d02: Double
  private[math] def d12: Double
  private[math] def d22: Double

  private[math] def d03: Double
  private[math] def d13: Double
  private[math] def d23: Double
}

/** <code>Read4x2</code> is a superclass of all the 4x2 matrices.
 * <p>
 *   There are double and float matrices.
 * </p>
 *
 * @author Aleksey Nikiforov (lex)
 */
private[math] abstract class Read4x2[V <: ConstVec[_]] extends AnyMat[V] {
  final override def size :Int = 2

  private[math] def f00: Float
  private[math] def f10: Float
  private[math] def f20: Float
  private[math] def f30: Float

  private[math] def f01: Float
  private[math] def f11: Float
  private[math] def f21: Float
  private[math] def f31: Float


  private[math] def d00: Double
  private[math] def d10: Double
  private[math] def d20: Double
  private[math] def d30: Double

  private[math] def d01: Double
  private[math] def d11: Double
  private[math] def d21: Double
  private[math] def d31: Double
}

/** <code>Read4x3</code> is a superclass of all the 4x3 matrices.
 * <p>
 *   There are double and float matrices.
 * </p>
 *
 * @author Aleksey Nikiforov (lex)
 */
private[math] abstract class Read4x3[V <: ConstVec[_]] extends AnyMat[V] {
  final override def size :Int = 3

  private[math] def f00: Float
  private[math] def f10: Float
  private[math] def f20: Float
  private[math] def f30: Float

  private[math] def f01: Float
  private[math] def f11: Float
  private[math] def f21: Float
  private[math] def f31: Float

  private[math] def f02: Float
  private[math] def f12: Float
  private[math] def f22: Float
  private[math] def f32: Float


  private[math] def d00: Double
  private[math] def d10: Double
  private[math] def d20: Double
  private[math] def d30: Double

  private[math] def d01: Double
  private[math] def d11: Double
  private[math] def d21: Double
  private[math] def d31: Double

  private[math] def d02: Double
  private[math] def d12: Double
  private[math] def d22: Double
  private[math] def d32: Double
}

/** <code>Read4x4</code> is a superclass of all the 4x4 matrices.
 * <p>
 *   There are double and float matrices.
 * </p>
 *
 * @author Aleksey Nikiforov (lex)
 */
private[math] abstract class Read4x4[V <: ConstVec[_]] extends AnyMat[V] {
  final override def size :Int = 4

  private[math] def f00: Float
  private[math] def f10: Float
  private[math] def f20: Float
  private[math] def f30: Float

  private[math] def f01: Float
  private[math] def f11: Float
  private[math] def f21: Float
  private[math] def f31: Float

  private[math] def f02: Float
  private[math] def f12: Float
  private[math] def f22: Float
  private[math] def f32: Float

  private[math] def f03: Float
  private[math] def f13: Float
  private[math] def f23: Float
  private[math] def f33: Float


  private[math] def d00: Double
  private[math] def d10: Double
  private[math] def d20: Double
  private[math] def d30: Double

  private[math] def d01: Double
  private[math] def d11: Double
  private[math] def d21: Double
  private[math] def d31: Double

  private[math] def d02: Double
  private[math] def d12: Double
  private[math] def d22: Double
  private[math] def d32: Double

  private[math] def d03: Double
  private[math] def d13: Double
  private[math] def d23: Double
  private[math] def d33: Double
}
