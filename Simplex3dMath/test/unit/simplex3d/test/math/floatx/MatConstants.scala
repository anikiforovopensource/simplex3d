/*
 * Simplex3dMath - Test Package
 * Copyright (C) 2010-2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dMathTest.
 *
 * Simplex3dMathTest is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dMathTest is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.test.math.floatx

import simplex3d.math.float._


/**
 * @author Aleksey Nikiforov (lex)
 */
object MatConstants {
  val (m00, m10, m20, m30) = (1f, 2f, 3f, 4f)
  val (m01, m11, m21, m31) = (5f, 6f, 7f, 8f)
  val (m02, m12, m22, m32) = (9f, 10f, 11f, 12f)
  val (m03, m13, m23, m33) = (13f, 14f, 15f, 16f)

  val (f00, f10, f20, f30) = (1f+1e-5f, 2f+1e-5f, 3f+1e-5f, 4f+1e-5f)
  val (f01, f11, f21, f31) = (5f+1e-5f, 6f+1e-5f, 7f+1e-5f, 8f+1e-5f)
  val (f02, f12, f22, f32) = (9f+1e-5f, 10f+1e-5f, 11f+1e-5f, 12f+1e-5f)
  val (f03, f13, f23, f33) = (13f+1e-5f, 14f+1e-5f, 15f+1e-5f, 16f+1e-5f)

  val (d00, d10, d20, d30) = (1+1e-5, 2+1e-5, 3+1e-5, 4+1e-5)
  val (d01, d11, d21, d31) = (5+1e-5, 6+1e-5, 7+1e-5, 8+1e-5)
  val (d02, d12, d22, d32) = (9+1e-5, 10+1e-5, 11+1e-5, 12+1e-5)
  val (d03, d13, d23, d33) = (13+1e-5, 14+1e-5, 15+1e-5, 16+1e-5)

  val M = ConstMat4(
    m00, m10, m20, m30,
    m01, m11, m21, m31,
    m02, m12, m22, m32,
    m03, m13, m23, m33
  )
}
