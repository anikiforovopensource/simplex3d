/*
 * Simplex3d, MathTest package
 * Copyright (C) 2009-2010 Simplex3d Team
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

package test.math.floatm

import org.scalatest._

import simplex3d.math.floatm.renamed._
import simplex3d.math.floatm.FloatMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
class FloatMathMatTest extends FunSuite {

    test("Inverse") {
        val m2 = Mat2(2, 4, 5, 3)
        val m2i = inverse(inverse(m2))
        assert(!hasErrors(m2i))
        assert(approxEqual(m2, m2i, 1e-6f))

        val m23 = Mat2x3(2, 4, 5, 3, 5, 3)
        val m23i = inverse(inverse(m23))
        assert(!hasErrors(m23i))
        assert(approxEqual(m23, m23i, 1e-6f))

        val m3 = Mat3(2, 4, 5, 3, 3, 6, 4, 3, 2)
        val m3i = inverse(inverse(m3))
        assert(!hasErrors(m3i))
        assert(approxEqual(m3, m3i, 1e-6f))

        val m34 = Mat3x4(2, 4, 5, 3, 3, 6, 4, 3, 2, 6, 2, 4)
        val m34i = inverse(inverse(m34))
        assert(!hasErrors(m34i))
        assert(approxEqual(m34, m34i, 1e-6f))

        val m4 = Mat4(2, 4, 5, 3, 3, 6, 4, 3, 2, 3, 6, 4, 8, 4, 3, 6)
        val m4i = inverse(inverse(m4))
        assert(!hasErrors(m4i))
        assert(approxEqual(m4, m4i, 1e-5f))
    }
}
