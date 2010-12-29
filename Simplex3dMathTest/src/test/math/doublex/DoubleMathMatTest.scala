/*
 * Simplex3d, MathTest package
 * Copyright (C) 2009-2010, Simplex3d Team
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

package test.math.doublex

import org.scalatest._

import simplex3d.math.double._
import simplex3d.math.doublex.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
class DoubleMathMatTest extends FunSuite {

  test("Matrix Math") {
    assert(
      matrixCompMult(
        Mat2x2(
          1, 2,
          3, 4
        ),
        Mat2x2(
          2, 3,
          4, 5
        )
      ) == Mat2x2(
        2, 6,
        12, 20
      )
    )

    assert(
      matrixCompMult(
        Mat2x3(
          1, 2,
          3, 4,
          5, 6
        ),
        Mat2x3(
          2, 3,
          4, 5,
          6, 7
        )
      ) == Mat2x3(
        2, 6,
        12, 20,
        30, 42
      )
    )

    assert(
      matrixCompMult(
        Mat2x4(
          1, 2,
          3, 4,
          5, 6,
          7, 8),
        Mat2x4(
          2, 3,
          4, 5,
          6, 7,
          8, 9
        )
      ) == Mat2x4(
        2, 6,
        12, 20,
        30, 42,
        56, 72
      )
    )

    assert(
      matrixCompMult(
        Mat3x2(
          1, 2, 3,
          4, 5, 6
        ),
        Mat3x2(
          2, 3, 4,
          5, 6, 7
        )
      ) == Mat3x2(
        2, 6, 12,
        20, 30, 42
      )
    )

    assert(
      matrixCompMult(
        Mat3x3(
          1, 2, 3,
          4, 5, 6,
          7, 8, 9
        ),
        Mat3x3(
          2, 3, 4,
          5, 6, 7,
          8, 9, 10
        )
      ) == Mat3x3(
        2, 6, 12,
        20, 30, 42,
        56, 72, 90
      )
    )

    assert(
      matrixCompMult(
        Mat3x4(
          1, 2, 3,
          4, 5, 6,
          7, 8, 9,
          10, 11, 12
        ),
        Mat3x4(
          2, 3, 4,
          5, 6, 7,
          8, 9, 10,
          11, 12, 13
        )
      ) == Mat3x4(
        2, 6, 12,
        20, 30, 42,
        56, 72, 90,
        110, 132, 156
      )
    )

    assert(
      matrixCompMult(
        Mat4x2(
          1, 2, 3, 4,
          5, 6, 7, 8
        ),
        Mat4x2(
          2, 3, 4, 5,
          6, 7, 8, 9
        )
      ) == Mat4x2(
        2, 6, 12, 20,
        30, 42, 56, 72
      )
    )

    assert(
      matrixCompMult(
        Mat4x3(
          1, 2, 3, 4,
          5, 6, 7, 8,
          9, 10, 11, 12),
        Mat4x3(
          2, 3, 4, 5,
          6, 7, 8, 9,
          10, 11, 12, 13
        )
      ) == Mat4x3(
        2, 6, 12, 20,
        30, 42, 56, 72,
        90, 110, 132, 156
      )
    )

    assert(
      matrixCompMult(
        Mat4x4(
          1, 2, 3, 4,
          5, 6, 7, 8,
          9, 10, 11, 12,
          13, 14, 15, 16
        ),
        Mat4x4(
          2, 3, 4, 5,
          6, 7, 8, 9,
          10, 11, 12, 13,
          14, 15, 16, 17
        )
      ) == Mat4x4(
        2, 6, 12, 20,
        30, 42, 56, 72,
        90, 110, 132, 156,
        182, 210, 240, 272
      )
    )

    assert(outerProduct(Vec2(1, 2), Vec2(2, 3)) == Mat2x2(
        2, 4,
        3, 6
    ))

    assert(outerProduct(Vec2(1, 2), Vec3(2, 3, 4)) == Mat2x3(
        2, 4,
        3, 6,
        4, 8
    ))

    assert(outerProduct(Vec2(1, 2), Vec4(2, 3, 4, 5)) == Mat2x4(
        2, 4,
        3, 6,
        4, 8,
        5, 10
    ))

    assert(outerProduct(Vec3(1, 2, 3), Vec2(2, 3)) == Mat3x2(
        2, 4, 6,
        3, 6, 9
    ))

    assert(outerProduct(Vec3(1, 2, 3), Vec3(2, 3, 4)) == Mat3x3(
        2, 4, 6,
        3, 6, 9,
        4, 8, 12
    ))

    assert(outerProduct(Vec3(1, 2, 3), Vec4(2, 3, 4, 5)) == Mat3x4(
        2, 4, 6,
        3, 6, 9,
        4, 8, 12,
        5, 10, 15
    ))

    assert(outerProduct(Vec4(1, 2, 3, 4), Vec2(2, 3)) == Mat4x2(
        2, 4, 6, 8,
        3, 6, 9, 12
    ))

    assert(outerProduct(Vec4(1, 2, 3, 4), Vec3(2, 3, 4)) == Mat4x3(
        2, 4, 6, 8,
        3, 6, 9, 12,
        4, 8, 12, 16
    ))

    assert(outerProduct(Vec4(1, 2, 3, 4), Vec4(2, 3, 4, 5)) == Mat4x4(
        2, 4, 6, 8,
        3, 6, 9, 12,
        4, 8, 12, 16,
        5, 10, 15, 20
    ))

    assert(
      transpose(
        Mat2x2(
          1, 2,
          3, 4
        )
      ) == Mat2x2(
        1, 3,
        2, 4
      )
    )

    assert(
      transpose(
        Mat2x3(
          1, 2,
          3, 4,
          5, 6
        )
      ) == Mat3x2(
        1, 3, 5,
        2, 4, 6
      )
    )

    assert(
      transpose(
        Mat2x4(
          1, 2,
          3, 4,
          5, 6,
          7, 8
        )
      ) == Mat4x2(
        1, 3, 5, 7,
        2, 4, 6, 8
      )
    )

    assert(
      transpose(
        Mat3x2(
          1, 2, 3,
          4, 5, 6
        )
      ) == Mat2x3(
        1, 4,
        2, 5,
        3, 6
      )
    )

    assert(
      transpose(
        Mat3x3(
          1, 2, 3,
          4, 5, 6,
          7, 8, 9
        )
      ) == Mat3x3(
        1, 4, 7,
        2, 5, 8,
        3, 6, 9
      )
    )

    assert(
      transpose(
        Mat3x4(
          1, 2, 3,
          4, 5, 6,
          7, 8, 9,
          10, 11, 12
        )
      ) == Mat4x3(
        1, 4, 7, 10,
        2, 5, 8, 11,
        3, 6, 9, 12
      )
    )

    assert(
      transpose(
        Mat4x2(
          1, 2, 3, 4,
          5, 6, 7, 8
        )
      ) == Mat2x4(
        1, 5,
        2, 6,
        3, 7,
        4, 8
      )
    )

    assert(
      transpose(
        Mat4x3(
          1, 2, 3, 4,
          5, 6, 7, 8,
          9, 10, 11, 12
        )
      ) == Mat3x4(
        1, 5, 9,
        2, 6, 10,
        3, 7, 11,
        4, 8, 12
      )
    )

    assert(
      transpose(
        Mat4x4(
          1, 2, 3, 4,
          5, 6, 7, 8,
          9, 10, 11, 12,
          13, 14, 15, 16
        )
      ) == Mat4x4(
        1, 5, 9, 13,
        2, 6, 10, 14,
        3, 7, 11, 15,
        4, 8, 12, 16
      )
    )

    // determinant and inverse
    val m2 = Mat2(2, 4, 5, 3)
    assert(determinant(m2) == -14)
    val m2i = inverse(inverse(m2))
    assert(!hasErrors(m2i))
    assert(approxEqual(m2, m2i, 1e-15))

    val m3 = Mat3(2, 4, 5, 3, 3, 6, 4, 3, 2)
    assert(determinant(m3) == 33)
    val m3i = inverse(inverse(m3))
    assert(!hasErrors(m3i))
    assert(approxEqual(m3, m3i, 1e-15))

    val m4 = Mat4(2, 4, 5, 3, 3, 6, 4, 3, 2, 3, 6, 4, 8, 4, 3, 6)
    assert(determinant(m4) == 39)
    val m4i = inverse(inverse(m4))
    assert(!hasErrors(m4i))
    assert(approxEqual(m4, m4i, 1e-13))
  }
}
