/*
 * Simplex3d, MathTest package
 * Copyright (C) 2010, Simplex3d Team
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

package test.math

import java.nio._
import org.scalatest._

import simplex3d.math._
import simplex3d.math.floatm._
import simplex3d.math.doublem._


/**
 * @author Aleksey Nikiforov (lex)
 */
class BufferConversionTest extends FunSuite {

  val floatArray = Array(
    1f+1e-5f, 2f+1e-5f, 3f+1e-5f, 4f+1e-5f,
    5f+1e-5f, 6f+1e-5f, 7f+1e-5f, 8f+1e-5f,
    9f+1e-5f, 10f+1e-5f, 11f+1e-5f, 12f+1e-5f,
    13f+1e-5f, 14f+1e-5f, 15f+1e-5f, 16f+1e-5f
  )
  val F = Mat4x4f(
    floatArray(0), floatArray(1), floatArray(2), floatArray(3),
    floatArray(4), floatArray(5), floatArray(6), floatArray(7),
    floatArray(8), floatArray(9), floatArray(10), floatArray(11),
    floatArray(12), floatArray(13), floatArray(14), floatArray(15)
  )

  val doubleArray = Array(
    1+1e-14, 2+1e-14, 3+1e-14, 4+1e-14,
    5+1e-14, 6+1e-14, 7+1e-14, 8+1e-14,
    9+1e-14, 10+1e-14, 11+1e-14, 12+1e-14,
    13+1e-14, 14+1e-14, 15+1e-14, 16+1e-14
  )
  val D = Mat4x4d(
    doubleArray(0), doubleArray(1), doubleArray(2), doubleArray(3),
    doubleArray(4), doubleArray(5), doubleArray(6), doubleArray(7),
    doubleArray(8), doubleArray(9), doubleArray(10), doubleArray(11),
    doubleArray(12), doubleArray(13), doubleArray(14), doubleArray(15)
  )

  def verify(array: Array[Float], offset: Int, rows: Int, columns: Int) {
    for (c <- 0 until 4) {
      for (r <- 0 until 4) {
        if (r < rows && c < columns) {
          if (array(offset + r + c*4) != floatArray(r + c*4))
            throw new AssertionError("Test failed.")
        }
        else if (r == c) {
          if (array(offset + r + c*4) != 1)
            throw new AssertionError("Test failed.")
        }
        else {
          if (array(offset + r + c*4) != 0)
            throw new AssertionError("Test failed.")
        }
      }
    }
  }
  def verify(buffer: FloatBuffer, rows: Int, columns: Int) {
    for (c <- 0 until 4) {
      for (r <- 0 until 4) {
        if (r < rows && c < columns) {
          if (buffer.get() != floatArray(r + c*4))
            throw new AssertionError("Test failed.")
        }
        else if (r == c) {
          if (buffer.get() != 1)
            throw new AssertionError("Test failed.")
        }
        else {
          if (buffer.get() != 0)
            throw new AssertionError("Test failed.")
        }
      }
    }
  }

  def verify(array: Array[Double], offset: Int, rows: Int, columns: Int) {
    for (c <- 0 until 4) {
      for (r <- 0 until 4) {
        if (r < rows && c < columns) {
          if (array(offset + r + c*4) != doubleArray(r + c*4))
            throw new AssertionError("Test failed.")
        }
        else if (r == c) {
          if (array(offset + r + c*4) != 1)
            throw new AssertionError("Test failed.")
        }
        else {
          if (array(offset + r + c*4) != 0)
            throw new AssertionError("Test failed.")
        }
      }
    }
  }
  def verify(buffer: DoubleBuffer, rows: Int, columns: Int) {
    for (c <- 0 until 4) {
      for (r <- 0 until 4) {
        if (r < rows && c < columns) {
          if (buffer.get() != doubleArray(r + c*4))
            throw new AssertionError("Test failed.")
        }
        else if (r == c) {
          if (buffer.get() != 1)
            throw new AssertionError("Test failed.")
        }
        else {
          if (buffer.get() != 0)
            throw new AssertionError("Test failed.")
        }
      }
    }
  }

  test("Float mat conversion") {
    val offset = 7

    // Array
    {
      val array = new Array[Float](16)
      matrixToArray(Mat2x2f(F), array)
      verify(array, 0, 2, 2)
    }

    {
      val array = new Array[Float](16 + offset)
      matrixToArray(Mat2x2f(F), array, offset)
      verify(array, offset, 2, 2)
    }

    {
      val array = new Array[Float](16)
      matrixToArray(Mat2x3f(F), array)
      verify(array, 0, 2, 3)
    }

    {
      val array = new Array[Float](16 + offset)
      matrixToArray(Mat2x3f(F), array, offset)
      verify(array, offset, 2, 3)
    }

    {
      val array = new Array[Float](16)
      matrixToArray(Mat2x4f(F), array)
      verify(array, 0, 2, 4)
    }

    {
      val array = new Array[Float](16 + offset)
      matrixToArray(Mat2x4f(F), array, offset)
      verify(array, offset, 2, 4)
    }

    {
      val array = new Array[Float](16)
      matrixToArray(Mat3x2f(F), array)
      verify(array, 0, 3, 2)
    }

    {
      val array = new Array[Float](16 + offset)
      matrixToArray(Mat3x2f(F), array, offset)
      verify(array, offset, 3, 2)
    }

    {
      val array = new Array[Float](16)
      matrixToArray(Mat3x3f(F), array)
      verify(array, 0, 3, 3)
    }

    {
      val array = new Array[Float](16 + offset)
      matrixToArray(Mat3x3f(F), array, offset)
      verify(array, offset, 3, 3)
    }

    {
      val array = new Array[Float](16)
      matrixToArray(Mat3x4f(F), array)
      verify(array, 0, 3, 4)
    }

    {
      val array = new Array[Float](16 + offset)
      matrixToArray(Mat3x4f(F), array, offset)
      verify(array, offset, 3, 4)
    }

    {
      val array = new Array[Float](16)
      matrixToArray(Mat4x2f(F), array)
      verify(array, 0, 4, 2)
    }

    {
      val array = new Array[Float](16 + offset)
      matrixToArray(Mat4x2f(F), array, offset)
      verify(array, offset, 4, 2)
    }

    {
      val array = new Array[Float](16)
      matrixToArray(Mat4x3f(F), array)
      verify(array, 0, 4, 3)
    }

    {
      val array = new Array[Float](16 + offset)
      matrixToArray(Mat4x3f(F), array, offset)
      verify(array, offset, 4, 3)
    }

    {
      val array = new Array[Float](16)
      matrixToArray(Mat4x4f(F), array)
      verify(array, 0, 4, 4)
    }

    {
      val array = new Array[Float](16 + offset)
      matrixToArray(Mat4x4f(F), array, offset)
      verify(array, offset, 4, 4)
    }


    // Buffer
    {
      val buffer = FloatBuffer.wrap(new Array[Float](16))
      matrixToBuffer(Mat2x2f(F), buffer)
      buffer.position(0); verify(buffer, 2, 2)
    }

    {
      val buffer = FloatBuffer.wrap(new Array[Float](16 + offset))
      buffer.position(offset); matrixToBuffer(Mat2x2f(F), buffer)
      buffer.position(offset); verify(buffer, 2, 2)
    }

    {
      val buffer = FloatBuffer.wrap(new Array[Float](16))
      matrixToBuffer(Mat2x3f(F), buffer)
      buffer.position(0); verify(buffer, 2, 3)
    }

    {
      val buffer = FloatBuffer.wrap(new Array[Float](16 + offset))
      buffer.position(offset); matrixToBuffer(Mat2x3f(F), buffer)
      buffer.position(offset); verify(buffer, 2, 3)
    }

    {
      val buffer = FloatBuffer.wrap(new Array[Float](16))
      matrixToBuffer(Mat2x4f(F), buffer)
      buffer.position(0); verify(buffer, 2, 4)
    }

    {
      val buffer = FloatBuffer.wrap(new Array[Float](16 + offset))
      buffer.position(offset); matrixToBuffer(Mat2x4f(F), buffer)
      buffer.position(offset); verify(buffer, 2, 4)
    }

    {
      val buffer = FloatBuffer.wrap(new Array[Float](16))
      matrixToBuffer(Mat3x2f(F), buffer)
      buffer.position(0); verify(buffer, 3, 2)
    }

    {
      val buffer = FloatBuffer.wrap(new Array[Float](16 + offset))
      buffer.position(offset); matrixToBuffer(Mat3x2f(F), buffer)
      buffer.position(offset); verify(buffer, 3, 2)
    }

    {
      val buffer = FloatBuffer.wrap(new Array[Float](16))
      matrixToBuffer(Mat3x3f(F), buffer)
      buffer.position(0); verify(buffer, 3, 3)
    }

    {
      val buffer = FloatBuffer.wrap(new Array[Float](16 + offset))
      buffer.position(offset); matrixToBuffer(Mat3x3f(F), buffer)
      buffer.position(offset); verify(buffer, 3, 3)
    }

    {
      val buffer = FloatBuffer.wrap(new Array[Float](16))
      matrixToBuffer(Mat3x4f(F), buffer)
      buffer.position(0); verify(buffer, 3, 4)
    }

    {
      val buffer = FloatBuffer.wrap(new Array[Float](16 + offset))
      buffer.position(offset); matrixToBuffer(Mat3x4f(F), buffer)
      buffer.position(offset); verify(buffer, 3, 4)
    }

    {
      val buffer = FloatBuffer.wrap(new Array[Float](16))
      matrixToBuffer(Mat4x2f(F), buffer)
      buffer.position(0); verify(buffer, 4, 2)
    }

    {
      val buffer = FloatBuffer.wrap(new Array[Float](16 + offset))
      buffer.position(offset); matrixToBuffer(Mat4x2f(F), buffer)
      buffer.position(offset); verify(buffer, 4, 2)
    }

    {
      val buffer = FloatBuffer.wrap(new Array[Float](16))
      matrixToBuffer(Mat4x3f(F), buffer)
      buffer.position(0); verify(buffer, 4, 3)
    }

    {
      val buffer = FloatBuffer.wrap(new Array[Float](16 + offset))
      buffer.position(offset); matrixToBuffer(Mat4x3f(F), buffer)
      buffer.position(offset); verify(buffer, 4, 3)
    }

    {
      val buffer = FloatBuffer.wrap(new Array[Float](16))
      matrixToBuffer(Mat4x4f(F), buffer)
      buffer.position(0); verify(buffer, 4, 4)
    }

    {
      val buffer = FloatBuffer.wrap(new Array[Float](16 + offset))
      buffer.position(offset); matrixToBuffer(Mat4x4f(F), buffer)
      buffer.position(offset); verify(buffer, 4, 4)
    }
  }


  test("Double mat conversion") {
    val offset = 7

    // Array
    {
      val array = new Array[Double](16)
      matrixToArray(Mat2x2d(D), array)
      verify(array, 0, 2, 2)
    }

    {
      val array = new Array[Double](16 + offset)
      matrixToArray(Mat2x2d(D), array, offset)
      verify(array, offset, 2, 2)
    }

    {
      val array = new Array[Double](16)
      matrixToArray(Mat2x3d(D), array)
      verify(array, 0, 2, 3)
    }

    {
      val array = new Array[Double](16 + offset)
      matrixToArray(Mat2x3d(D), array, offset)
      verify(array, offset, 2, 3)
    }

    {
      val array = new Array[Double](16)
      matrixToArray(Mat2x4d(D), array)
      verify(array, 0, 2, 4)
    }

    {
      val array = new Array[Double](16 + offset)
      matrixToArray(Mat2x4d(D), array, offset)
      verify(array, offset, 2, 4)
    }

    {
      val array = new Array[Double](16)
      matrixToArray(Mat3x2d(D), array)
      verify(array, 0, 3, 2)
    }

    {
      val array = new Array[Double](16 + offset)
      matrixToArray(Mat3x2d(D), array, offset)
      verify(array, offset, 3, 2)
    }

    {
      val array = new Array[Double](16)
      matrixToArray(Mat3x3d(D), array)
      verify(array, 0, 3, 3)
    }

    {
      val array = new Array[Double](16 + offset)
      matrixToArray(Mat3x3d(D), array, offset)
      verify(array, offset, 3, 3)
    }

    {
      val array = new Array[Double](16)
      matrixToArray(Mat3x4d(D), array)
      verify(array, 0, 3, 4)
    }

    {
      val array = new Array[Double](16 + offset)
      matrixToArray(Mat3x4d(D), array, offset)
      verify(array, offset, 3, 4)
    }

    {
      val array = new Array[Double](16)
      matrixToArray(Mat4x2d(D), array)
      verify(array, 0, 4, 2)
    }

    {
      val array = new Array[Double](16 + offset)
      matrixToArray(Mat4x2d(D), array, offset)
      verify(array, offset, 4, 2)
    }

    {
      val array = new Array[Double](16)
      matrixToArray(Mat4x3d(D), array)
      verify(array, 0, 4, 3)
    }

    {
      val array = new Array[Double](16 + offset)
      matrixToArray(Mat4x3d(D), array, offset)
      verify(array, offset, 4, 3)
    }

    {
      val array = new Array[Double](16)
      matrixToArray(Mat4x4d(D), array)
      verify(array, 0, 4, 4)
    }

    {
      val array = new Array[Double](16 + offset)
      matrixToArray(Mat4x4d(D), array, offset)
      verify(array, offset, 4, 4)
    }


    // Buffer
    {
      val buffer = DoubleBuffer.wrap(new Array[Double](16))
      matrixToBuffer(Mat2x2d(D), buffer)
      buffer.position(0); verify(buffer, 2, 2)
    }

    {
      val buffer = DoubleBuffer.wrap(new Array[Double](16 + offset))
      buffer.position(offset); matrixToBuffer(Mat2x2d(D), buffer)
      buffer.position(offset); verify(buffer, 2, 2)
    }

    {
      val buffer = DoubleBuffer.wrap(new Array[Double](16))
      matrixToBuffer(Mat2x3d(D), buffer)
      buffer.position(0); verify(buffer, 2, 3)
    }

    {
      val buffer = DoubleBuffer.wrap(new Array[Double](16 + offset))
      buffer.position(offset); matrixToBuffer(Mat2x3d(D), buffer)
      buffer.position(offset); verify(buffer, 2, 3)
    }

    {
      val buffer = DoubleBuffer.wrap(new Array[Double](16))
      matrixToBuffer(Mat2x4d(D), buffer)
      buffer.position(0); verify(buffer, 2, 4)
    }

    {
      val buffer = DoubleBuffer.wrap(new Array[Double](16 + offset))
      buffer.position(offset); matrixToBuffer(Mat2x4d(D), buffer)
      buffer.position(offset); verify(buffer, 2, 4)
    }

    {
      val buffer = DoubleBuffer.wrap(new Array[Double](16))
      matrixToBuffer(Mat3x2d(D), buffer)
      buffer.position(0); verify(buffer, 3, 2)
    }

    {
      val buffer = DoubleBuffer.wrap(new Array[Double](16 + offset))
      buffer.position(offset); matrixToBuffer(Mat3x2d(D), buffer)
      buffer.position(offset); verify(buffer, 3, 2)
    }

    {
      val buffer = DoubleBuffer.wrap(new Array[Double](16))
      matrixToBuffer(Mat3x3d(D), buffer)
      buffer.position(0); verify(buffer, 3, 3)
    }

    {
      val buffer = DoubleBuffer.wrap(new Array[Double](16 + offset))
      buffer.position(offset); matrixToBuffer(Mat3x3d(D), buffer)
      buffer.position(offset); verify(buffer, 3, 3)
    }

    {
      val buffer = DoubleBuffer.wrap(new Array[Double](16))
      matrixToBuffer(Mat3x4d(D), buffer)
      buffer.position(0); verify(buffer, 3, 4)
    }

    {
      val buffer = DoubleBuffer.wrap(new Array[Double](16 + offset))
      buffer.position(offset); matrixToBuffer(Mat3x4d(D), buffer)
      buffer.position(offset); verify(buffer, 3, 4)
    }

    {
      val buffer = DoubleBuffer.wrap(new Array[Double](16))
      matrixToBuffer(Mat4x2d(D), buffer)
      buffer.position(0); verify(buffer, 4, 2)
    }

    {
      val buffer = DoubleBuffer.wrap(new Array[Double](16 + offset))
      buffer.position(offset); matrixToBuffer(Mat4x2d(D), buffer)
      buffer.position(offset); verify(buffer, 4, 2)
    }

    {
      val buffer = DoubleBuffer.wrap(new Array[Double](16))
      matrixToBuffer(Mat4x3d(D), buffer)
      buffer.position(0); verify(buffer, 4, 3)
    }

    {
      val buffer = DoubleBuffer.wrap(new Array[Double](16 + offset))
      buffer.position(offset); matrixToBuffer(Mat4x3d(D), buffer)
      buffer.position(offset); verify(buffer, 4, 3)
    }

    {
      val buffer = DoubleBuffer.wrap(new Array[Double](16))
      matrixToBuffer(Mat4x4d(D), buffer)
      buffer.position(0); verify(buffer, 4, 4)
    }

    {
      val buffer = DoubleBuffer.wrap(new Array[Double](16 + offset))
      buffer.position(offset); matrixToBuffer(Mat4x4d(D), buffer)
      buffer.position(offset); verify(buffer, 4, 4)
    }
  }
}
