/*
 * Simplex3D, Math tests
 * Copyright (C) 2009 Simplex3D team
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package bench.math


/**
 * @author Aleksey Nikiforov (lex)
 */
object MatBench {
    def main(args: Array[String]) {
        val b = new MatMulBench()
        b.runWorstCase
        b.runBestCase
    }
}

class MatMulBench {
    val length = 100
    val loops = 1000000

    val data1 = new Array[Mat4d](length)
    val data2 = new Array[ModifiedMat4d](length)

    val seed = 1
    val random = new scala.util.Random(seed)
    def rd = random.nextDouble()

    def runWorstCase() {
        var start = 0L

        start = System.currentTimeMillis
        testOriginalW(length, loops)
        val originalTime = System.currentTimeMillis - start

        start = System.currentTimeMillis
        testModifiedW(length, loops)
        val modifiedTime = System.currentTimeMillis - start

        println("Original time: " + originalTime +
                ", modified time: " + modifiedTime + ".")
    }

    def runBestCase() {
        var start = 0L

        start = System.currentTimeMillis
        testOriginalB(length, loops)
        val originalTime = System.currentTimeMillis - start
        
        start = System.currentTimeMillis
        testModifiedB(length, loops)
        val modifiedTime = System.currentTimeMillis - start

        println("Original time: " + originalTime +
                ", modified time: " + modifiedTime + ".")
    }

    def testOriginalW(length: Int, loops: Int) {
        random.setSeed(seed)

        var l = 0; while (l < loops) {
            val m = new Mat4d(
                rd, rd, rd, rd,
                rd, rd, rd, rd,
                rd, rd, rd, rd,
                rd, rd, rd, rd
            )
            val r = new Mat4d(
                rd, rd, rd, rd,
                rd, rd, rd, rd,
                rd, rd, rd, rd,
                rd, rd, rd, rd
            )
            val d = new Mat4d(
                rd, rd, rd, rd,
                rd, rd, rd, rd,
                rd, rd, rd, rd,
                rd, rd, rd, rd
            )

            var i = 2; while (i < length) {

                m.mul(d, d)

                i += 1
            }
            l += 1
        }
    }

    def testModifiedW(length: Int, loops: Int) {
        random.setSeed(seed)

        var l = 0; while (l < loops) {
            val m = new Mat4d(
                rd, rd, rd, rd,
                rd, rd, rd, rd,
                rd, rd, rd, rd,
                rd, rd, rd, rd
            )
            val r = new Mat4d(
                rd, rd, rd, rd,
                rd, rd, rd, rd,
                rd, rd, rd, rd,
                rd, rd, rd, rd
            )
            val d = new Mat4d(
                rd, rd, rd, rd,
                rd, rd, rd, rd,
                rd, rd, rd, rd,
                rd, rd, rd, rd
            )

            var i = 2; while (i < length) {

                m.mul(d, d)

                i += 1
            }
            l += 1
        }
    }

    def testOriginalB(length: Int, loops: Int) {
        random.setSeed(seed)

        var l = 0; while (l < loops) {
            val m = new Mat4d(
                rd, rd, rd, rd,
                rd, rd, rd, rd,
                rd, rd, rd, rd,
                rd, rd, rd, rd
            )
            val r = new Mat4d(
                rd, rd, rd, rd,
                rd, rd, rd, rd,
                rd, rd, rd, rd,
                rd, rd, rd, rd
            )
            val d = new Mat4d(
                rd, rd, rd, rd,
                rd, rd, rd, rd,
                rd, rd, rd, rd,
                rd, rd, rd, rd
            )

            var i = 2; while (i < length) {

                m.mul(d, r)

                i += 1
            }
            l += 1
        }
    }

    def testModifiedB(length: Int, loops: Int) {
        random.setSeed(seed)

        var l = 0; while (l < loops) {
            val m = new Mat4d(
                rd, rd, rd, rd,
                rd, rd, rd, rd,
                rd, rd, rd, rd,
                rd, rd, rd, rd
            )
            val r = new Mat4d(
                rd, rd, rd, rd,
                rd, rd, rd, rd,
                rd, rd, rd, rd,
                rd, rd, rd, rd
            )
            val d = new Mat4d(
                rd, rd, rd, rd,
                rd, rd, rd, rd,
                rd, rd, rd, rd,
                rd, rd, rd, rd
            )

            var i = 2; while (i < length) {

                m.mul(d, r)

                i += 1
            }
            l += 1
        }
    }
}
class Mat4d (
    var m00: Double, var m10: Double, var m20: Double, var m30: Double,
    var m01: Double, var m11: Double, var m21: Double, var m31: Double,
    var m02: Double, var m12: Double, var m22: Double, var m32: Double,
    var m03: Double, var m13: Double, var m23: Double, var m33: Double
) {

    def mul(m: Mat4d, result: Mat4d) {
        val a00 = m00*m.m00 + m01*m.m10 + m02*m.m20 + m03*m.m30
        val a10 = m10*m.m00 + m11*m.m10 + m12*m.m20 + m13*m.m30
        val a20 = m20*m.m00 + m21*m.m10 + m22*m.m20 + m23*m.m30
        val a30 = m30*m.m00 + m31*m.m10 + m32*m.m20 + m33*m.m30

        val a01 = m00*m.m01 + m01*m.m11 + m02*m.m21 + m03*m.m31
        val a11 = m10*m.m01 + m11*m.m11 + m12*m.m21 + m13*m.m31
        val a21 = m20*m.m01 + m21*m.m11 + m22*m.m21 + m23*m.m31
        val a31 = m30*m.m01 + m31*m.m11 + m32*m.m21 + m33*m.m31

        val a02 = m00*m.m02 + m01*m.m12 + m02*m.m22 + m03*m.m32
        val a12 = m10*m.m02 + m11*m.m12 + m12*m.m22 + m13*m.m32
        val a22 = m20*m.m02 + m21*m.m12 + m22*m.m22 + m23*m.m32
        val a32 = m30*m.m02 + m31*m.m12 + m32*m.m22 + m33*m.m32

        val a03 = m00*m.m03 + m01*m.m13 + m02*m.m23 + m03*m.m33
        val a13 = m10*m.m03 + m11*m.m13 + m12*m.m23 + m13*m.m33
        val a23 = m20*m.m03 + m21*m.m13 + m22*m.m23 + m23*m.m33
        val a33 = m30*m.m03 + m31*m.m13 + m32*m.m23 + m33*m.m33

        result.m00 = a00; result.m10 = a10; result.m20 = a20; result.m30 = a30;
        result.m01 = a01; result.m11 = a11; result.m21 = a21; result.m31 = a31;
        result.m02 = a02; result.m12 = a12; result.m22 = a22; result.m32 = a32;
        result.m03 = a03; result.m13 = a13; result.m23 = a23; result.m33 = a33
    }

    override def toString = {
        this.getClass.getSimpleName +
        "(" +
            m00 + ", " + m10 + ", " + m20 + ", " + m30 + "; " +
            m01 + ", " + m11 + ", " + m21 + ", " + m31 + "; " +
            m02 + ", " + m12 + ", " + m22 + ", " + m32 + "; " +
            m03 + ", " + m13 + ", " + m23 + ", " + m33 +
        ")"
    }
}

class ModifiedMat4d (
    var m00: Double, var m10: Double, var m20: Double, var m30: Double,
    var m01: Double, var m11: Double, var m21: Double, var m31: Double,
    var m02: Double, var m12: Double, var m22: Double, var m32: Double,
    var m03: Double, var m13: Double, var m23: Double, var m33: Double
) {

    def mul(m: ModifiedMat4d, result: ModifiedMat4d) {
    if ((this ne m) && (this ne result) && (m ne result)) {
        result.m00 = m00*m.m00 + m01*m.m10 + m02*m.m20 + m03*m.m30
        result.m10 = m10*m.m00 + m11*m.m10 + m12*m.m20 + m13*m.m30
        result.m20 = m20*m.m00 + m21*m.m10 + m22*m.m20 + m23*m.m30
        result.m30 = m30*m.m00 + m31*m.m10 + m32*m.m20 + m33*m.m30

        result.m01 = m00*m.m01 + m01*m.m11 + m02*m.m21 + m03*m.m31
        result.m11 = m10*m.m01 + m11*m.m11 + m12*m.m21 + m13*m.m31
        result.m21 = m20*m.m01 + m21*m.m11 + m22*m.m21 + m23*m.m31
        result.m31 = m30*m.m01 + m31*m.m11 + m32*m.m21 + m33*m.m31

        result.m02 = m00*m.m02 + m01*m.m12 + m02*m.m22 + m03*m.m32
        result.m12 = m10*m.m02 + m11*m.m12 + m12*m.m22 + m13*m.m32
        result.m22 = m20*m.m02 + m21*m.m12 + m22*m.m22 + m23*m.m32
        result.m32 = m30*m.m02 + m31*m.m12 + m32*m.m22 + m33*m.m32

        result.m03 = m00*m.m03 + m01*m.m13 + m02*m.m23 + m03*m.m33
        result.m13 = m10*m.m03 + m11*m.m13 + m12*m.m23 + m13*m.m33
        result.m23 = m20*m.m03 + m21*m.m13 + m22*m.m23 + m23*m.m33
        result.m33 = m30*m.m03 + m31*m.m13 + m32*m.m23 + m33*m.m33
    } else {
        val a00 = m00*m.m00 + m01*m.m10 + m02*m.m20 + m03*m.m30
        val a10 = m10*m.m00 + m11*m.m10 + m12*m.m20 + m13*m.m30
        val a20 = m20*m.m00 + m21*m.m10 + m22*m.m20 + m23*m.m30
        val a30 = m30*m.m00 + m31*m.m10 + m32*m.m20 + m33*m.m30

        val a01 = m00*m.m01 + m01*m.m11 + m02*m.m21 + m03*m.m31
        val a11 = m10*m.m01 + m11*m.m11 + m12*m.m21 + m13*m.m31
        val a21 = m20*m.m01 + m21*m.m11 + m22*m.m21 + m23*m.m31
        val a31 = m30*m.m01 + m31*m.m11 + m32*m.m21 + m33*m.m31

        val a02 = m00*m.m02 + m01*m.m12 + m02*m.m22 + m03*m.m32
        val a12 = m10*m.m02 + m11*m.m12 + m12*m.m22 + m13*m.m32
        val a22 = m20*m.m02 + m21*m.m12 + m22*m.m22 + m23*m.m32
        val a32 = m30*m.m02 + m31*m.m12 + m32*m.m22 + m33*m.m32

        val a03 = m00*m.m03 + m01*m.m13 + m02*m.m23 + m03*m.m33
        val a13 = m10*m.m03 + m11*m.m13 + m12*m.m23 + m13*m.m33
        val a23 = m20*m.m03 + m21*m.m13 + m22*m.m23 + m23*m.m33
        val a33 = m30*m.m03 + m31*m.m13 + m32*m.m23 + m33*m.m33

        result.m00 = a00; result.m10 = a10; result.m20 = a20; result.m30 = a30;
        result.m01 = a01; result.m11 = a11; result.m21 = a21; result.m31 = a31;
        result.m02 = a02; result.m12 = a12; result.m22 = a22; result.m32 = a32;
        result.m03 = a03; result.m13 = a13; result.m23 = a23; result.m33 = a33
    }
    }

    override def toString = {
        this.getClass.getSimpleName +
        "(" +
            m00 + ", " + m10 + ", " + m20 + ", " + m30 + "; " +
            m01 + ", " + m11 + ", " + m21 + ", " + m31 + "; " +
            m02 + ", " + m12 + ", " + m22 + ", " + m32 + "; " +
            m03 + ", " + m13 + ", " + m23 + ", " + m33 +
        ")"
    }
}
