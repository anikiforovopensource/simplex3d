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

import simplex3d.math._
import simplex3d.math.BaseMath._
import simplex3d.math.floatm.renamed._
import simplex3d.math.floatm.FloatMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
object RotationBench {
    def main(args: Array[String]) {
        new RotationBenchCase().run()
    }
}

class RotationBenchCase {
    val length = 10000
    val loops = 1000
    
    val random = new scala.util.Random(1)
    def rd = random.nextFloat()
    val data = new Array[Vec4](length)
    for (i <- 0 until length) {
        data(i) = Vec4(rd, rd, rd, rd)
    }

    def run() {
        var start = 0L

        start = System.currentTimeMillis
        testRegular(length, loops)
        val regularTime = System.currentTimeMillis - start
        
        start = System.currentTimeMillis
        testCopy(length, loops)
        val copyTime = System.currentTimeMillis - start

        println("Reg time: " + regularTime +
                ", copy time: " + copyTime + ".")
    }

    def testRegular(length: Int, loops: Int) {
        var answer = 0

        var l = 0; while (l < loops) {
            var i = 0; while (i < length) {

                // Bench code
                val v = data(i)
                val u = Vec3(v.y, v.z, v.w)
                val t = Mat3x4(1)
                t(0) = u
                rotationMatFrom(v.x, u, t)
                var p = t.transformPoint(u)
                answer = int(p.x + p.y + p.z)
                rotationMatFrom(v.x + 1, u, t)
                p = t.transformPoint(u)
                answer = int(p.x + p.y + p.z)
                rotationMatFrom(v.x + 2, u, t)
                p = t.transformPoint(u)
                answer = int(p.x + p.y + p.z)

                i += 1
            }
            l += 1
        }

        println(answer)
    }

    def testCopy(length: Int, loops: Int) {
        var answer = 0

        var l = 0; while (l < loops) {
            var i = 0; while (i < length) {

                // Bench code
                val v = data(i)
                val u = Vec3(v.y, v.z, v.w)
                var t = Mat3x4(rotationMatNew(v.x, u))
                t(0) = u
                var p = t.transformPoint(u)
                answer = int(p.x + p.y + p.z)
                t = Mat3x4(rotationMatNew(v.x + 1, u))
                t(0) = u
                p = t.transformPoint(u)
                answer = int(p.x + p.y + p.z)
                t = Mat3x4(rotationMatNew(v.x + 2, u))
                t(0) = u
                p = t.transformPoint(u)
                answer = int(p.x + p.y + p.z)

                i += 1
            }
            l += 1
        }

        println(answer)
    }

    def rotationMatNew(angle: Float, axis: AnyVec3) =
    {
        import axis._

        val sinA = sin(angle)
        val cosA = cos(angle)
        val c = 1 - cosA
        val sx = sinA*x
        val sy = sinA*y
        val sz = sinA*z
        val temp = c*x
        val cxy = temp*y
        val cxz = temp*z
        val cyz = c*y*z

        Mat3(
            cosA + c*x*x, cxy + sz, cxz - sy,
            cxy - sz, cosA + c*y*y, cyz + sx,
            cxz + sy, cyz - sx, cosA + c*z*z
        )
    }

}
