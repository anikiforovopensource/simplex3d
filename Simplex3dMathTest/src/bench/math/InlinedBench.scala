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

package bench.math

import simplex3d.math.floatm._
import simplex3d.math.floatm.FloatMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
object InlinedBench {
    def main(args: Array[String]) {
        new InlinedBenchCase().run()
    }
}

class InlinedBenchCase {
    val length = 100
    val loops = 200

    val arrowCount = 5000
    val arrowOriginal = genArrow()
    val arrows = new Array[Float](2*4*arrowCount*3)

    val random = new java.util.Random
    import random._
    def nextVec3f = Vec3f(nextFloat, nextFloat, nextFloat)

    def genArrow() :Array[Float] = {
        val arrow = new Array[Float](2*4*3)
        val arrowLength = 4f
        val arrowHead = sin(radians(60))
        val headScale = 1/3f

        val origin = Vec3f.UnitZ * arrowLength / 2
        val dir = -Vec3f.UnitZ * arrowLength

        val p1 = normalize(cross(Vec3f.UnitY, dir)) * arrowLength*0.5f*headScale
        val p2 = -p1

        def store(i: Int, u: AnyVec3f) {
            arrow(i*3) = u.x
            arrow(i*3 + 1) = u.y
            arrow(i*3 + 2) = u.z
        }
        store(0, origin + dir*arrowHead*headScale)
        store(1, origin + dir)
        val t2 = origin + dir*arrowHead*headScale + p1
        store(2, t2)
        val t3 = origin + dir*arrowHead*headScale + p2
        store(3, t3)
        store(4, origin)
        store(5, t2)
        store(6, origin)
        store(7, t3)

        arrow
    }

    def run() {
        var start = 0L

        start = System.currentTimeMillis
        testInlined(length, loops)
        val inlinedTime = System.currentTimeMillis - start

        start = System.currentTimeMillis
        testReg(length, loops)
        val regularTime = System.currentTimeMillis - start

        start = System.currentTimeMillis
        testMake(length, loops)
        val makeTime = System.currentTimeMillis - start

        println("reg time: " + regularTime +
                ", make time: " + makeTime +
                ", inlined time: " + inlinedTime + ".")
    }

    final def make(scale: AnyVec3f,
                   rotation: AnyMat3f,
                   translation: AnyVec3f)
    :Transform3f =
    {
        import scale.{x => sx, y => sy, z => sz}
        import rotation._
        import translation.{x => tx, y => ty, z => tz}

        Transform3f(ConstMat3x4f(
            m00*sx, m10*sx, m20*sx,
            m01*sy, m11*sy, m21*sy,
            m02*sz, m12*sz, m22*sz,
            tx, ty, tz
        ))
    }

    def testReg(length: Int, loops: Int) {
        var answer = 0

        var l = 0; while (l < loops) {
            var cc = 0; while (cc < length) {


    setSeed(1)
    val spread = 80
    val am = arrows
    val as = arrowOriginal
    val t = Vec3f(0)
    val end = (am.length / 3) - 8
    var i = 0; while (i < end) {
        val loc = (nextVec3f - Vec3f(0.5f))*(spread*2)
        val model = Transform3f(lookAt(-loc, Vec3f.UnitY)) translate(loc)
        var j = 0; while (j < 8) {
            val id = j*3
            t.x = as(id)
            t.y = as(id + 1)
            t.z = as(id + 2)
            val p = model.transformPoint(t)

            val dd = (i + j)*3
            am(dd) = p.x
            am(dd + 1) = p.y
            am(dd + 2) = p.z
            
            j += 1
        }
        i += 8
    }

                cc += 1
            }
            l += 1
        }

        println(answer)
    }

    def testMake(length: Int, loops: Int) {
        var answer = 0

        var l = 0; while (l < loops) {
            var cc = 0; while (cc < length) {


    setSeed(1)
    val spread = 80
    val am = arrows
    val as = arrowOriginal
    val t = Vec3f(0)
    val end = (am.length / 3) - 8
    var i = 0; while (i < end) {
        val loc = (nextVec3f - Vec3f(0.5f))*(spread*2)
        val model = make(Vec3f.One, lookAt(-loc, Vec3f.UnitY), loc)
        var j = 0; while (j < 8) {
            val id = j*3
            t.x = as(id)
            t.y = as(id + 1)
            t.z = as(id + 2)
            val p = model.transformPoint(t)

            val dd = (i + j)*3
            am(dd) = p.x
            am(dd + 1) = p.y
            am(dd + 2) = p.z

            j += 1
        }
        i += 8
    }

                cc += 1
            }
            l += 1
        }

        println(answer)
    }

    def testInlined(length: Int, loops: Int) {
        var answer = 0

        var l = 0; while (l < loops) {
            var cc = 0; while (cc < length) {

    setSeed(1)
    val spread = 80
    val spread2 = spread*spread

    val array = arrows
    val rray = arrowOriginal
    val end = array.length/3
    var i = 0; while (i < end) {
        val s2 = spread*2
        val lx = (nextFloat - 0.5f)*(s2)
        val ly = (nextFloat - 0.5f)*(s2)
        val lz = (nextFloat - 0.5f)*(s2)

        var invlen = 1/sqrt(lx*lx + ly*ly + lz*lz)
        val zax = -lx*invlen
        val zay = -ly*invlen
        val zaz = -lz*invlen

        var xax = Vec3f.UnitY.y*zaz - zay*Vec3f.UnitY.z
        var xay = Vec3f.UnitY.z*zax - zaz*Vec3f.UnitY.x
        var xaz = Vec3f.UnitY.x*zay - zax*Vec3f.UnitY.y

        invlen = 1/sqrt(xax*xax + xay*xay + xaz*xaz)
        xax *= invlen
        xay *= invlen
        xaz *= invlen

        val yax = zay*xaz - xay*zaz
        val yay = zaz*xax - xaz*zax
        val yaz = zax*xay - xax*zay

        var j = 0; while (j < 8) {
            val ro = j*3
            val ux = rray(ro)
            val uy = rray(ro + 1)
            val uz = rray(ro + 2)

            val offset = (i + j)*3
            array(offset) = xax*ux + yax*uy + zax*uz + lx
            array(offset + 1) = xay*ux + yay*uy + zay*uz + ly
            array(offset + 2) = xaz*ux + yaz*uy + zaz*uz + lz

            j += 1
        }

        i += 8
    }

                cc += 1
            }
            l += 1
        }

        println(answer)
    }

}
