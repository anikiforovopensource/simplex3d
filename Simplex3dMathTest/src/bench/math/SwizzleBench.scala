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

import simplex3d.math.doublem.renamed._


/**
 * @author Aleksey Nikiforov (lex)
 */
object SwizzleBench {
    def main(args: Array[String]) {
        new SwizzleBenchCase().run()
    }
}

class SwizzleBenchCase {
    def run() {
        val length = 10000
        val loops = 100000

        var start = 0L
        
        start = System.currentTimeMillis
        testTrait(length, loops)
        val traitTime = System.currentTimeMillis - start

        start = System.currentTimeMillis
        testAbstract(length, loops)
        val abstractTime = System.currentTimeMillis - start

        start = System.currentTimeMillis
        testInlined(length, loops)
        val inlinedTime = System.currentTimeMillis - start

        start = System.currentTimeMillis
        testImplemented(length, loops)
        val implementedTime = System.currentTimeMillis - start

        start = System.currentTimeMillis
        testNoSwizzle(length, loops)
        val noSwizzleTime = System.currentTimeMillis - start

        println("Trait time: " + traitTime +
                ", abstract time: " + abstractTime +
                ", inlined time: " + inlinedTime +
                ", no swizzle time: " + noSwizzleTime +
                ", implemented time: " + implementedTime + ".")
    }

    def testTrait(length: Int, loops: Int) {
        implicit def vecToSwizzled(u: Vec4m) = new Vec4Swizzled(u)
        var answer = 0

        var l = 0; while (l < loops) {
            var i = 0; while (i < length) {

                // Bench code
                val v = Vec4m(i, i + 1, i + 2, i + 3)
                val u = v.yzwx
                val r = v + u
                val l2 = (r.x*r.x + r.y*r.y + r.z*r.z + r.w*r.w)
                answer += l2.asInstanceOf[Int]

                i += 1
            }
            l += 1
        }

        println(answer)
    }

    def testAbstract(length: Int, loops: Int) {
        var answer = 0

        var l = 0; while (l < loops) {
            var i = 0; while (i < length) {

                // Bench code
                val v = Vec4m(i, i + 1, i + 2, i + 3)
                val u = v.xyzw
                val r = v + u
                val l2 = (r.x*r.x + r.y*r.y + r.z*r.z + r.w*r.w)
                answer += l2.asInstanceOf[Int]

                i += 1
            }
            l += 1
        }

        println(answer)
    }

    def testInlined(length: Int, loops: Int) {
        var answer = 0

        var l = 0; while (l < loops) {
            var i = 0; while (i < length) {

                // Bench code
                val v = Vec4m(i, i + 1, i + 2, i + 3)
                val u = v.zwxy
                val r = v + u
                val l2 = (r.x*r.x + r.y*r.y + r.z*r.z + r.w*r.w)
                answer += l2.asInstanceOf[Int]

                i += 1
            }
            l += 1
        }

        println(answer)
    }

    def testImplemented(length: Int, loops: Int) {
        var answer = 0

        var l = 0; while (l < loops) {
            var i = 0; while (i < length) {

                // Bench code
                val v = ConstVec4(i, i + 1, i + 2, i + 3)
                val u = v.yzwx
                val r = v + u
                val l2 = (r.x*r.x + r.y*r.y + r.z*r.z + r.w*r.w)
                answer += l2.asInstanceOf[Int]

                i += 1
            }
            l += 1
        }

        println(answer)
    }
    
    def testNoSwizzle(length: Int, loops: Int) {
        var answer = 0

        var l = 0; while (l < loops) {
            var i = 0; while (i < length) {

                // Bench code
                val v = Vec4m(i, i + 1, i + 2, i + 3)
                val u = Vec4m(v.x, v.y, v.z, v.w)
                val r = v + u
                val l2 = (r.x*r.x + r.y*r.y + r.z*r.z + r.w*r.w)
                answer += l2.asInstanceOf[Int]

                i += 1
            }
            l += 1
        }

        println(answer)
    }
}

abstract class AbsSwizzle[P, R] {
    def x: P
    def y: P
    def z: P
    def w: P
    def absMake(x: P, y: P, z: P, w: P) :R

    def xyzw = absMake(x, y, z, w)
}

// Modified Vec4
final class Vec4m(var x: Double, var y: Double, var z: Double, var w: Double)
extends AbsSwizzle[Double, Vec4m] with ReadDouble
{
    def +(u: Vec4m) = new Vec4m(x + u.x, y + u.y, z + u.z, w + u.w)
    def add(u: Vec4m, r: Vec4m) = {
        r.x = x + u.x
        r.y = y + u.y
        r.z = z + u.z
        r.w = w + u.w
        r
    }

    def zwxy = new Vec4m(z, w, x, y)

    def absMake(x: Double, y: Double, z: Double, w: Double) :Vec4m =
        new Vec4m(x, y, z, w)
}

object Vec4m {
    def apply(x: Double, y: Double, z: Double, w: Double) =
        new Vec4m(x, y, z, w)

    def apply(u: Vec4m) =
        new Vec4m(u.x, u.y, u.z, u.w)
}

trait Read[P] {
    def x: P
    def y: P
    def z: P
    def w: P
}

trait ReadDouble extends Read[Double] {
    def x: Double
    def y: Double
    def z: Double
    def w: Double
}

trait Swizzle4[P, R] extends VecFactory[P, R] {
    def x: P
    def y: P
    def z: P
    def w: P

    def yzwx: R = make(y, z, w, x)
}

trait VecFactory[P, R] {
    protected def make(x: P, y: P, z: P, w: P) :R
}

class Vec4mFactory extends VecFactory[Double, Vec4m] {
    protected def make(x: Double, y: Double, z: Double, w: Double) :Vec4m = {
        new Vec4m(x, y, z, w)
    }
}

class Vec4Swizzled(u: Vec4m) extends Vec4mFactory
with Swizzle4[Double, Vec4m]
{
    def x = u.x
    def y = u.y
    def z = u.z
    def w = u.w
}
