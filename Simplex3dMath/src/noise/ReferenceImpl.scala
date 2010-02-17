/*
 * Simplex3d, reference implementation of SimplexNoise
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
 *
 * ADDITIONAL TERMS:
 * This file is a Scala port of GLSL shader by Stefan Gustavson.
 * The shader license states:
 *
 * Author: Stefan Gustavson ITN-LiTH (stegu@itn.liu.se) 2004-12-05
 * Simplex indexing functions by Bill Licea-Kane, ATI (bill@ati.com)
 *
 * You may use, modify and redistribute this code free of charge,
 * provided that the author's names and this notice appear intact.
 */
package noise

import simplex3d.math.intm._
import simplex3d.math.doublem._
import simplex3d.math.doublem.DoubleMath._


/**
 * @author Stefan Gustavson, ITN-LiTH
 * @author Bill Licea-Kane, ATI
 *
 *         Ported to Scala
 * @author Aleksey Nikiforov (lex)
 */
object ReferenceImpl {

    def noise(x: Double, y: Double) :Double = {
        noise1(Vec2d(x, y))
    }
    def noise(x: Double, y: Double, z:Double) :Double = {
        noise1(Vec3d(x, y, z))
    }
    def noise(x: Double, y: Double, z:Double, w:Double) :Double = {
        noise1(Vec4d(x, y, z, w))
    }
    

    val perm: Array[Int] = Array(
        151, 160, 137, 91, 90, 15, 131, 13, 201, 95, 96, 53, 194, 233, 7, 225,
        140, 36, 103, 30, 69, 142, 8, 99, 37, 240, 21, 10, 23, 190,  6, 148,
        247, 120, 234, 75, 0, 26, 197, 62, 94, 252, 219, 203, 117, 35, 11, 32,
        57, 177, 33, 88, 237, 149, 56, 87, 174, 20, 125, 136, 171, 168,  68,
        175, 74, 165, 71, 134, 139, 48, 27, 166, 77, 146, 158, 231, 83, 111,
        229, 122, 60, 211, 133, 230, 220, 105, 92, 41, 55, 46, 245, 40, 244,
        102, 143, 54,  65, 25, 63, 161,  1, 216, 80, 73, 209, 76, 132, 187, 208,
        89, 18, 169, 200, 196, 135, 130, 116, 188, 159, 86, 164, 100, 109, 198,
        173, 186,  3, 64, 52, 217, 226, 250, 124, 123, 5, 202, 38, 147, 118,
        126, 255, 82, 85, 212, 207, 206, 59, 227, 47, 16, 58, 17, 182, 189, 28,
        42, 223, 183, 170, 213, 119, 248, 152,  2, 44, 154, 163,  70, 221, 153,
        101, 155, 167,  43, 172, 9, 129, 22, 39, 253,  19, 98, 108, 110, 79,
        113, 224, 232, 178, 185,  112, 104, 218, 246, 97, 228, 251, 34, 242,
        193, 238, 210, 144, 12, 191, 179, 162, 241,  81, 51, 145, 235, 249, 14,
        239, 107, 49, 192, 214,  31, 181, 199, 106, 157, 184,  84, 204, 176,
        115, 121, 50, 45, 127,  4, 150, 254, 138, 236, 205, 93, 222, 114, 67,
        29, 24, 72, 243, 141, 128, 195, 78, 66, 215, 61, 156, 180)

    val grad3: Array[Array[Int]] = Array(
        Array(0,1,1), Array(0,1,-1), Array(0,-1,1), Array(0,-1,-1),
        Array(1,0,1), Array(1,0,-1), Array(-1,0,1), Array(-1,0,-1),
        Array(1,1,0), Array(1,-1,0), Array(-1,1,0), Array(-1,-1,0),
        Array(1,0,-1), Array(-1,0,-1), Array(0,-1,1), Array(0,1,1))

    val grad4: Array[Array[Int]] = Array(
        Array(0,1,1,1), Array(0,1,1,-1), Array(0,1,-1,1), Array(0,1,-1,-1),
        Array(0,-1,1,1), Array(0,-1,1,-1), Array(0,-1,-1,1), Array(0,-1,-1,-1),
        Array(1,0,1,1), Array(1,0,1,-1), Array(1,0,-1,1), Array(1,0,-1,-1),
        Array(-1,0,1,1), Array(-1,0,1,-1), Array(-1,0,-1,1), Array(-1,0,-1,-1),
        Array(1,1,0,1), Array(1,1,0,-1), Array(1,-1,0,1), Array(1,-1,0,-1),
        Array(-1,1,0,1), Array(-1,1,0,-1), Array(-1,-1,0,1), Array(-1,-1,0,-1),
        Array(1,1,1,0), Array(1,1,-1,0), Array(1,-1,1,0), Array(1,-1,-1,0),
        Array(-1,1,1,0), Array(-1,1,-1,0), Array(-1,-1,1,0), Array(-1,-1,-1,0))

    // STUB START
    val tperm = new Array[Vec4d](256*256)
    initTperm()
    val tgrad = new Array[Vec4d](256*256)
    initTgrad()

    def convert(u: Vec4i) :Vec4d = {
        Vec4d(
            u.x / 256d,
            u.y / 256d,
            u.z / 256d,
            u.w / 256d
        )
    }

    def initTperm() {
        for(i <- 0 until 256)
            for(j <- 0 until 256) {
                val v = perm((j + perm(i)) & 0xFF)
                tperm(i*256 + j) = convert(Vec4i(
                    grad3(v & 0x0F)(0)*64 + 64,
                    grad3(v & 0x0F)(1)*64 + 64,
                    grad3(v & 0x0F)(2)*64 + 64,
                    v
                ))
        }
    }

    def initTgrad() {
        for(i <- 0 until 256)
            for(j <- 0 until 256) {
                val v = perm((j + perm(i)) & 0xFF)
                tgrad(i*256 + j) = convert(Vec4i(
                        grad4(v & 0x1F)(0)*64 + 64,
                        grad4(v & 0x1F)(1)*64 + 64,
                        grad4(v & 0x1F)(2)*64 + 64,
                        grad4(v & 0x1F)(3)*64 + 64
                ))
        }
    }

    val permTexture = 1
    val gradTexture = 2
    def texture2D(texture: Int, u: Vec2d): Vec4d = {
        val c = Vec2i(u*256)%256
        if (c.x < 0) c.x = 256 + c.x
        if (c.y < 0) c.y = 256 + c.y
        
        texture match {
            case `permTexture` => tperm(c.y*256 + c.x)
            case `gradTexture` => tgrad(c.y*256 + c.x)
        }
    }
    // STUB END

    
    /*
     * Efficient simplex indexing functions by Bill Licea-Kane, ATI. Thanks!
     * (This was originally implemented as a texture lookup. Nice to avoid that.)
     */
    def simplex(P: AnyVec3d, offset1: Vec3d, offset2: Vec3d) {
        val offset0 = Vec3d(0)

        val isX = step ( P.yz, P.xx )         // P.x >= P.y ? 1.0 : 0.0;  P.x >= P.z ? 1.0 : 0.0;
        offset0.x  = dot( isX, Vec2d( 1.0 ) )  // Accumulate all P.x >= other channels in offset.x
        offset0.yz = 1.0 - isX                // Accumulate all P.x <  other channels in offset.yz

        val isY = step( P.z, P.y )          // P.y >= P.z ? 1.0 : 0.0;
        offset0.y += isY                      // Accumulate P.y >= P.z in offset.y
        offset0.z += 1.0 - isY                // Accumulate P.y <  P.z in offset.z

        // offset0 now contains the unique values 0,1,2 in each channel
        // 2 for the channel greater than other channels
        // 1 for the channel that is less than one but greater than another
        // 0 for the channel less than other channels
        // Equality ties are broken in favor of first x, then y
        // (z always loses ties)

        offset2 := clamp( offset0, 0.0, 1.0 )
        // offset2 contains 1 in each channel that was 1 or 2
        offset0 -= 1
        offset1 := clamp( offset0, 0.0, 1.0 )
        // offset1 contains 1 in the single channel that was 1
    }

    def simplex( P: AnyVec4d, offset1: Vec4d, offset2: Vec4d, offset3: Vec4d ) =
    {
        val offset0 = Vec4d(0)

        val isX = step( P.yzw, P.xxx )        // See comments in 3D simplex function
        offset0.x = dot( isX, Vec3d( 1.0 ) )
        offset0.yzw = 1.0 - isX

        val isY = step( P.zw, P.yy )
        offset0.y += dot( isY, Vec2d( 1.0 ) )
        offset0.zw += 1.0 - isY

        val isZ = step( P.w, P.z )
        offset0.z += isZ
        offset0.w += 1.0 - isZ

        // offset0 now contains the unique values 0,1,2,3 in each channel

        offset3 := clamp( offset0, 0.0, 1.0 )
        offset0 -= 1
        offset2 := clamp( offset0, 0.0, 1.0 )
        offset0 -= 1
        offset1 := clamp( offset0, 0.0, 1.0 )
    }


    val ONE = 1/256d

    // Skew and unskew factors are a bit hairy for 2D, so define them as constants
    val F2 = (sqrt(3.0)-1.0)/2.0
    val G2 = (3.0-sqrt(3.0))/6.0

    /*
     * 2D simplex noise. Somewhat slower but much better looking than classic noise.
     */
    def noise1(P: AnyVec2d) :Double = {
        // Skew the (x,y) space to determine which cell of 2 simplices we're in
        val s = (P.x + P.y) * F2 // Hairy factor for 2D skewing
        var Pi = floor(P + s)
        val t = (Pi.x + Pi.y) * G2 // Hairy factor for unskewing
        val P0 = Pi - t // Unskew the cell origin back to (x,y) space
        Pi = Pi * ONE // Integer part, scaled and offset for texture lookup

        val Pf0 = P - P0  // The x,y distances from the cell origin

        // For the 2D case, the simplex shape is an equilateral triangle.
        // Find out whether we are above or below the x=y diagonal to
        // determine which of the two triangles we're in.
        var o1: Vec2d = null
        if(Pf0.x >= Pf0.y) o1 = Vec2d(1.0, 0.0) // +x, +y traversal order
        else o1 = Vec2d(0.0, 1.0)               // +y, +x traversal order

        // Noise contribution from simplex origin
        val grad0 = texture2D(permTexture, Pi).rg * 4.0 - 1.0
        var t0 = 0.5 - dot(Pf0, Pf0)
        var n0: Double = 0
        if (t0 < 0.0) n0 = 0.0
        else {
            t0 *= t0
            n0 = t0 * t0 * dot(grad0, Pf0)
        }

        // Noise contribution from middle corner
        val Pf1 = Pf0 - o1 + G2
        val grad1 = texture2D(permTexture, Pi + o1*ONE).rg * 4.0 - 1.0
        var t1 = 0.5 - dot(Pf1, Pf1);
        var n1: Double = 0
        if (t1 < 0.0) n1 = 0.0
        else {
            t1 *= t1
            n1 = t1 * t1 * dot(grad1, Pf1)
        }

        // Noise contribution from last corner
        val Pf2 = Pf0 - Vec2d(1.0-2.0*G2)
        val grad2 = texture2D(permTexture, Pi + Vec2d(ONE, ONE)).rg * 4.0 - 1.0
        var t2 = 0.5 - dot(Pf2, Pf2)
        var n2: Double = 0
        if(t2 < 0.0) n2 = 0.0
        else {
            t2 *= t2
            n2 = t2 * t2 * dot(grad2, Pf2)
        }

        // Sum up and scale the result to cover the range [-1,1]
        70.0 * (n0 + n1 + n2)
    }

    // The skewing and unskewing factors are much simpler for the 3D case
    val F3 = 1/3d
    val G3 = 1/6d

    /*
     * 3D simplex noise. Comparable in speed to classic noise, better looking.
     */
    def noise1(P: AnyVec3d) :Double = {
        // Skew the (x,y,z) space to determine which cell of 6 simplices we're in
        val s = (P.x + P.y + P.z) * F3 // Factor for 3D skewing
        var Pi = floor(P + s)
        val t = (Pi.x + Pi.y + Pi.z) * G3
        val P0 = Pi - t // Unskew the cell origin back to (x,y,z) space
        Pi = Pi * ONE // Integer part, scaled and offset for texture lookup

        val Pf0 = P - P0 // The x,y distances from the cell origin

        // For the 3D case, the simplex shape is a slightly irregular tetrahedron.
        // To find out which of the six possible tetrahedra we're in, we need to
        // determine the magnitude ordering of x, y and z components of Pf0.
        var o1: Vec3d = Vec3d(0)
        var o2: Vec3d = Vec3d(0)
        simplex(Pf0, o1, o2)

        // Noise contribution from simplex origin
        val perm0 = texture2D(permTexture, Pi.xy).a
        val grad0 = texture2D(permTexture, Vec2d(perm0, Pi.z)).rgb * 4.0 - 1.0
        var t0 = 0.6 - dot(Pf0, Pf0)
        var n0 = 0d
        if (t0 < 0.0) n0 = 0.0
        else {
            t0 *= t0
            n0 = t0 * t0 * dot(grad0, Pf0)
        }

        // Noise contribution from second corner
        val Pf1 = Pf0 - o1 + G3
        val perm1 = texture2D(permTexture, Pi.xy + o1.xy*ONE).a
        val grad1 = texture2D(permTexture, Vec2d(perm1, Pi.z + o1.z*ONE)).rgb * 4.0 - 1.0
        var t1 = 0.6 - dot(Pf1, Pf1)
        var n1 = 0d
        if (t1 < 0.0) n1 = 0.0
        else {
            t1 *= t1
            n1 = t1 * t1 * dot(grad1, Pf1)
        }

        // Noise contribution from third corner
        val Pf2 = Pf0 - o2 + 2.0 * G3
        val perm2 = texture2D(permTexture, Pi.xy + o2.xy*ONE).a
        val grad2 = texture2D(permTexture, Vec2d(perm2, Pi.z + o2.z*ONE)).rgb * 4.0 - 1.0
        var t2 = 0.6 - dot(Pf2, Pf2)
        var n2 = 0d
        if (t2 < 0.0) n2 = 0.0
        else {
            t2 *= t2
            n2 = t2 * t2 * dot(grad2, Pf2)
        }

        // Noise contribution from last corner
        val Pf3 = Pf0 - Vec3d(1.0-3.0*G3)
        val perm3 = texture2D(permTexture, Pi.xy + Vec2d(ONE, ONE)).a
        val grad3 = texture2D(permTexture, Vec2d(perm3, Pi.z + ONE)).rgb * 4.0 - 1.0
        var t3 = 0.6 - dot(Pf3, Pf3)
        var n3 = 0d
        if(t3 < 0.0) n3 = 0.0
        else {
            t3 *= t3
            n3 = t3 * t3 * dot(grad3, Pf3)
        }

        // Sum up and scale the result to cover the range [-1,1]
        32.0 * (n0 + n1 + n2 + n3)
    }

    // The skewing and unskewing factors are hairy again for the 4D case
    val F4 = (sqrt(5.0)-1.0)/4.0
    val G4 = (5.0-sqrt(5.0))/20.0

    /*
     * 4D simplex noise. A lot faster than classic 4D noise, and better looking.
     */
    def noise1(P: AnyVec4d) :Double = {
        // Skew the (x,y,z,w) space to determine which cell of 24 simplices we're in
        val s = (P.x + P.y + P.z + P.w) * F4 // Factor for 4D skewing
        var Pi = floor(P + s)
        val t = (Pi.x + Pi.y + Pi.z + Pi.w) * G4
        val P0 = Pi - t // Unskew the cell origin back to (x,y,z,w) space
        Pi = Pi * ONE // Integer part, scaled and offset for texture lookup

        val Pf0 = P - P0  // The x,y distances from the cell origin

        // For the 4D case, the simplex is a 4D shape I won't even try to describe.
        // To find out which of the 24 possible simplices we're in, we need to
        // determine the magnitude ordering of x, y, z and w components of Pf0.
        var o1 = Vec4d(0)
        var o2 = Vec4d(0)
        var o3 = Vec4d(0)
        simplex(Pf0, o1, o2, o3)

        // Noise contribution from simplex origin
        val perm0xy = texture2D(permTexture, Pi.xy).a
        val perm0zw = texture2D(permTexture, Pi.zw).a
        val grad0 = texture2D(gradTexture, Vec2d(perm0xy, perm0zw)).rgba * 4.0 - 1.0
        var t0 = 0.6 - dot(Pf0, Pf0)
        var n0 = 0d
        if (t0 < 0.0) n0 = 0.0
        else {
            t0 *= t0
            n0 = t0 * t0 * dot(grad0, Pf0)
        }

        // Noise contribution from second corner
        val Pf1 = Pf0 - o1 + G4
        o1 = o1 * ONE
        val perm1xy = texture2D(permTexture, Pi.xy + o1.xy).a
        val perm1zw = texture2D(permTexture, Pi.zw + o1.zw).a
        val grad1 = texture2D(gradTexture, Vec2d(perm1xy, perm1zw)).rgba * 4.0 - 1.0
        var t1 = 0.6 - dot(Pf1, Pf1)
        var n1 = 0d
        if (t1 < 0.0) n1 = 0.0
        else {
            t1 *= t1
            n1 = t1 * t1 * dot(grad1, Pf1)
        }

        // Noise contribution from third corner
        val Pf2 = Pf0 - o2 + 2.0 * G4
        o2 = o2 * ONE
        val perm2xy = texture2D(permTexture, Pi.xy + o2.xy).a
        val perm2zw = texture2D(permTexture, Pi.zw + o2.zw).a
        val grad2 = texture2D(gradTexture, Vec2d(perm2xy, perm2zw)).rgba * 4.0 - 1.0
        var t2 = 0.6 - dot(Pf2, Pf2)
        var n2 = 0d
        if (t2 < 0.0) n2 = 0.0
        else {
            t2 *= t2
            n2 = t2 * t2 * dot(grad2, Pf2)
        }

        // Noise contribution from fourth corner
        val Pf3 = Pf0 - o3 + 3.0 * G4
        o3 = o3 * ONE
        val perm3xy = texture2D(permTexture, Pi.xy + o3.xy).a
        val perm3zw = texture2D(permTexture, Pi.zw + o3.zw).a
        val grad3 = texture2D(gradTexture, Vec2d(perm3xy, perm3zw)).rgba * 4.0 - 1.0
        var t3 = 0.6 - dot(Pf3, Pf3)
        var n3 = 0d
        if (t3 < 0.0) n3 = 0.0
        else {
            t3 *= t3
            n3 = t3 * t3 * dot(grad3, Pf3)
        }

        // Noise contribution from last corner
        val Pf4 = Pf0 - Vec4d(1.0-4.0*G4)
        val perm4xy = texture2D(permTexture, Pi.xy + Vec2d(ONE, ONE)).a
        val perm4zw = texture2D(permTexture, Pi.zw + Vec2d(ONE, ONE)).a
        val grad4 = texture2D(gradTexture, Vec2d(perm4xy, perm4zw)).rgba * 4.0 - 1.0
        var t4 = 0.6 - dot(Pf4, Pf4)
        var n4 = 0d
        if(t4 < 0.0) n4 = 0.0
        else {
            t4 *= t4
            n4 = t4 * t4 * dot(grad4, Pf4)
        }

        // Sum up and scale the result to cover the range [-1,1]
        27.0 * (n0 + n1 + n2 + n3 + n4)
    }
}
