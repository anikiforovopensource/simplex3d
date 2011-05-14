/*
 * Simplex3d, SimplexNoise
 * Copyright (C) 2010-2011, Aleksey Nikiforov
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
package simplex3d.math


/** <code>SimplexNoise</code> contains the implementation
 * of simplex noise algorithm.
 *
 * @author Stefan Gustavson, ITN-LiTH
 * @author Bill Licea-Kane, ATI
 *
 * Ported to Scala, implemented 1D noise, improved scaling constants, added seed.
 * @author Aleksey Nikiforov (lex)
 */
class SimplexNoise(val seed: Long) {

  import SimplexNoise._

  
  private final val permArray: Array[Byte] = {

    val array = Array[Byte](
      -105, -96, -119, 91, 90, 15, -125, 13, -55, 95, 96, 53, -62, -23, 7, -31, -116,
      36, 103, 30, 69, -114, 8, 99, 37, -16, 21, 10, 23, -66, 6, -108, -9, 120, -22,
      75, 0, 26, -59, 62, 94, -4, -37, -53, 117, 35, 11, 32, 57, -79, 33, 88, -19,
      -107, 56, 87, -82, 20, 125, -120, -85, -88, 68, -81, 74, -91, 71, -122, -117,
      48, 27, -90, 77, -110, -98, -25, 83, 111, -27, 122, 60, -45, -123, -26, -36,
      105, 92, 41, 55, 46, -11, 40, -12, 102, -113, 54, 65, 25, 63, -95, 1, -40, 80,
      73, -47, 76, -124, -69, -48, 89, 18, -87, -56, -60, -121, -126, 116, -68, -97,
      86, -92, 100, 109, -58, -83, -70, 3, 64, 52, -39, -30, -6, 124, 123, 5, -54, 38,
      -109, 118, 126, -1, 82, 85, -44, -49, -50, 59, -29, 47, 16, 58, 17, -74, -67,
      28, 42, -33, -73, -86, -43, 119, -8, -104, 2, 44, -102, -93, 70, -35, -103, 101,
      -101, -89, 43, -84, 9, -127, 22, 39, -3, 19, 98, 108, 110, 79, 113, -32, -24,
      -78, -71, 112, 104, -38, -10, 97, -28, -5, 34, -14, -63, -18, -46, -112, 12,
      -65, -77, -94, -15, 81, 51, -111, -21, -7, 14, -17, 107, 49, -64, -42, 31, -75,
      -57, 106, -99, -72, 84, -52, -80, 115, 121, 50, 45, 127, 4, -106, -2, -118, -20,
      -51, 93, -34, 114, 67, 29, 24, 72, -13, -115, -128, -61, 78, 66, -41, 61, -100, -76
    )

    val random = new java.util.Random(seed)
    var i = 0; while (i < array.length) {
      val randomIndex = random.nextInt(array.length)

      val tmp = array(i)
      array(i) = array(randomIndex)
      array(randomIndex) = tmp

      i += 1
    }

    array
  }

  private final def perm(i: Int) = {
    val xx = i ^ (i >> 16)
    permArray((xx ^ (xx >> 8)) & 0xFF)
  }
  

  // The implementation is restricted to arguments in range [-2E8, +2E8] when using Ints.
  private final def ifloor(x: Double) :Long = {
    val i = x.toLong
    if (x > 0 || x == i) i else i - 1
  }


  /** Computes 1D simplex noise.
   * @param x x coordinate
   * @return simplex noise value for the specified coordinate.
   */
  final def noise(x: Double) :Double = {
    // The implementation is restricted to arguments in range [-2E8, +2E8] when using Ints.
    val pix = ifloor(x*F1)

    // The x distance from the cell origin.
    val p0x = x - pix*G1

    val ix = pix.toInt

    // For the 1D case, the simplex shape is an interval of length 1.

    // Noise contribution from left point.
    val t0 = 0.5 - p0x*p0x
    val n0 =
      if (t0 < 0.0) 0.0
      else {
        val px = perm(ix)
        // Gradient function, produces ints in [-8, 8] excluding 0 from perm.
        val grad = if ((px & 0x8) == 0) ((px & 0x7) + 1) else (px | 0xFFFFFFF8)
        val t = t0 * t0
        t * t * grad*p0x
      }

    // Noise contribution from right point.
    val p1x = p0x - G1
    val t1 = 0.5 - p1x*p1x
    val n1 =
      if (t1 < 0.0) 0.0
      else {
        val px = perm(ix + 1)
        // Gradient function, produces ints in [-8, 8] excluding 0 from perm.
        val grad = if ((px & 0x8) == 0) ((px & 0x7) + 1) else (px | 0xFFFFFFF8)
        val t = t1 * t1
        t * t * grad*p1x
      }

    // The maximum noise value is at the center of the interval with grad0 = 8 and grad1 = -8.
    // The length of the interval is 1/(sqrt(2).
    // scale = 1/((0.5 - (1/(sqrt(2)*2))^2)^4*8*(1/(sqrt(2)*2))*2) = 8.93922646833363783934

    // Sum up and scale the result to cover the range [-1,1].
    8.93922646833363783934 * (n0 + n1)
  }
  
  
  /** Computes 2D simplex noise. Somewhat slower but much better looking
   * than classic (Perlin) noise.
   * @param x x coordinate
   * @param y y coordinate
   * @return simplex noise value for the specified coordinates.
   */
  final def noise(x: Double, y: Double) :Double = {
    // Skew the (x,y) space to determine which cell of 2 simplices we're in.
    val s = (x + y) * F2 // Hairy factor for 2D skewing.

    // The implementation is restricted to arguments in range [-2E8, +2E8] when using Ints.
    val pix = ifloor(x + s)
    val piy = ifloor(y + s)
    val t = (pix + piy) * G2 // Hairy factor for unskewing.

    // The x,y distances from the cell origin.
    val p0x = x - pix + t
    val p0y = y - piy + t

    val ix = pix.toInt
    val iy = piy.toInt

    // For the 2D case, the simplex shape is an equilateral triangle.
    // Find out whether we are above or below the x=y diagonal to
    // determine which of the two triangles we're in.
    val o1x = if(p0x >= p0y) 1 else 0
    val o1y = 1 - o1x

    // Noise contribution from simplex origin.
    val t0 = 0.5 - p0x*p0x - p0y*p0y
    val n0 =
      if (t0 < 0.0) 0.0
      else {
        val py = perm(iy)
        val px = perm(ix + py)
        val grad = grad3(px & 0x0F)
        val t = t0 * t0
        t * t * (grad(0)*p0x + grad(1)*p0y)
      }

    // Noise contribution from middle corner.
    val p1x = p0x - o1x + G2
    val p1y = p0y - o1y + G2
    val t1 = 0.5 - p1x*p1x - p1y*p1y
    val n1 =
      if (t1 < 0.0) 0.0
      else {
        val py = perm(iy + o1y)
        val px = perm(ix + o1x + py)
        val grad = grad3(px & 0x0F)
        val t = t1 * t1
        t * t * (grad(0)*p1x + grad(1)*p1y)
      }

    // Noise contribution from last corner.
    val p2x = p0x - G22
    val p2y = p0y - G22
    val t2 = 0.5 - p2x*p2x - p2y*p2y
    val n2 =
      if(t2 < 0.0) 0.0
      else {
        val py = perm(iy + 1)
        val px = perm(ix + 1 + py)
        val grad = grad3(px & 0x0F)
        val t = t2 * t2
        t * t * (grad(0)*p2x + grad(1)*p2y)
      }

    // The maximum noise value is at the center of the interval between p0 and p2,
    // with gradient vectors maximizing the gradient sum,
    // for example: (grad(0)*p0x + grad(1)*p0y) = abs(p0x) + abs(p0y).
    // p1 is out of range of influence and is discarded. The formula then becomes:
    // scale = 1/((0.5 - 2*G22*G22/4)^4*(4*G22/2))

    // Sum up and scale the result to cover the range [-1,1].
    70.14805770653953038786 * (n0 + n1 + n2)
  }


  /** Computes 3D simplex noise. Comparable in speed to classic (Perlin) noise,
   * better looking.
   * @param x x coordinate
   * @param y y coordinate
   * @param z z coordinate
   * @return simplex noise value for the specified coordinates.
   */
  final def noise(x: Double, y: Double, z:Double) :Double = {
    // Skew the (x,y,z) space to determine which cell of 6 simplices we're in.
    val s = (x + y + z) * F3 // Factor for 3D skewing.

    // The implementation is restricted to arguments in range [-2E8, +2E8] when using Ints.
    val pix = ifloor(x + s)
    val piy = ifloor(y + s)
    val piz = ifloor(z + s)
    val t = (pix + piy + piz) * G3

    // The x,y,z distances from the cell origin.
    val p0x = x - pix + t
    val p0y = y - piy + t
    val p0z = z - piz + t

    val ix = pix.toInt
    val iy = piy.toInt
    val iz = piz.toInt

    // For the 3D case, the simplex shape is a slightly irregular tetrahedron.
    // To find out which of the six possible tetrahedra we're in, we need to
    // determine the magnitude ordering of x, y and z components of p0.
    var o1x = 0; var o1y = 0; var o1z = 0
    var o2x = 0; var o2y = 0; var o2z = 0

    if (p0x >= p0y) {
      if (p0y >= p0z) {
        o1x = 1; o1y = 0; o1z = 0
        o2x = 1; o2y = 1; o2z = 0
      }
      else if (p0x >= p0z) {
        o1x = 1; o1y = 0; o1z = 0
        o2x = 1; o2y = 0; o2z = 1
      }
      else {
        o1x = 0; o1y = 0; o1z = 1
        o2x = 1; o2y = 0; o2z = 1
      }
    } else {
      if (p0y < p0z) {
        o1x = 0; o1y = 0; o1z = 1
        o2x = 0; o2y = 1; o2z = 1
      }
      else if (p0x < p0z) {
        o1x = 0; o1y = 1; o1z = 0
        o2x = 0; o2y = 1; o2z = 1
      }
      else {
        o1x = 0; o1y = 1; o1z = 0
        o2x = 1; o2y = 1; o2z = 0
      }
    }

    // Noise contribution from simplex origin.
    val t0 = 0.5 - p0x*p0x - p0y*p0y - p0z*p0z
    val n0 =
      if (t0 < 0.0) 0.0
      else {
        val pz = perm(iz)
        val py = perm(iy + pz)
        val px = perm(ix + py)
        val grad = grad3(px & 0x0F)
        val t = t0 * t0
        t * t * (grad(0)*p0x + grad(1)*p0y + grad(2)*p0z)
      }

    // Noise contribution from second corner.
    val p1x = p0x - o1x + G3
    val p1y = p0y - o1y + G3
    val p1z = p0z - o1z + G3
    val t1 = 0.5 - p1x*p1x - p1y*p1y - p1z*p1z
    val n1 =
      if (t1 < 0.0) 0.0
      else {
        val pz = perm(iz + o1z)
        val py = perm(iy + o1y + pz)
        val px = perm(ix + o1x + py)
        val grad = grad3(px & 0x0F)
        val t = t1 * t1
        t * t * (grad(0)*p1x + grad(1)*p1y + grad(2)*p1z)
      }

    // Noise contribution from third corner.
    val p2x = p0x - o2x + G32
    val p2y = p0y - o2y + G32
    val p2z = p0z - o2z + G32
    val t2 = 0.5 - p2x*p2x - p2y*p2y - p2z*p2z
    val n2 =
      if (t2 < 0.0) 0.0
      else {
        val pz = perm(iz + o2z)
        val py = perm(iy + o2y + pz)
        val px = perm(ix + o2x + py)
        val grad = grad3(px & 0x0F)
        val t = t2 * t2
        t * t * (grad(0)*p2x + grad(1)*p2y + grad(2)*p2z)
      }

    // Noise contribution from last corner.
    val p3x = p0x - G33
    val p3y = p0y - G33
    val p3z = p0z - G33
    val t3 = 0.5 - p3x*p3x - p3y*p3y - p3z*p3z
    val n3 =
      if(t3 < 0.0) 0.0
      else {
        val pz = perm(iz + 1)
        val py = perm(iy + 1 + pz)
        val px = perm(ix + 1 + py)
        val grad = grad3(px & 0x0F)
        val t = t3 * t3
        t * t * (grad(0)*p3x + grad(1)*p3y + grad(2)*p3z)
      }

    // This formula is a guess based on a numeric search for max value.
    // max value point (after floor): (1/2, 2/3, 2/3)
    // scale = 1/((0.5 - (1/6)*(1/6) - (1/6)*(1/6))^4*((1/6) + (1/6)))

    // Sum up and scale the result to cover the range [-1,1].
    76.88671875 * (n0 + n1 + n2 + n3)
  }


  /** Computes 4D simplex noise. A lot faster than classic (Perlin) 4D noise,
   * and better looking.
   * @param x x coordinate
   * @param y y coordinate
   * @param z z coordinate
   * @param w w coordinate
   * @return simplex noise value for the specified coordinates.
   */
  final def noise(x: Double, y: Double, z:Double, w:Double) :Double = {
    // Skew the (x,y,z,w) space to determine which cell of 24 simplices we're in.
    val s = (x + y + z + w) * F4 // Factor for 4D skewing.

    // The implementation is restricted to arguments in range [-2E8, +2E8] when using Ints.
    val pix = ifloor(x + s)
    val piy = ifloor(y + s)
    val piz = ifloor(z + s)
    val piw = ifloor(w + s)
    val t = (pix + piy + piz + piw) * G4

    // The x,y,z,w distances from the cell origin.
    val p0x = x - pix + t
    val p0y = y - piy + t
    val p0z = z - piz + t
    val p0w = w - piw + t

    val ix = pix.toInt
    val iy = piy.toInt
    val iz = piz.toInt
    val iw = piw.toInt

    // For the 4D case, the simplex is a 4D shape I won't even try to describe.
    // To find out which of the 24 possible simplices we're in, we need to
    // determine the magnitude ordering of x, y, z and w components of p0.
    var bx = 0; var by = 0; var bz = 0; var bw = 0
    if (p0x >= p0y) bx += 1 else by += 1
    if (p0x >= p0z) bx += 1 else bz += 1
    if (p0x >= p0w) bx += 1 else bw += 1
    if (p0y >= p0z) by += 1 else bz += 1
    if (p0y >= p0w) by += 1 else bw += 1
    if (p0z >= p0w) bz += 1 else bw += 1

    val o3x = if (bx > 0) 1 else 0
    val o3y = if (by > 0) 1 else 0
    val o3z = if (bz > 0) 1 else 0
    val o3w = if (bw > 0) 1 else 0

    val o2x = if (bx > 1) 1 else 0
    val o2y = if (by > 1) 1 else 0
    val o2z = if (bz > 1) 1 else 0
    val o2w = if (bw > 1) 1 else 0

    val o1x = if (bx > 2) 1 else 0
    val o1y = if (by > 2) 1 else 0
    val o1z = if (bz > 2) 1 else 0
    val o1w = if (bw > 2) 1 else 0

    // Noise contribution from simplex origin.
    val t0 = 0.5 - p0x*p0x - p0y*p0y - p0z*p0z - p0w*p0w
    val n0 =
      if (t0 < 0.0) 0.0
      else {
        val pw = perm(iw)
        val pz = perm(iz + pw)
        val py = perm(iy + pz)
        val px = perm(ix + py)
        val grad = grad4(px & 0x1F)
        val t = t0 * t0
        t * t * (grad(0)*p0x + grad(1)*p0y + grad(2)*p0z + grad(3)*p0w)
      }

    // Noise contribution from second corner.
    val p1x = p0x - o1x + G4
    val p1y = p0y - o1y + G4
    val p1z = p0z - o1z + G4
    val p1w = p0w - o1w + G4
    val t1 = 0.5 - p1x*p1x - p1y*p1y - p1z*p1z - p1w*p1w
    val n1 =
      if (t1 < 0.0) 0.0
      else {
        val pw = perm(iw + o1w)
        val pz = perm(iz + o1z + pw)
        val py = perm(iy + o1y + pz)
        val px = perm(ix + o1x + py)
        val grad = grad4(px & 0x1F)
        val t = t1 * t1
        t * t * (grad(0)*p1x + grad(1)*p1y + grad(2)*p1z + grad(3)*p1w)
      }

    // Noise contribution from third corner.
    val p2x = p0x - o2x + G42
    val p2y = p0y - o2y + G42
    val p2z = p0z - o2z + G42
    val p2w = p0w - o2w + G42
    val t2 = 0.5 - p2x*p2x - p2y*p2y - p2z*p2z - p2w*p2w
    val n2 =
      if (t2 < 0.0) 0.0
      else {
        val pw = perm(iw + o2w)
        val pz = perm(iz + o2z + pw)
        val py = perm(iy + o2y + pz)
        val px = perm(ix + o2x + py)
        val grad = grad4(px & 0x1F)
        val t = t2 * t2
        t * t * (grad(0)*p2x + grad(1)*p2y + grad(2)*p2z + grad(3)*p2w)
      }

    // Noise contribution from fourth corner.
    val p3x = p0x - o3x + G43
    val p3y = p0y - o3y + G43
    val p3z = p0z - o3z + G43
    val p3w = p0w - o3w + G43
    val t3 = 0.5 - p3x*p3x - p3y*p3y - p3z*p3z - p3w*p3w
    val n3 =
      if (t3 < 0.0) 0.0
      else {
        val pw = perm(iw + o3w)
        val pz = perm(iz + o3z + pw)
        val py = perm(iy + o3y + pz)
        val px = perm(ix + o3x + py)
        val grad = grad4(px & 0x1F)
        val t = t3 * t3
        t * t * (grad(0)*p3x + grad(1)*p3y + grad(2)*p3z + grad(3)*p3w)
      }

    // Noise contribution from last corner.
    val p4x = p0x - G44
    val p4y = p0y - G44
    val p4z = p0z - G44
    val p4w = p0w - G44
    val t4 = 0.5 - p4x*p4x - p4y*p4y - p4z*p4z - p4w*p4w
    val n4 =
      if(t4 < 0.0) 0.0
      else {
        val pw = perm(iw + 1)
        val pz = perm(iz + 1 + pw)
        val py = perm(iy + 1 + pz)
        val px = perm(ix + 1 + py)
        val grad = grad4(px & 0x1F)
        val t = t4 * t4
        t * t * (grad(0)*p4x + grad(1)*p4y + grad(2)*p4z + grad(3)*p4w)
      }

    // This number is a result of a numeric search for max value.
    // max value point (after floor): Vec4(0, Vec3(0.13608593378889))
    // Discarding digits will not affect scale after 0.13608593000000.

    // Sum up and scale the result to cover the range [-1,1].
    62.777711789695920 * (n0 + n1 + n2 + n3 + n4)
  }
}

object SimplexNoise extends SimplexNoise(0) {

  final val frequency = 0.7071067811865475244

  // Offset values
  final val offset1 = 7.2*0.7071067811865475244
  final val offset2 = 15.9*0.7071067811865475244
  final val offset3 = 22.3*0.7071067811865475244


  private final val grad3: Array[Array[Byte]] = Array(
    Array(0,1,1), Array(0,1,-1), Array(0,-1,1), Array(0,-1,-1),
    Array(1,0,1), Array(1,0,-1), Array(-1,0,1), Array(-1,0,-1),
    Array(1,1,0), Array(1,-1,0), Array(-1,1,0), Array(-1,-1,0),
    Array(1,0,-1), Array(-1,0,-1), Array(0,-1,1), Array(0,1,1)
  )

  private final val grad4: Array[Array[Byte]] = Array(
    Array(0,1,1,1), Array(0,1,1,-1), Array(0,1,-1,1), Array(0,1,-1,-1),
    Array(0,-1,1,1), Array(0,-1,1,-1), Array(0,-1,-1,1), Array(0,-1,-1,-1),
    Array(1,0,1,1), Array(1,0,1,-1), Array(1,0,-1,1), Array(1,0,-1,-1),
    Array(-1,0,1,1), Array(-1,0,1,-1), Array(-1,0,-1,1), Array(-1,0,-1,-1),
    Array(1,1,0,1), Array(1,1,0,-1), Array(1,-1,0,1), Array(1,-1,0,-1),
    Array(-1,1,0,1), Array(-1,1,0,-1), Array(-1,-1,0,1), Array(-1,-1,0,-1),
    Array(1,1,1,0), Array(1,1,-1,0), Array(1,-1,1,0), Array(1,-1,-1,0),
    Array(-1,1,1,0), Array(-1,1,-1,0), Array(-1,-1,1,0), Array(-1,-1,-1,0)
  )

  // Skew and unskew factors making the noise frequency consistent with 2D/3D/4D.
  private final val F1 = 1.4142135623730950488 //Math.sqrt(2.0)
  private final val G1 = 0.7071067811865475244 //1 / Math.sqrt(2.0)

  // Skew and unskew factors are a bit hairy for 2D, so define them as constants.
  private final val F2 = 0.36602540378443864676 //(Math.sqrt(3.0) - 1.0) / 2.0
  private final val G2 = 0.21132486540518711775 //(3.0 - Math.sqrt(3.0)) / 6.0
  private final val G22 = 0.57735026918962576451 //1 - 2.0 * (3.0 - Math.sqrt(3.0)) / 6.0

  // The skewing and unskewing factors are much simpler for the 3D case.
  private final val F3 = 1 / 3.0
  private final val G3 = 1 / 6.0
  private final val G32 = 2 / 6.0
  private final val G33 = 1 - 3 / 6.0

  // The skewing and unskewing factors are hairy again for the 4D case.
  private final val F4 = 0.3090169943749474241 //(Math.sqrt(5.0) - 1.0) / 4.0
  private final val G4 = 0.13819660112501051518 //(5.0 - Math.sqrt(5.0)) / 20.0
  private final val G42 = 0.27639320225002103036 //2.0 * ((5.0 - Math.sqrt(5.0)) / 20.0)
  private final val G43 = 0.41458980337503154554 //3.0 * ((5.0 - Math.sqrt(5.0)) / 20.0)
  private final val G44 = 0.44721359549995793928 //1 - 4.0 * ((5.0 - Math.sqrt(5.0)) / 20.0)
}
