/*
 * This code is originally released by Stefan Gustavson in his paper located at
 * http://staffwww.itn.liu.se/~stegu/simplexnoise/simplexnoise.pdf
 *
 * Modifications by Karsten Schmidt.
 * Used under LGPL v2.1. and ported to Scala by Aleksey Nikiforov.
 */

package ext.toxi.math.noise

import java.lang.Math
import simplex3d.math.BaseMath


/**
 * Simplex Noise in 2D, 3D and 4D. Based on the example code of this paper:
 * http://staffwww.itn.liu.se/~stegu/simplexnoise/simplexnoise.pdf
 *
 * @author Stefan Gustavson, Linkping University, Sweden (stegu at itn dot liu
 *         dot se)
 *
 *         Slight optimizations & restructuring by
 * @author Karsten Schmidt (info at toxi dot co dot uk)
 *
 *         Ported to Scala
 * @author Aleksey Nikiforov (lex)
 */
object SimplexNoise {
    private val SQRT3: Double = Math.sqrt(3.0)
    private val SQRT5: Double = Math.sqrt(5.0)

    /**
     * Skewing and unskewing factors for 2D, 3D and 4D, some of them
     * pre-multiplied.
     */
    private val F2: Double = 0.5 * (SQRT3 - 1.0)
    private val G2: Double = (3.0 - SQRT3) / 6.0
    private val G22: Double = G2 * 2.0 - 1

    private val F3: Double = 1.0 / 3.0
    private val G3: Double = 1.0 / 6.0

    private val F4: Double = (SQRT5 - 1.0) / 4.0
    private val G4: Double = (5.0 - SQRT5) / 20.0
    private val G42: Double = G4 * 2.0
    private val G43: Double = G4 * 3.0
    private val G44: Double = G4 * 4.0 - 1.0

    /**
     * Gradient vectors for 3D (pointing to mid points of all edges of a unit
     * cube)
     */
    private val grad3: Array[Array[Int]] = Array(
        Array(1, 1, 0), Array(-1, 1, 0), Array(1, -1, 0), Array(-1, -1, 0),
        Array(1, 0, 1), Array(-1, 0, 1), Array(1, 0, -1), Array(-1, 0, -1),
        Array(0, 1, 1), Array(0, -1, 1), Array(0, 1, -1), Array(0, -1, -1))

    /**
     * Gradient vectors for 4D (pointing to mid points of all edges of a unit 4D
     * hypercube)
     */
    private val grad4: Array[Array[Int]] = Array(
        Array(0, 1, 1, 1), Array(0, 1, 1, -1), Array(0, 1, -1, 1),
        Array(0, 1, -1, -1), Array(0, -1, 1, 1), Array(0, -1, 1, -1),
        Array(0, -1, -1, 1), Array(0, -1, -1, -1), Array(1, 0, 1, 1),
        Array(1, 0, 1, -1), Array(1, 0, -1, 1), Array(1, 0, -1, -1),
        Array(-1, 0, 1, 1), Array(-1, 0, 1, -1), Array(-1, 0, -1, 1),
        Array(-1, 0, -1, -1), Array(1, 1, 0, 1), Array(1, 1, 0, -1),
        Array(1, -1, 0, 1), Array(1, -1, 0, -1), Array(-1, 1, 0, 1),
        Array(-1, 1, 0, -1), Array(-1, -1, 0, 1), Array(-1, -1, 0, -1),
        Array(1, 1, 1, 0), Array(1, 1, -1, 0), Array(1, -1, 1, 0),
        Array(1, -1, -1, 0), Array(-1, 1, 1, 0), Array(-1, 1, -1, 0),
        Array(-1, -1, 1, 0), Array(-1, -1, -1, 0))

    /**
     * Permutation table
     */
    private val p: Array[Int] = Array( 151, 160, 137, 91, 90, 15, 131, 13, 201,
            95, 96, 53, 194, 233, 7, 225, 140, 36, 103, 30, 69, 142, 8, 99, 37,
            240, 21, 10, 23, 190, 6, 148, 247, 120, 234, 75, 0, 26, 197, 62,
            94, 252, 219, 203, 117, 35, 11, 32, 57, 177, 33, 88, 237, 149, 56,
            87, 174, 20, 125, 136, 171, 168, 68, 175, 74, 165, 71, 134, 139,
            48, 27, 166, 77, 146, 158, 231, 83, 111, 229, 122, 60, 211, 133,
            230, 220, 105, 92, 41, 55, 46, 245, 40, 244, 102, 143, 54, 65, 25,
            63, 161, 1, 216, 80, 73, 209, 76, 132, 187, 208, 89, 18, 169, 200,
            196, 135, 130, 116, 188, 159, 86, 164, 100, 109, 198, 173, 186, 3,
            64, 52, 217, 226, 250, 124, 123, 5, 202, 38, 147, 118, 126, 255,
            82, 85, 212, 207, 206, 59, 227, 47, 16, 58, 17, 182, 189, 28, 42,
            223, 183, 170, 213, 119, 248, 152, 2, 44, 154, 163, 70, 221, 153,
            101, 155, 167, 43, 172, 9, 129, 22, 39, 253, 19, 98, 108, 110, 79,
            113, 224, 232, 178, 185, 112, 104, 218, 246, 97, 228, 251, 34, 242,
            193, 238, 210, 144, 12, 191, 179, 162, 241, 81, 51, 145, 235, 249,
            14, 239, 107, 49, 192, 214, 31, 181, 199, 106, 157, 184, 84, 204,
            176, 115, 121, 50, 45, 127, 4, 150, 254, 138, 236, 205, 93, 222,
            114, 67, 29, 24, 72, 243, 141, 128, 195, 78, 66, 215, 61, 156, 180)

    /**
     * To remove the need for index wrapping, double the permutation table
     * length
     */
    private val perm: Array[Int] = new Array[Int](0x200)
    /**
     * A lookup table to traverse the simplex around a given point in 4D.
     * Details can be found where this table is used, in the 4D noise method.
     */
    private val simplex: Array[Array[Int]] = Array(
     Array(0, 1, 2, 3), Array(0, 1, 3, 2),
     Array(0, 0, 0, 0), Array(0, 2, 3, 1), Array(0, 0, 0, 0), Array(0, 0, 0, 0),
     Array(0, 0, 0, 0), Array(1, 2, 3, 0), Array(0, 2, 1, 3), Array(0, 0, 0, 0),
     Array(0, 3, 1, 2), Array(0, 3, 2, 1), Array(0, 0, 0, 0), Array(0, 0, 0, 0),
     Array(0, 0, 0, 0), Array(1, 3, 2, 0), Array(0, 0, 0, 0), Array(0, 0, 0, 0),
     Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(0, 0, 0, 0),
     Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(1, 2, 0, 3), Array(0, 0, 0, 0),
     Array(1, 3, 0, 2), Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(0, 0, 0, 0),
     Array(2, 3, 0, 1), Array(2, 3, 1, 0), Array(1, 0, 2, 3), Array(1, 0, 3, 2),
     Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(2, 0, 3, 1),
     Array(0, 0, 0, 0), Array(2, 1, 3, 0), Array(0, 0, 0, 0), Array(0, 0, 0, 0),
     Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(0, 0, 0, 0),
     Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(2, 0, 1, 3), Array(0, 0, 0, 0),
     Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(3, 0, 1, 2), Array(3, 0, 2, 1),
     Array(0, 0, 0, 0), Array(3, 1, 2, 0), Array(2, 1, 0, 3), Array(0, 0, 0, 0),
     Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(3, 1, 0, 2), Array(0, 0, 0, 0),
     Array(3, 2, 0, 1), Array(3, 2, 1, 0))

    for (i <- 0 until 0x200) {
        perm(i) = p(i & 0xff)
    }

    /**
     * Computes dot product in 2D.
     *
     * @param g
     *        2-vector (grid offset)
     * @param x
     * @param y
     * @return dot product
     */
    private def dot(g: Array[Int], x: Double, y: Double) :Double = {
        g(0) * x + g(1) * y;
    }

    /**
     * Computes dot product in 3D.
     *
     * @param g
     *        3-vector (grid offset)
     * @param x
     * @param y
     * @param z
     * @return dot product
     */
    private def dot(g: Array[Int], x: Double, y: Double, z: Double) :Double = {
        g(0) * x + g(1) * y + g(2) * z;
    }

    /**
     * Computes dot product in 4D.
     *
     * @param g
     *        4-vector (grid offset)
     * @param x
     * @param y
     * @param z
     * @param w
     * @return dot product
     */
    private def dot(g: Array[Int], x: Double, y: Double, z: Double, w: Double)
    :Double = {
        g(0) * x + g(1) * y + g(2) * z + g(3) * w;
    }

    /**
     * This method is a *lot* faster than using (int)Math.floor(x).
     *
     * @param x
     *        value to be floored
     * @return
     */
    private def fastfloor(x: Double) :Int = {
        val i = BaseMath.int(x)
        if(x > 0 || x == i) i else i - 1
    }

    /**
     * Computes 2D Simplex Noise.
     *
     * @param x
     *        coordinate
     * @param y
     *        coordinate
     * @return noise value in range -1 ... +1.
     */
    def noise(x: Double, y: Double) :Double = {
        // Noise contributions from the three
        var n0 = 0d; var n1 = 0d; var n2 = 0d
        // corners
        // Skew the input space to determine which simplex cell we're in
        val s = (x + y) * F2 // Hairy factor for 2D
        val i = fastfloor(x + s)
        val j = fastfloor(y + s)
        val t = (i + j) * G2
        val x0 = x - (i - t) // The x,y distances from the cell origin
        val y0 = y - (j - t)
        // For the 2D case, the simplex shape is an equilateral triangle.
        // Determine which simplex we are in.
        // Offsets for second (middle) corner of simplex in (i,j)
        var i1 = 0; var j1 = 0
        if (x0 > y0) {
            i1 = 1
            j1 = 0
        } // lower triangle, XY order: (0,0)->(1,0)->(1,1)
        else {
            i1 = 0
            j1 = 1
        } // upper triangle, YX order: (0,0)->(0,1)->(1,1)
        // A step of (1,0) in (i,j) means a step of (1-c,-c) in (x,y), and
        // a step of (0,1) in (i,j) means a step of (-c,1-c) in (x,y), where
        // c = (3-sqrt(3))/6
        val x1 = x0 - i1 + G2 // Offsets for middle corner in (x,y) unskewed
        val y1 = y0 - j1 + G2
        val x2 = x0 + G22 // Offsets for last corner in (x,y) unskewed
        val y2 = y0 + G22
        // Work out the hashed gradient indices of the three simplex corners
        val ii = i & 0xff
        val jj = j & 0xff
        // Calculate the contribution from the three corners
        var t0 = 0.5 - x0 * x0 - y0 * y0
        if (t0 > 0) {
            t0 *= t0
            val gi0 = perm(ii + perm(jj)) % 12
            n0 = t0 * t0 * dot(grad3(gi0), x0, y0) // (x,y) of grad3 used for
            // 2D gradient
        }
        var t1 = 0.5 - x1 * x1 - y1 * y1
        if (t1 > 0) {
            t1 *= t1
            val gi1 = perm(ii + i1 + perm(jj + j1)) % 12
            n1 = t1 * t1 * dot(grad3(gi1), x1, y1)
        }
        var t2 = 0.5 - x2 * x2 - y2 * y2
        if (t2 > 0) {
            t2 *= t2
            val gi2 = perm(ii + 1 + perm(jj + 1)) % 12
            n2 = t2 * t2 * dot(grad3(gi2), x2, y2)
        }
        // Add contributions from each corner to get the final noise value.
        // The result is scaled to return values in the interval [-1,1].
        70.0 * (n0 + n1 + n2)
    }


    /**
     * Computes 3D Simplex Noise.
     *
     * @param x
     *        coordinate
     * @param y
     *        coordinate
     * @param z
     *        coordinate
     * @return noise value in range -1 ... +1
     */
    def noise(x: Double, y: Double, z: Double) :Double = {
        var n0 = 0d; var n1 = 0d; var n2 = 0d; var n3 = 0d
        // Noise contributions from the
        // four corners
        // Skew the input space to determine which simplex cell we're in
        // final double F3 = 1.0 / 3.0;
        val s = (x + y + z) * F3 // Very nice and simple skew factor
        // for 3D
        val i = fastfloor(x + s)
        val j = fastfloor(y + s)
        val k = fastfloor(z + s)
        // final double G3 = 1.0 / 6.0; // Very nice and simple unskew factor,
        // too
        val t = (i + j + k) * G3
        val x0 = x - (i - t) // The x,y,z distances from the cell origin
        val y0 = y - (j - t)
        val z0 = z - (k - t)
        // For the 3D case, the simplex shape is a slightly irregular
        // tetrahedron.
        // Determine which simplex we are in.
        // Offsets for second corner of simplex in (i,j,k) coords
        var i1 = 0; var j1 = 0; var k1 = 0
        // Offsets for third corner of simplex in (i,j,k) coords
        var i2 = 0; var j2 = 0; var k2 = 0
        if (x0 >= y0) {
            if (y0 >= z0) {
                i1 = 1
                j1 = 0
                k1 = 0
                i2 = 1
                j2 = 1
                k2 = 0
            } // X Y Z order
            else if (x0 >= z0) {
                i1 = 1
                j1 = 0
                k1 = 0
                i2 = 1
                j2 = 0
                k2 = 1
            } // X Z Y order
            else {
                i1 = 0
                j1 = 0
                k1 = 1
                i2 = 1
                j2 = 0
                k2 = 1
            } // Z X Y order
        } else { // x0<y0
            if (y0 < z0) {
                i1 = 0
                j1 = 0
                k1 = 1
                i2 = 0
                j2 = 1
                k2 = 1
            } // Z Y X order
            else if (x0 < z0) {
                i1 = 0
                j1 = 1
                k1 = 0
                i2 = 0
                j2 = 1
                k2 = 1
            } // Y Z X order
            else {
                i1 = 0
                j1 = 1
                k1 = 0
                i2 = 1
                j2 = 1
                k2 = 0
            } // Y X Z order
        }
        // A step of (1,0,0) in (i,j,k) means a step of (1-c,-c,-c) in (x,y,z),
        // a step of (0,1,0) in (i,j,k) means a step of (-c,1-c,-c) in (x,y,z),
        // and
        // a step of (0,0,1) in (i,j,k) means a step of (-c,-c,1-c) in (x,y,z),
        // where
        // c = 1/6.
        val x1 = x0 - i1 + G3 // Offsets for second corner in (x,y,z) coords
        val y1 = y0 - j1 + G3
        val z1 = z0 - k1 + G3

        val x2 = x0 - i2 + F3 // Offsets for third corner in (x,y,z)
        val y2 = y0 - j2 + F3
        val z2 = z0 - k2 + F3

        val x3 = x0 - 0.5 // Offsets for last corner in (x,y,z)
        val y3 = y0 - 0.5
        val z3 = z0 - 0.5
        // Work out the hashed gradient indices of the four simplex corners
        val ii = i & 0xff
        val jj = j & 0xff
        val kk = k & 0xff

        // Calculate the contribution from the four corners
        var t0 = 0.6 - x0 * x0 - y0 * y0 - z0 * z0
        if (t0 > 0) {
            t0 *= t0
            val gi0 = perm(ii + perm(jj + perm(kk))) % 12
            n0 = t0 * t0 * dot(grad3(gi0), x0, y0, z0)
        }
        var t1 = 0.6 - x1 * x1 - y1 * y1 - z1 * z1
        if (t1 > 0) {
            t1 *= t1
            val gi1 = perm(ii + i1 + perm(jj + j1 + perm(kk + k1))) % 12
            n1 = t1 * t1 * dot(grad3(gi1), x1, y1, z1)
        }
        var t2 = 0.6 - x2 * x2 - y2 * y2 - z2 * z2
        if (t2 > 0) {
            t2 *= t2
            val gi2 = perm(ii + i2 + perm(jj + j2 + perm(kk + k2))) % 12
            n2 = t2 * t2 * dot(grad3(gi2), x2, y2, z2)
        }
        var t3 = 0.6 - x3 * x3 - y3 * y3 - z3 * z3
        if (t3 > 0) {
            t3 *= t3
            val gi3 = perm(ii + 1 + perm(jj + 1 + perm(kk + 1))) % 12
            n3 = t3 * t3 * dot(grad3(gi3), x3, y3, z3)
        }
        // Add contributions from each corner to get the final noise value.
        // The result is scaled to stay just inside [-1,1]
        32.0 * (n0 + n1 + n2 + n3)
    }

    /**
     * Computes 4D Simplex Noise.
     *
     * @param x
     *        coordinate
     * @param y
     *        coordinate
     * @param z
     *        coordinate
     * @param w
     *        coordinate
     * @return noise value in range -1 ... +1
     */
    def noise(x: Double, y: Double, z: Double, w: Double) :Double = {
        // The skewing and unskewing factors are hairy again for the 4D case
        // Noise contributions
        var n0 = 0d; var n1 = 0d; var n2 = 0d; var n3 = 0d; var n4 = 0d
        // from the five corners
        // Skew the (x,y,z,w) space to determine which cell of 24 simplices
        val s = (x + y + z + w) * F4 // Factor for 4D skewing
        val i = fastfloor(x + s)
        val j = fastfloor(y + s)
        val k = fastfloor(z + s)
        val l = fastfloor(w + s)
        val t = (i + j + k + l) * G4 // Factor for 4D unskewing
        val x0 = x - (i - t) // The x,y,z,w distances from the cell origin
        val y0 = y - (j - t)
        val z0 = z - (k - t)
        val w0 = w - (l - t)
        // For the 4D case, the simplex is a 4D shape I won't even try to
        // describe.
        // To find out which of the 24 possible simplices we're in, we need to
        // determine the magnitude ordering of x0, y0, z0 and w0.
        // The method below is a good way of finding the ordering of x,y,z,w and
        // then find the correct traversal order for the simplex were in.
        // First, six pair-wise comparisons are performed between each possible
        // pair of the four coordinates, and the results are used to add up
        // binary bits for an integer index.
        var c = 0
        if (x0 > y0) {
            c = 0x20
        }
        if (x0 > z0) {
            c |= 0x10
        }
        if (y0 > z0) {
            c |= 0x08
        }
        if (x0 > w0) {
            c |= 0x04
        }
        if (y0 > w0) {
            c |= 0x02
        }
        if (z0 > w0) {
            c |= 0x01
        }
        // The integer offsets for the second simplex corner
        var i1 = 0; var j1 = 0; var k1 = 0; var l1 = 0
        // The integer offsets for the third simplex corner
        var i2 = 0; var j2 = 0; var k2 = 0; var l2 = 0
        // The integer offsets for the fourth simplex corner
        var i3 = 0; var j3 = 0; var k3 = 0; var l3 = 0
        // simplex[c] is a 4-vector with the numbers 0, 1, 2 and 3 in some
        // order. Many values of c will never occur, since e.g. x>y>z>w makes
        // x<z, y<w and x<w impossible. Only the 24 indices which have non-zero
        // entries make any sense. We use a thresholding to set the coordinates
        // in turn from the largest magnitude. The number 3 in the "simplex"
        // array is at the position of the largest coordinate.
        val sc: Array[Int] = simplex(c)
        i1 = if (sc(0) >= 3) 1 else 0
        j1 = if (sc(1) >= 3) 1 else 0
        k1 = if (sc(2) >= 3) 1 else 0
        l1 = if (sc(3) >= 3) 1 else 0
        // The number 2 in the "simplex" array is at the second largest
        // coordinate.
        i2 = if (sc(0) >= 2) 1 else 0
        j2 = if (sc(1) >= 2) 1 else 0
        k2 = if (sc(2) >= 2) 1 else 0
        l2 = if (sc(3) >= 2) 1 else 0
        // The number 1 in the "simplex" array is at the second smallest
        // coordinate.
        i3 = if (sc(0) >= 1) 1 else 0
        j3 = if (sc(1) >= 1) 1 else 0
        k3 = if (sc(2) >= 1) 1 else 0
        l3 = if (sc(3) >= 1) 1 else 0
        // The fifth corner has all coordinate offsets = 1, so no need to look
        // that up.
        val x1 = x0 - i1 + G4 // Offsets for second corner in (x,y,z,w)
        val y1 = y0 - j1 + G4
        val z1 = z0 - k1 + G4
        val w1 = w0 - l1 + G4


        val x2 = x0 - i2 + G42 // Offsets for third corner in (x,y,z,w)
        val y2 = y0 - j2 + G42
        val z2 = z0 - k2 + G42
        val w2 = w0 - l2 + G42

        val x3 = x0 - i3 + G43 // Offsets for fourth corner in (x,y,z,w)
        val y3 = y0 - j3 + G43
        val z3 = z0 - k3 + G43
        val w3 = w0 - l3 + G43

        val x4 = x0 + G44 // Offsets for last corner in (x,y,z,w)
        val y4 = y0 + G44
        val z4 = z0 + G44
        val w4 = w0 + G44

        // Work out the hashed gradient indices of the five simplex corners
        val ii = i & 0xff
        val jj = j & 0xff
        val kk = k & 0xff
        val ll = l & 0xff

        // Calculate the contribution from the five corners
        var t0 = 0.6 - x0 * x0 - y0 * y0 - z0 * z0 - w0 * w0
        if (t0 > 0) {
            t0 *= t0
            val gi0 = perm(ii + perm(jj + perm(kk + perm(ll)))) % 32
            n0 = t0 * t0 * dot(grad4(gi0), x0, y0, z0, w0)
        }
        var t1 = 0.6 - x1 * x1 - y1 * y1 - z1 * z1 - w1 * w1
        if (t1 > 0) {
            t1 *= t1
            val gi1 = perm(ii + i1
                    + perm(jj + j1 + perm(kk + k1 + perm(ll + l1)))) % 32
            n1 = t1 * t1 * dot(grad4(gi1), x1, y1, z1, w1)
        }
        var t2 = 0.6 - x2 * x2 - y2 * y2 - z2 * z2 - w2 * w2
        if (t2 > 0) {
            t2 *= t2
            val gi2 = perm(ii + i2
                    + perm(jj + j2 + perm(kk + k2 + perm(ll + l2)))) % 32
            n2 = t2 * t2 * dot(grad4(gi2), x2, y2, z2, w2)
        }
        var t3 = 0.6 - x3 * x3 - y3 * y3 - z3 * z3 - w3 * w3
        if (t3 > 0) {
            t3 *= t3
            val gi3 = perm(ii + i3
                    + perm(jj + j3 + perm(kk + k3 + perm(ll + l3)))) % 32
            n3 = t3 * t3 * dot(grad4(gi3), x3, y3, z3, w3)
        }
        var t4 = 0.6 - x4 * x4 - y4 * y4 - z4 * z4 - w4 * w4
        if (t4 > 0) {
            t4 *= t4
            val gi4 = perm(ii + 1 + perm(jj + 1 + perm(
                        kk + 1 + perm(ll + 1)))) % 32
            n4 = t4 * t4 * dot(grad4(gi4), x4, y4, z4, w4)
        }
        // Sum up and scale the result to cover the range [-1,1]
        27.0 * (n0 + n1 + n2 + n3 + n4)
    }
}
