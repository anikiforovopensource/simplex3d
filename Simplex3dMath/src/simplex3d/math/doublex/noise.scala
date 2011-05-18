/*
 * Simplex3d, DoubleMath module
 * Copyright (C) 2011, Aleksey Nikiforov
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
 */

package simplex3d.math.doublex

import simplex3d.math._
import simplex3d.math.doublex.functions.{abs, pow}


/** Various functions to work with noise.
 *
 * @author Aleksey Nikiforov (lex)
 */
object noise {

  // 1/wavelength = 1/sqrt(2)
  final val BaseFrequency = 0.7071067811865475244

  // Simplex side length * 2
  final val BaseWavelength = 1.4142135623730950488


  /** This class uses 46-bit seed given by the formula: usedSeed = (seed & ((1L << 46) - 1)).
   *
   */
  class SimplexNoise(val seed: Long) {

    private[noise] val nx = new SimplexNoiseImpl(seed & ((1L << 46) - 1))
    private[noise] val ny = new SimplexNoiseImpl(seed & ((1L << 46) - 1) | (1L << 46))
    private[noise] val nz = new SimplexNoiseImpl(seed & ((1L << 46) - 1) | (2L << 46))
    private[noise] val nw = new SimplexNoiseImpl(seed & ((1L << 46) - 1) | (3L << 46))

    /** Computes the value of the 1-dimensional simplex noise function.
     * The simplex noise function is C^2^ continuous (the first and the second derivatives are continuous).
     *
     * The return values of the simplex noise function have the following properties:
     *   - They are in the range [-1, 1].
     *   - They have Gaussian dirstibution.
     *   - The overall average is zero.
     *   - A particular argument will always result in the same return value.
     *   - Statistical properties do not change with rotation and translation of the domain.
     *
     * '''noise1''' function returns zero when the arguments are coordinates of simplex vertices.
     * For 1-dimensional case this means `noise1(N*simplexSide) == 0` for all integral N.
     * Simplex side is the same for all dimensions and is equal to `1/sqrt(2) = 0.7071067811865475244`.
     *
     * @param x a double argument.
     * @return the value of the simplex noise function.
     */
    def noise1(x: Double) :Double = nx.noise(x)
    def noise1(u: inVec2d) :Double = nx.noise(u.x, u.y)
    def noise1(u: inVec3d) :Double = nx.noise(u.x, u.y, u.z)
    def noise1(u: inVec4d) :Double = nx.noise(u.x, u.y, u.z, u.w)

    /** Computes __two__ independent values of the 1-dimensional simplex noise function.
     * @param x a double argument.
     * @return two values of the simplex noise function packed as Vec2.
     * @see [[simplex3d.math.noise.SimplexNoise.noise1(Double)]]
     */
    def noise2(x: Double) :Vec2d = new Vec2d(
      nx.noise(x),
      ny.noise(x)
    )
    def noise2(u: inVec2d) :Vec2d = new Vec2d(
      nx.noise(u.x, u.y),
      ny.noise(u.x, u.y)
    )
    def noise2(u: inVec3d) :Vec2d = new Vec2d(
      nx.noise(u.x, u.y, u.z),
      ny.noise(u.x, u.y, u.z)
    )
    def noise2(u: inVec4d) :Vec2d = new Vec2d(
      nx.noise(u.x, u.y, u.z, u.w),
      ny.noise(u.x, u.y, u.z, u.w)
    )

    def noise3(x: Double) :Vec3d = new Vec3d(
      nx.noise(x),
      ny.noise(x),
      nz.noise(x)
    )
    def noise3(u: inVec2d) :Vec3d = new Vec3d(
      nx.noise(u.x, u.y),
      ny.noise(u.x, u.y),
      nz.noise(u.x, u.y)
    )
    def noise3(u: inVec3d) :Vec3d = new Vec3d(
      nx.noise(u.x, u.y, u.z),
      ny.noise(u.x, u.y, u.z),
      nz.noise(u.x, u.y, u.z)
    )
    def noise3(u: inVec4d) :Vec3d = new Vec3d(
      nx.noise(u.x, u.y, u.z, u.w),
      ny.noise(u.x, u.y, u.z, u.w),
      nz.noise(u.x, u.y, u.z, u.w)
    )

    def noise4(x: Double) :Vec4d = new Vec4d(
      nx.noise(x),
      ny.noise(x),
      nz.noise(x),
      nw.noise(x)
    )
    def noise4(u: inVec2d) :Vec4d = new Vec4d(
      nx.noise(u.x, u.y),
      ny.noise(u.x, u.y),
      nz.noise(u.x, u.y),
      nw.noise(u.x, u.y)
    )
    def noise4(u: inVec3d) :Vec4d = new Vec4d(
      nx.noise(u.x, u.y, u.z),
      ny.noise(u.x, u.y, u.z),
      nz.noise(u.x, u.y, u.z),
      nw.noise(u.x, u.y, u.z)
    )
    def noise4(u: inVec4d) :Vec4d = new Vec4d(
      nx.noise(u.x, u.y, u.z, u.w),
      ny.noise(u.x, u.y, u.z, u.w),
      nz.noise(u.x, u.y, u.z, u.w),
      nw.noise(u.x, u.y, u.z, u.w)
    )
  }


  class NoiseSum(
    val frequency: Double,
    val octaves: Int,
    lacunarity: Double = 2.0,
    persistence: Double = 0.5,
    val noiseSrc: SimplexNoise = functions
  ) {
    
    import noiseSrc._


    private[this] val frequencyFactors = {
      val array = new Array[Double](octaves)

      var i = 0; while (i < octaves) {
        array(i) = pow(lacunarity, i)*frequency*BaseWavelength
        i += 1
      }

      array
    }
    private[this] val amplitudeFactors = {
      val array = new Array[Double](octaves)

      var i = 0; while (i < octaves) {
        array(i) = pow(persistence, i)
        i += 1
      }

      array
    }

    //
    def noise1(x: Double) :Double = {
      var sum = 0.0; var i = 0; while (i < octaves) {
        val f = frequencyFactors(i)
        val a = amplitudeFactors(i)

        sum += nx.noise(x*f + (i << 4))*a

        i += 1
      }

      sum
    }
    def noise1(u: inVec2d) :Double = {
      var sum = 0.0; var i = 0; while (i < octaves) {
        val f = frequencyFactors(i)
        val a = amplitudeFactors(i)

        sum += nx.noise(u.x*f + (i << 4), u.y*f)*a

        i += 1
      }

      sum
    }
    def noise1(u: inVec3d) :Double = {
      var sum = 0.0; var i = 0; while (i < octaves) {
        val f = frequencyFactors(i)
        val a = amplitudeFactors(i)

        sum += nx.noise(u.x*f + (i << 4), u.y*f, u.z*f)*a

        i += 1
      }

      sum
    }
    def noise1(u: inVec4d) :Double = {
      var sum = 0.0; var i = 0; while (i < octaves) {
        val f = frequencyFactors(i)
        val a = amplitudeFactors(i)

        sum += nx.noise(u.x*f + (i << 4), u.y*f, u.z*f, u.w*f)*a

        i += 1
      }

      sum
    }

    //
    def noise2(x: Double) :Vec2d = {
      var sum = Vec2d(0); var i = 0; while (i < octaves) {
        val f = frequencyFactors(i)
        val a = amplitudeFactors(i)

        val px = x*f + (i << 4)

        sum.x += nx.noise(px)*a
        sum.y += ny.noise(px)*a

        i += 1
      }

      sum
    }
    def noise2(u: inVec2d) :Vec2d = {
      var sum = Vec2d(0); var i = 0; while (i < octaves) {
        val f = frequencyFactors(i)
        val a = amplitudeFactors(i)

        val px = u.x*f + (i << 4)
        val py = u.y*f

        sum.x += nx.noise(px, py)*a
        sum.y += ny.noise(px, py)*a

        i += 1
      }

      sum
    }
    def noise2(u: inVec3d) :Vec2d = {
      var sum = Vec2d(0); var i = 0; while (i < octaves) {
        val f = frequencyFactors(i)
        val a = amplitudeFactors(i)

        val px = u.x*f + (i << 4)
        val py = u.y*f
        val pz = u.z*f

        sum.x += nx.noise(px, py, pz)*a
        sum.y += ny.noise(px, py, pz)*a

        i += 1
      }

      sum
    }
    def noise2(u: inVec4d) :Vec2d = {
      var sum = Vec2d(0); var i = 0; while (i < octaves) {
        val f = frequencyFactors(i)
        val a = amplitudeFactors(i)

        val px = u.x*f + (i << 4)
        val py = u.y*f
        val pz = u.z*f
        val pw = u.w*f

        sum.x += nx.noise(px, py, pz, pw)*a
        sum.y += ny.noise(px, py, pz, pw)*a

        i += 1
      }

      sum
    }

    //
    def noise3(x: Double) :Vec3d = {
      var sum = Vec3d(0); var i = 0; while (i < octaves) {
        val f = frequencyFactors(i)
        val a = amplitudeFactors(i)

        val px = x*f + (i << 4)

        sum.x += nx.noise(px)*a
        sum.y += ny.noise(px)*a
        sum.z += nz.noise(px)*a

        i += 1
      }

      sum
    }
    def noise3(u: inVec2d) :Vec3d = {
      var sum = Vec3d(0); var i = 0; while (i < octaves) {
        val f = frequencyFactors(i)
        val a = amplitudeFactors(i)

        val px = u.x*f + (i << 4)
        val py = u.y*f

        sum.x += nx.noise(px, py)*a
        sum.y += ny.noise(px, py)*a
        sum.z += nz.noise(px, py)*a

        i += 1
      }

      sum
    }
    def noise3(u: inVec3d) :Vec3d = {
      var sum = Vec3d(0); var i = 0; while (i < octaves) {
        val f = frequencyFactors(i)
        val a = amplitudeFactors(i)

        val px = u.x*f + (i << 4)
        val py = u.y*f
        val pz = u.z*f

        sum.x += nx.noise(px, py, pz)*a
        sum.y += ny.noise(px, py, pz)*a
        sum.z += nz.noise(px, py, pz)*a

        i += 1
      }

      sum
    }
    def noise3(u: inVec4d) :Vec3d = {
      var sum = Vec3d(0); var i = 0; while (i < octaves) {
        val f = frequencyFactors(i)
        val a = amplitudeFactors(i)

        val px = u.x*f + (i << 4)
        val py = u.y*f
        val pz = u.z*f
        val pw = u.w*f

        sum.x += nx.noise(px, py, pz, pw)*a
        sum.y += ny.noise(px, py, pz, pw)*a
        sum.z += nz.noise(px, py, pz, pw)*a

        i += 1
      }

      sum
    }

    //
    def noise4(x: Double) :Vec4d = {
      var sum = Vec4d(0); var i = 0; while (i < octaves) {
        val f = frequencyFactors(i)
        val a = amplitudeFactors(i)

        val px = x*f + (i << 4)

        sum.x += nx.noise(px)*a
        sum.y += ny.noise(px)*a
        sum.z += nz.noise(px)*a
        sum.w += nw.noise(px)*a

        i += 1
      }

      sum
    }
    def noise4(u: inVec2d) :Vec4d = {
      var sum = Vec4d(0); var i = 0; while (i < octaves) {
        val f = frequencyFactors(i)
        val a = amplitudeFactors(i)

        val px = u.x*f + (i << 4)
        val py = u.y*f

        sum.x += nx.noise(px, py)*a
        sum.y += ny.noise(px, py)*a
        sum.z += nz.noise(px, py)*a
        sum.w += nw.noise(px, py)*a

        i += 1
      }

      sum
    }
    def noise4(u: inVec3d) :Vec4d = {
      var sum = Vec4d(0); var i = 0; while (i < octaves) {
        val f = frequencyFactors(i)
        val a = amplitudeFactors(i)

        val px = u.x*f + (i << 4)
        val py = u.y*f
        val pz = u.z*f

        sum.x += nx.noise(px, py, pz)*a
        sum.y += ny.noise(px, py, pz)*a
        sum.z += nz.noise(px, py, pz)*a
        sum.w += nw.noise(px, py, pz)*a

        i += 1
      }

      sum
    }
    def noise4(u: inVec4d) :Vec4d = {
      var sum = Vec4d(0); var i = 0; while (i < octaves) {
        val f = frequencyFactors(i)
        val a = amplitudeFactors(i)

        val px = u.x*f + (i << 4)
        val py = u.y*f
        val pz = u.z*f
        val pw = u.w*f

        sum.x += nx.noise(px, py, pz, pw)*a
        sum.y += ny.noise(px, py, pz, pw)*a
        sum.z += nz.noise(px, py, pz, pw)*a
        sum.w += nw.noise(px, py, pz, pw)*a

        i += 1
      }

      sum
    }


    //
    def turbulence1(roundness: Double, x: Double) :Double = {
      var sum = 0.0; var i = 0; while (i < octaves) {
        val f = frequencyFactors(i)
        val a = amplitudeFactors(i)

        sum += abs(nx.noise(x*f + (i << 4)) + roundness*a)*a

        i += 1
      }

      sum
    }
    def turbulence1(roundness: Double, u: inVec2d) :Double = {
      var sum = 0.0; var i = 0; while (i < octaves) {
        val f = frequencyFactors(i)
        val a = amplitudeFactors(i)
        
        sum += abs(nx.noise(u.x*f + (i << 4), u.y*f) + roundness*a)*a

        i += 1
      }

      sum
    }
    def turbulence1(roundness: Double, u: inVec3d) :Double = {
      var sum = 0.0; var i = 0; while (i < octaves) {
        val f = frequencyFactors(i)
        val a = amplitudeFactors(i)
        
        sum += abs(nx.noise(u.x*f + (i << 4), u.y*f, u.z*f) + roundness*a)*a

        i += 1
      }

      sum
    }
    def turbulence1(roundness: Double, u: inVec4d) :Double = {
      var sum = 0.0; var i = 0; while (i < octaves) {
        val f = frequencyFactors(i)
        val a = amplitudeFactors(i)
        
        sum += abs(nx.noise(u.x*f + (i << 4), u.y*f, u.z*f, u.w*f) + roundness*a)*a

        i += 1
      }
      
      sum
    }

    //
    def turbulence2(roundness: Double, x: Double) :Vec2d = {
      var sum = Vec2d(0); var i = 0; while (i < octaves) {
        val f = frequencyFactors(i)
        val a = amplitudeFactors(i)
        val r = roundness*a

        val px = x*f + (i << 4)

        sum.x += abs(nx.noise(px) + r)*a
        sum.y += abs(ny.noise(px) + r)*a

        i += 1
      }

      sum
    }
    def turbulence2(roundness: Double, u: inVec2d) :Vec2d = {
      var sum = Vec2d(0); var i = 0; while (i < octaves) {
        val f = frequencyFactors(i)
        val a = amplitudeFactors(i)
        val r = roundness*a

        val px = u.x*f + (i << 4)
        val py = u.y*f

        sum.x += abs(nx.noise(px, py) + r)*a
        sum.y += abs(ny.noise(px, py) + r)*a

        i += 1
      }

      sum
    }
    def turbulence2(roundness: Double, u: inVec3d) :Vec2d = {
      var sum = Vec2d(0); var i = 0; while (i < octaves) {
        val f = frequencyFactors(i)
        val a = amplitudeFactors(i)
        val r = roundness*a

        val px = u.x*f + (i << 4)
        val py = u.y*f
        val pz = u.z*f

        sum.x += abs(nx.noise(px, py, pz) + r)*a
        sum.y += abs(ny.noise(px, py, pz) + r)*a

        i += 1
      }

      sum
    }
    def turbulence2(roundness: Double, u: inVec4d) :Vec2d = {
      var sum = Vec2d(0); var i = 0; while (i < octaves) {
        val f = frequencyFactors(i)
        val a = amplitudeFactors(i)
        val r = roundness*a

        val px = u.x*f + (i << 4)
        val py = u.y*f
        val pz = u.z*f
        val pw = u.w*f

        sum.x += abs(nx.noise(px, py, pz, pw) + r)*a
        sum.y += abs(ny.noise(px, py, pz, pw) + r)*a

        i += 1
      }

      sum
    }

    //
    def turbulence3(roundness: Double, x: Double) :Vec3d = {
      var sum = Vec3d(0); var i = 0; while (i < octaves) {
        val f = frequencyFactors(i)
        val a = amplitudeFactors(i)
        val r = roundness*a

        val px = x*f + (i << 4)

        sum.x += abs(nx.noise(px) + r)*a
        sum.y += abs(ny.noise(px) + r)*a
        sum.z += abs(nz.noise(px) + r)*a

        i += 1
      }

      sum
    }
    def turbulence3(roundness: Double, u: inVec2d) :Vec3d = {
      var sum = Vec3d(0); var i = 0; while (i < octaves) {
        val f = frequencyFactors(i)
        val a = amplitudeFactors(i)
        val r = roundness*a

        val px = u.x*f + (i << 4)
        val py = u.y*f

        sum.x += abs(nx.noise(px, py) + r)*a
        sum.y += abs(ny.noise(px, py) + r)*a
        sum.z += abs(nz.noise(px, py) + r)*a

        i += 1
      }

      sum
    }
    def turbulence3(roundness: Double, u: inVec3d) :Vec3d = {
      var sum = Vec3d(0); var i = 0; while (i < octaves) {
        val f = frequencyFactors(i)
        val a = amplitudeFactors(i)
        val r = roundness*a

        val px = u.x*f + (i << 4)
        val py = u.y*f
        val pz = u.z*f

        sum.x += abs(nx.noise(px, py, pz) + r)*a
        sum.y += abs(ny.noise(px, py, pz) + r)*a
        sum.z += abs(nz.noise(px, py, pz) + r)*a

        i += 1
      }

      sum
    }
    def turbulence3(roundness: Double, u: inVec4d) :Vec3d = {
      var sum = Vec3d(0); var i = 0; while (i < octaves) {
        val f = frequencyFactors(i)
        val a = amplitudeFactors(i)
        val r = roundness*a

        val px = u.x*f + (i << 4)
        val py = u.y*f
        val pz = u.z*f
        val pw = u.w*f

        sum.x += abs(nx.noise(px, py, pz, pw) + r)*a
        sum.y += abs(ny.noise(px, py, pz, pw) + r)*a
        sum.z += abs(nz.noise(px, py, pz, pw) + r)*a

        i += 1
      }

      sum
    }

    //
    def turbulence4(roundness: Double, x: Double) :Vec4d = {
      var sum = Vec4d(0); var i = 0; while (i < octaves) {
        val f = frequencyFactors(i)
        val a = amplitudeFactors(i)
        val r = roundness*a

        val px = x*f + (i << 4)

        sum.x += abs(nx.noise(px) + r)*a
        sum.y += abs(ny.noise(px) + r)*a
        sum.z += abs(nz.noise(px) + r)*a
        sum.w += abs(nw.noise(px) + r)*a

        i += 1
      }

      sum
    }
    def turbulence4(roundness: Double, u: inVec2d) :Vec4d = {
      var sum = Vec4d(0); var i = 0; while (i < octaves) {
        val f = frequencyFactors(i)
        val a = amplitudeFactors(i)
        val r = roundness*a

        val px = u.x*f + (i << 4)
        val py = u.y*f

        sum.x += abs(nx.noise(px, py) + r)*a
        sum.y += abs(ny.noise(px, py) + r)*a
        sum.z += abs(nz.noise(px, py) + r)*a
        sum.w += abs(nw.noise(px, py) + r)*a

        i += 1
      }

      sum
    }
    def turbulence4(roundness: Double, u: inVec3d) :Vec4d = {
      var sum = Vec4d(0); var i = 0; while (i < octaves) {
        val f = frequencyFactors(i)
        val a = amplitudeFactors(i)
        val r = roundness*a

        val px = u.x*f + (i << 4)
        val py = u.y*f
        val pz = u.z*f

        sum.x += abs(nx.noise(px, py, pz) + r)*a
        sum.y += abs(ny.noise(px, py, pz) + r)*a
        sum.z += abs(nz.noise(px, py, pz) + r)*a
        sum.w += abs(nw.noise(px, py, pz) + r)*a

        i += 1
      }

      sum
    }
    def turbulence4(roundness: Double, u: inVec4d) :Vec4d = {
      var sum = Vec4d(0); var i = 0; while (i < octaves) {
        val f = frequencyFactors(i)
        val a = amplitudeFactors(i)
        val r = roundness*a

        val px = u.x*f + (i << 4)
        val py = u.y*f
        val pz = u.z*f
        val pw = u.w*f

        sum.x += abs(nx.noise(px, py, pz, pw) + r)*a
        sum.y += abs(ny.noise(px, py, pz, pw) + r)*a
        sum.z += abs(nz.noise(px, py, pz, pw) + r)*a
        sum.w += abs(nw.noise(px, py, pz, pw) + r)*a

        i += 1
      }

      sum
    }
  }
  
}
