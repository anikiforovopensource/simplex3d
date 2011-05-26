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


/** Noise sum.
 *
 * @author Aleksey Nikiforov (lex)
 */
final class Turbulence(
  val frequency: Double,
  val octaves: Int,
  lacunarity: Double = 2.0,
  persistence: Double = 0.5,
  roundness: Double = 0.0,
  val noiseSrc: NoiseSource = functions
) {

  import noiseSrc._


  private[this] val frequencyFactors = {
    val array = new Array[Double](octaves)

    var i = 0; while (i < octaves) {
      array(i) = pow(lacunarity, i)*frequency
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

      sum += abs(nx.noise(x*f + (i << 4)) + roundness*a)*a

      i += 1
    }

    sum
  }
  def noise1(u: inVec2d) :Double = {
    var sum = 0.0; var i = 0; while (i < octaves) {
      val f = frequencyFactors(i)
      val a = amplitudeFactors(i)

      sum += abs(nx.noise(u.x*f + (i << 4), u.y*f) + roundness*a)*a

      i += 1
    }

    sum
  }
  def noise1(u: inVec3d) :Double = {
    var sum = 0.0; var i = 0; while (i < octaves) {
      val f = frequencyFactors(i)
      val a = amplitudeFactors(i)

      sum += abs(nx.noise(u.x*f + (i << 4), u.y*f, u.z*f) + roundness*a)*a

      i += 1
    }

    sum
  }
  def noise1(u: inVec4d) :Double = {
    var sum = 0.0; var i = 0; while (i < octaves) {
      val f = frequencyFactors(i)
      val a = amplitudeFactors(i)

      sum += abs(nx.noise(u.x*f + (i << 4), u.y*f, u.z*f, u.w*f) + roundness*a)*a

      i += 1
    }

    sum
  }

  //
  def noise2(x: Double) :Vec2d = {
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
  def noise2(u: inVec2d) :Vec2d = {
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
  def noise2(u: inVec3d) :Vec2d = {
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
  def noise2(u: inVec4d) :Vec2d = {
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
  def noise3(x: Double) :Vec3d = {
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
  def noise3(u: inVec2d) :Vec3d = {
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
  def noise3(u: inVec3d) :Vec3d = {
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
  def noise3(u: inVec4d) :Vec3d = {
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
  def noise4(x: Double) :Vec4d = {
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
  def noise4(u: inVec2d) :Vec4d = {
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
  def noise4(u: inVec3d) :Vec4d = {
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
  def noise4(u: inVec4d) :Vec4d = {
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
