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

import java.io._
import simplex3d.math._
import simplex3d.math.doublex.functions.{abs, pow}


/** Noise sum.
 *
 * @author Aleksey Nikiforov (lex)
 */
@SerialVersionUID(8104346712419693669L)
final class Turbulence(
  val frequency: Double,
  val octaves: Int,
  val lacunarity: Double = 2.0,
  val persistence: Double = 0.5,
  val roundness: Double = 0.0,
  val source: NoiseSource = NoiseDefaults.DefaultSource
) extends Serializable {

  @transient private[this] var frequencyFactors: Array[Double] = _
  @transient private[this] var amplitudeFactors: Array[Double] = _
  initTransient()

  private[this] def initTransient() {
    frequencyFactors = new Array[Double](octaves)

    var i = 0; while (i < octaves) {
      frequencyFactors(i) = pow(lacunarity, i)*frequency
      i += 1
    }


    amplitudeFactors = new Array[Double](octaves)

    i = 0; while (i < octaves) {
      amplitudeFactors(i) = pow(persistence, i)
      i += 1
    }
  }

  
  def apply(x: Double) :Double = {
    var sum = 0.0; var i = 0; while (i < octaves) {
      val f = frequencyFactors(i)
      val a = amplitudeFactors(i)

      sum += abs(source(x*f + (i << 4)) + roundness*a)*a

      i += 1
    }

    sum
  }
  def apply(u: inVec2d) :Double = {
    var sum = 0.0; var i = 0; while (i < octaves) {
      val f = frequencyFactors(i)
      val a = amplitudeFactors(i)

      sum += abs(source(u.x*f + (i << 4), u.y*f) + roundness*a)*a

      i += 1
    }

    sum
  }
  def apply(u: inVec3d) :Double = {
    var sum = 0.0; var i = 0; while (i < octaves) {
      val f = frequencyFactors(i)
      val a = amplitudeFactors(i)

      sum += abs(source(u.x*f + (i << 4), u.y*f, u.z*f) + roundness*a)*a

      i += 1
    }

    sum
  }
  def apply(u: inVec4d) :Double = {
    var sum = 0.0; var i = 0; while (i < octaves) {
      val f = frequencyFactors(i)
      val a = amplitudeFactors(i)

      sum += abs(source(u.x*f + (i << 4), u.y*f, u.z*f, u.w*f) + roundness*a)*a

      i += 1
    }

    sum
  }


  @throws(classOf[IOException])
  private[this] def writeObject(out: ObjectOutputStream) {
    out.defaultWriteObject()
  }

  @throws(classOf[IOException]) @throws(classOf[ClassNotFoundException])
  private[this] def readObject(in: ObjectInputStream) {
    in.defaultReadObject()
    initTransient()
  }
}
