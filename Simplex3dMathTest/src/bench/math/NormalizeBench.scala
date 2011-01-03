/*
 * Simplex3d, MathTest package
 * Copyright (C) 2010-2011, Simplex3d Team
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

import simplex3d.math._
import simplex3d.math.doublex.functions._
import simplex3d.math.double._


/**
 * @author Aleksey Nikiforov (lex)
 */
object NormalizeBench {
  def main(args: Array[String]) {
    new NormalizeTC().run()
  }
}

class NormalizeTC {
  val length = 10000
  val loops = 10000

  def run() {
    var start = 0L

    start = System.currentTimeMillis
    testOptimisedBest(length, loops)
    val optimisedBestTime = System.currentTimeMillis - start
    
    start = System.currentTimeMillis
    testNormalize(length, loops)
    val normalizeTime = System.currentTimeMillis - start

    println("Normalize time: " + normalizeTime + ".")
    println("Opt best time: " + optimisedBestTime + ".")
  }

  final def normalizeTo(u: Vec3, absDelta: Double) = {
    val len2 = dot(u, u)
    if (approxEqual(len2, 1, absDelta)) u
    else u/(sqrt(len2))
  }

  final def testNormalize(length: Int, loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {

        // Bench code
        val u = normalize(Vec3(i, i + 1, i + 2))
        val t = normalize(u)
        answer += Int(t.x + t.y + t.z)

        i += 1
      }
      l += 1
    }

    println(answer)
  }

  final def testOptimisedBest(length: Int, loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {

        // Bench code
        val u = normalize(Vec3(i, i + 1, i + 2))
        val t = normalizeTo(u, 1e-14)
        answer += Int(t.x + t.y + t.z)

        i += 1
      }
      l += 1
    }

    println(answer)
  }
}
