/*
 * Simplex3dMath - Test Package
 * Copyright (C) 2012, Aleksey Nikiforov
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

package simplex3d.bench.math


/**
 * @author Aleksey Nikiforov (lex)
 */
object HashCodeBench {
  
  def bench(name: String)(block: => String){
    System.gc()
    val start = System.currentTimeMillis
    
    println(block)
    
    System.gc()
    val total = System.currentTimeMillis - start
    
    println(name + ": " + total/1000.0)
  }

  def test() {
    val loops = 10000
    val size = 10000
    
    val dataA = new Array[VecA](size)
    val dataB = new Array[VecB](size)
    
    for (i <- 0 until size) {
      dataA(i) = new VecA(i, i + 1, i + 2)
      dataB(i) = new VecB(i, i + 1, i + 2)
    }
    
    bench("Custom hashCode()") {
      var sum = 0
      
      var l = 0; while (l < loops) {
        var i = 0; while (i < size) {
          
          sum += dataB(i).hashCode
          
          i += 1
        }
        l += 1
      }
      
      sum.toString
    }
    
    bench("Scala hashCode()") {
      var sum = 0
      
      var l = 0; while (l < loops) {
        var i = 0; while (i < size) {
          
          sum += dataA(i).hashCode
          
          i += 1
        }
        l += 1
      }
      
      sum.toString
    }
  }
  
  def main(args: Array[String]) {
    for (i <- 0 until 5) {
      println("\nRun " + (i + 1) + ":")
      test()
    }
  }
}

final class VecA(val x: Double, val y: Double, val z: Double) {
  override def hashCode :Int = {
    41 * (
      41 * (
        41 + x.hashCode
      ) + y.hashCode
    ) + z.hashCode
  }
}

object HashCode {
  def doubleHashCode(d: Double) :Int = {
    val bits = java.lang.Double.doubleToLongBits(d)
    (bits ^ (bits >>> 32)).toInt
  }
}

final class VecB(val x: Double, val y: Double, val z: Double) {
  override def hashCode :Int = {
    import HashCode._
    
    41 * (
      41 * (
        41 + doubleHashCode(x)
      ) + doubleHashCode(y)
    ) + doubleHashCode(z)
  }
}
