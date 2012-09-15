/*
 * Simplex3dAlgorithm - Test Package
 * Copyright (C) 2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dAlgorithmTest.
 *
 * Simplex3dAlgorithmTest is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dAlgorithmTest is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */


package bench.noise


object GradBench {

  def main() {
    test()
    test()
    test()
    test()
  }

  val loops = 200*1000*1000 + 1

  def test() {
    println("\nTesting...")
    var start = 0L

    start = System.currentTimeMillis
    test2logic(loops)
    val test2logicTime = System.currentTimeMillis - start
    
    start = System.currentTimeMillis
    test2array(loops)
    val test2arrayTime = System.currentTimeMillis - start
    
    start = System.currentTimeMillis
    test3logic(loops)
    val test3logicTime = System.currentTimeMillis - start
    
    start = System.currentTimeMillis
    test3array(loops)
    val test3arrayTime = System.currentTimeMillis - start
    
    start = System.currentTimeMillis
    test4logic(loops)
    val test4logicTime = System.currentTimeMillis - start
    
    start = System.currentTimeMillis
    test4array(loops)
    val test4arrayTime = System.currentTimeMillis - start


    println("\nResults:")
    println("2 logic time: " + test2logicTime + ".")
    println("2 array time: " + test2arrayTime + ".")
    println("3 logic time: " + test3logicTime + ".")
    println("3 array time: " + test3arrayTime + ".")
    println("4 logic time: " + test4logicTime + ".")
    println("4 array time: " + test4arrayTime + ".")
  }

  def test2logic(loops: Int) {
    var a = 0
    
    var i = 0; while (i < loops) {
      a += grad2dot(i)(1, 2).toInt
      i += 1
    }
    
    println(a)
  }
  
  def test2array(loops: Int) {
    var a = 0
    
    var i = 0; while (i < loops) {
      a += grad2array(i)(1, 2).toInt
      i += 1
    }
    
    println(a)
  }
  
  def test3logic(loops: Int) {
    var a = 0
    
    var i = 0; while (i < loops) {
      a += grad3dot(i)(1, 2, 3).toInt
      i += 1
    }
    
    println(a)
  }
  
  def test3array(loops: Int) {
    var a = 0
    
    var i = 0; while (i < loops) {
      a += grad3array(i)(1, 2, 3).toInt
      i += 1
    }
    
    println(a)
  }
  
  def test4logic(loops: Int) {
    var a = 0
    
    var i = 0; while (i < loops) {
      a += grad4dot(i)(1, 2, 3, 4).toInt
      i += 1
    }
    
    println(a)
  }
  
  def test4array(loops: Int) {
    var a = 0
    
    var i = 0; while (i < loops) {
      a += grad4array(i)(1, 2, 3, 4).toInt
      i += 1
    }
    
    println(a)
  }
  
  def grad2dot(i: Int)(x: Double, y: Double) :Double = {
    import scala.annotation._
    
    ((i & 0x07): @switch) match {
      case 0 => x
      case 1 => y
      case 2 => -x
      case 3 => -y
      case 4 => x + y
      case 5 => x - y
      case 6 => -x + y
      case 7 => -x - y
    }
  }
  
  def grad3dot(i: Int)(x: Double, y: Double, z: Double) :Double = {
    import scala.annotation._
    
    val n = i % 12
    var a = 0.0; var b = 0.0
    
    if (n < 8) { a = y; if (n < 4) b = x else b = z }
    else { a = x; b = z }
    
    ((n & 0x03): @switch) match {
      case 0 => a + b
      case 1 => a - b
      case 2 => -a + b
      case 3 => -a - b
    }
  }
  
  def grad4dot(i: Int)(x: Double, y: Double, z: Double, w: Double) :Double = {
    import scala.annotation._
    
    val n = i & 0x1F
    var a = 0.0; var b = 0.0; var c = 0.0
    
    if (n < 16) { if (n < 8) { a = y; b = z; c = w } else { a = x; b = z; c = w } }
    else { if (n < 24) { a = x; b = y; c = w } else { a = x; b = y; c = z } }
    
    ((n & 0x07): @switch) match {
      case 0 => a + b + c
      case 1 => a + b - c
      case 2 => a - b + c
      case 3 => a - b - c
      case 4 => -a + b + c
      case 5 => -a + b - c
      case 6 => -a - b + c
      case 7 => -a - b - c
    }
  }
  
  def grad2array(i: Int)(x: Double, y: Double) :Double = {
    val v = array3(i & 0x0F)
    v(0)*x + v(1)*y
  }
  
  def grad3array(i: Int)(x: Double, y: Double, z: Double) :Double = {
    val v = array3(i % 12)
    v(0)*x + v(1)*y + v(2)*z
  }
  
  def grad4array(i: Int)(x: Double, y: Double, z: Double, w: Double) :Double = {
    val v = array4(i & 0x1F)
    v(0)*x + v(1)*y + v(2)*z + v(3)*w
  }
  
  private final val array3: Array[Array[Byte]] = Array(
    Array(0,1,1), Array(0,1,-1), Array(0,-1,1), Array(0,-1,-1),
    Array(1,0,1), Array(1,0,-1), Array(-1,0,1), Array(-1,0,-1),
    Array(1,1,0), Array(1,-1,0), Array(-1,1,0), Array(-1,-1,0),
    Array(1,0,-1), Array(-1,0,-1), Array(0,-1,1), Array(0,1,1)
  )

  private final val array4: Array[Array[Byte]] = Array(
    Array(0,1,1,1), Array(0,1,1,-1), Array(0,1,-1,1), Array(0,1,-1,-1),
    Array(0,-1,1,1), Array(0,-1,1,-1), Array(0,-1,-1,1), Array(0,-1,-1,-1),
    Array(1,0,1,1), Array(1,0,1,-1), Array(1,0,-1,1), Array(1,0,-1,-1),
    Array(-1,0,1,1), Array(-1,0,1,-1), Array(-1,0,-1,1), Array(-1,0,-1,-1),
    Array(1,1,0,1), Array(1,1,0,-1), Array(1,-1,0,1), Array(1,-1,0,-1),
    Array(-1,1,0,1), Array(-1,1,0,-1), Array(-1,-1,0,1), Array(-1,-1,0,-1),
    Array(1,1,1,0), Array(1,1,-1,0), Array(1,-1,1,0), Array(1,-1,-1,0),
    Array(-1,1,1,0), Array(-1,1,-1,0), Array(-1,-1,1,0), Array(-1,-1,-1,0)
  )
}
