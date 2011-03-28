/*
 * Simplex3d, MathTest package
 * Copyright (C) 2011, Aleksey Nikiforov
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

package test.math

import org.scalatest._
import simplex3d.math._
import simplex3d.math.floatx._
import simplex3d.math.doublex._
import simplex3d.math.doublex.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
class PrimitiveRefTest extends FunSuite {

  test("Mutable BooleanRef") {
    val r = new BooleanRef(false)
    
    r &= false; assert(r.toConst == false)
    r &= true; assert(r.toConst == false)
    
    r |= false; assert(r.toConst == false)
    r |= true; assert(r.toConst == true); r := false
    
    r ^= false; assert(r.toConst == false)
    r ^= true; assert(r.toConst == true)
    
    r &= false; assert(r.toConst == false); r := true
    r &= true; assert(r.toConst == true)
    
    r |= false; assert(r.toConst == true)
    r |= true; assert(r.toConst == true)
    
    r ^= false; assert(r.toConst == true)
    r ^= true; assert(r.toConst == false)
  }
  
  test("Mutable IntRef") {
    val r = new IntRef(4)
    
    r *= 2; assert(r.toConst == 8); r := 4
    r /= 2; assert(r.toConst == 2); r := 4
    r += 3; assert(r.toConst == 7); r := 4
    r -= 3; assert(r.toConst == 1); r := 4

    r %= 3; assert(r.toConst == 1); r := -4
    r >>= 1; assert(r.toConst == -2); r := -4
    r >>>= 1; assert(r.toConst == 0x7FFFFFFE); r := 4
    r <<= 1; assert(r.toConst == 8); r := 4
    r &= 6; assert(r.toConst == 4); r := 4
    r |= 2; assert(r.toConst == 6); r := 4
    r ^= 6; assert(r.toConst == 2)
  }
  
  test("Mutable FloatRef") {
    val r = new FloatRef(4)
    
    r *= 2; assert(r.toConst == 8); r := 4
    r /= 2; assert(r.toConst == 2); r := 4
    r += 3; assert(r.toConst == 7); r := 4
    r -= 3; assert(r.toConst == 1); r := 4
  }
  
  test("Mutable DoubleRef") {
    val r = new DoubleRef(4)
    
    r *= 2; assert(r.toConst == 8); r := 4
    r /= 2; assert(r.toConst == 2); r := 4
    r += 3; assert(r.toConst == 7); r := 4
    r -= 3; assert(r.toConst == 1); r := 4
  }
}
