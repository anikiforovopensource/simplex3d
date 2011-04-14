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

    assert(new BooleanRef(false) == new BooleanRef(false))
    assert(new BooleanRef(true) == new BooleanRef(true))
    assert(new BooleanRef(false) != new BooleanRef(true))
    assert(new BooleanRef(true) != new BooleanRef(false))

    new BooleanRef(true) match {
      case BooleanRef(true) => // do nothing
      case BooleanRef(false) => throw new AssertionError
    }

    new BooleanRef(false) match {
      case BooleanRef(true) => throw new AssertionError
      case BooleanRef(false) => // do nothing
    }

    {
      val conv1: BooleanRef = new BooleanRef(true).asInstanceOf[ReadBooleanRef]
      assert(conv1.toConst == true)
      val conv2: BooleanRef = true
      assert(conv2.toConst == true)
    }
    {
      val conv1: BooleanRef = new BooleanRef(false).asInstanceOf[ReadBooleanRef]
      assert(conv1.toConst == false)
      val conv2: BooleanRef = false
      assert(conv2.toConst == false)
    }
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

    assert(new IntRef(1) == new IntRef(1))
    assert(new IntRef(5) == new IntRef(5))
    assert(new IntRef(5) != new IntRef(1))
    assert(new IntRef(1) != new BooleanRef(true))
    assert(new IntRef(0) != new BooleanRef(false))

    new IntRef(2) match {
      case IntRef(1) => throw new AssertionError
      case IntRef(2) => // do nothing
      case IntRef(3) => throw new AssertionError
    }

    val conv1: IntRef = new IntRef(7).asInstanceOf[ReadIntRef]
    assert(conv1.toConst == 7)
    val conv2: IntRef = 7
    assert(conv2.toConst == 7)
  }
  
  test("Mutable FloatRef") {
    val r = new FloatRef(4)
    
    r *= 2; assert(r.toConst == 8); r := 4
    r /= 2; assert(r.toConst == 2); r := 4
    r += 3; assert(r.toConst == 7); r := 4
    r -= 3; assert(r.toConst == 1); r := 4

    assert(new FloatRef(1) == new FloatRef(1))
    assert(new FloatRef(5) == new FloatRef(5))
    assert(new FloatRef(5) != new FloatRef(1))
    assert(new FloatRef(1) != new BooleanRef(true))
    assert(new FloatRef(0) != new BooleanRef(false))
    assert(new FloatRef(5) == new IntRef(5))
    assert(new FloatRef(5) != new IntRef(1))

    new FloatRef(2) match {
      case FloatRef(1) => throw new AssertionError
      case FloatRef(2) => // do nothing
      case FloatRef(3) => throw new AssertionError
    }

    val conv1: FloatRef = new FloatRef(7).asInstanceOf[ReadFloatRef]
    assert(conv1.toConst == 7)
    val conv2: FloatRef = 7
    assert(conv2.toConst == 7)
  }
  
  test("Mutable DoubleRef") {
    val r = new DoubleRef(4)
    
    r *= 2; assert(r.toConst == 8); r := 4
    r /= 2; assert(r.toConst == 2); r := 4
    r += 3; assert(r.toConst == 7); r := 4
    r -= 3; assert(r.toConst == 1); r := 4

    assert(new DoubleRef(1) == new DoubleRef(1))
    assert(new DoubleRef(5) == new DoubleRef(5))
    assert(new DoubleRef(5) != new DoubleRef(1))
    assert(new DoubleRef(1) != new BooleanRef(true))
    assert(new DoubleRef(0) != new BooleanRef(false))
    assert(new DoubleRef(5) == new IntRef(5))
    assert(new DoubleRef(5) != new IntRef(1))
    assert(new DoubleRef(5) == new FloatRef(5))
    assert(new DoubleRef(5) != new FloatRef(1))

    new DoubleRef(2) match {
      case DoubleRef(1) => throw new AssertionError
      case DoubleRef(2) => // do nothing
      case DoubleRef(3) => throw new AssertionError
    }

    val conv1: DoubleRef = new DoubleRef(7).asInstanceOf[ReadDoubleRef]
    assert(conv1.toConst == 7)
    val conv2: DoubleRef = 7
    assert(conv2.toConst == 7)
  }
}
