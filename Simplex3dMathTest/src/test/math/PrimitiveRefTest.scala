/*
 * Simplex3dMath - Test Package
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
    
    
    val t: ReadBooleanRef = new BooleanRef(true)
    val f: ReadBooleanRef = new BooleanRef(false)
    
    assert(!(r && t))
    assert(!(r && f))
    
    assert(r || t)
    assert(!(r || f))
    
    assert(r ^ t)
    assert(!(r ^ f))
    
    
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
      val conv: ReadBooleanRef = true
      assert(conv.toConst == true)
      
      val s: Boolean = conv
      assert(s == true)
      assert(s != false)
      assert(s.asInstanceOf[Any] == true)
    }
    {
      val conv: ReadBooleanRef = false
      assert(conv.toConst == false)
      
      val s: Boolean = conv
      assert(s == false)
      assert(s != true)
      assert(s.asInstanceOf[Any] == false)
    }
  }
  
  test("Mutable IntRef") {
    val r = new IntRef(4)
    
    
    assert(r * (new IntRef(2)).asInstanceOf[ReadIntRef] == 8)
    assert(r / (new IntRef(2)).asInstanceOf[ReadIntRef] == 2)
    assert(r + (new IntRef(3)).asInstanceOf[ReadIntRef] == 7)
    assert(r - (new IntRef(3)).asInstanceOf[ReadIntRef] == 1)
    
    assert(r % (new IntRef(3)).asInstanceOf[ReadIntRef] == 1)
    
    r := -4
    assert(r >> (new IntRef(1)).asInstanceOf[ReadIntRef] == -2)
    assert(r >>> (new IntRef(1)).asInstanceOf[ReadIntRef] == 0x7FFFFFFE)
    
    r := 4
    assert(r << (new IntRef(1)).asInstanceOf[ReadIntRef] == 8)
    assert((r & (new IntRef(6)).asInstanceOf[ReadIntRef]) == 4)
    assert((r | (new IntRef(2)).asInstanceOf[ReadIntRef]) == 6)
    assert((r ^ (new IntRef(6)).asInstanceOf[ReadIntRef]) == 2)
    
    
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

    val conv: ReadIntRef = 7
    assert(conv.toConst == 7)
    
    val s: Int = conv
    assert(s == 7)
    assert(s != 6)
    assert(s.asInstanceOf[Any] == 7)
  }
  
  test("Mutable FloatRef") {
    val r = new FloatRef(4)
    
    
    assert(r * (new FloatRef(2)).asInstanceOf[ReadFloatRef] == 8)
    assert(r / (new FloatRef(2)).asInstanceOf[ReadFloatRef] == 2)
    assert(r + (new FloatRef(3)).asInstanceOf[ReadFloatRef] == 7)
    assert(r - (new FloatRef(3)).asInstanceOf[ReadFloatRef] == 1)
    
    
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

    val conv: ReadFloatRef = 7
    assert(conv.toConst == 7)
    
    val s: Float = conv
    assert(s == 7)
    assert(s != 6)
    assert(s.asInstanceOf[Any] == 7)
  }
  
  test("Mutable DoubleRef") {
    val r = new DoubleRef(4)
    
    
    assert(r * (new DoubleRef(2)).asInstanceOf[ReadDoubleRef] == 8)
    assert(r / (new DoubleRef(2)).asInstanceOf[ReadDoubleRef] == 2)
    assert(r + (new DoubleRef(3)).asInstanceOf[ReadDoubleRef] == 7)
    assert(r - (new DoubleRef(3)).asInstanceOf[ReadDoubleRef] == 1)
    
    
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

    val conv: ReadDoubleRef = 7
    assert(conv.toConst == 7)
    
    val s: Double = conv
    assert(s == 7)
    assert(s != 6)
    assert(s.asInstanceOf[Any] == 7)
  }
}
