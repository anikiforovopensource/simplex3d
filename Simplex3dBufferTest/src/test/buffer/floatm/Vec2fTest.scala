/*
 * Simplex3d, BufferTest package
 * Copyright (C) 2010, Simplex3d Team
 *
 * This file is part of Simplex3dBufferTest.
 *
 * Simplex3dBufferTest is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dBufferTest is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package test.buffer
package floatm

import org.scalatest._

import simplex3d.math._
import simplex3d.math.floatm._
import simplex3d.buffer.{allocateDirectBuffer => alloc, _}
import simplex3d.buffer.floatm._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Vec2fTest extends FunSuite {

  test("Factories") {
    import java.nio._
    import AttributeTest._
    import Descriptors._
    testArray(DataArray[Vec2f, UShort](10), false, CharBuffer.wrap(new Array[Char](20)))
    testBuffer(DataBuffer[Vec2f, UShort](10), false, CharBuffer.wrap(new Array[Char](20)))
    testView(DataView[Vec2f, UShort](alloc(10*2*2), 0, 2), 0, 2, false, CharBuffer.wrap(new Array[Char](20)))
  }
}
