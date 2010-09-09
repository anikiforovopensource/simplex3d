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
package intm

import org.scalatest._
import simplex3d.math.intm._
import simplex3d.buffer._
import simplex3d.buffer.intm._

import Descriptors._
import FactoryTest._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Int1Test extends FunSuite {

  test("Factories") {
    testArrayFromSize(DataArray[Int1, SByte](_))
    testArrayFromData[Int1, SByte](DataArray[Int1, SByte](_))
    testBufferFromSize(DataBuffer[Int1, SByte](_))
    testBufferFromData(DataBuffer[Int1, SByte](_))
    testViewFromData(DataView[Int1, SByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Int1, SByte](_))
    testReadViewFromData(ReadDataView[Int1, SByte](_, _, _))
    
    testArrayFromSize(DataArray[Int1, UByte](_))
    testArrayFromData[Int1, UByte](DataArray[Int1, UByte](_))
    testBufferFromSize(DataBuffer[Int1, UByte](_))
    testBufferFromData(DataBuffer[Int1, UByte](_))
    testViewFromData(DataView[Int1, UByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Int1, UByte](_))
    testReadViewFromData(ReadDataView[Int1, UByte](_, _, _))
    
    testArrayFromSize(DataArray[Int1, SShort](_))
    testArrayFromData[Int1, SShort](DataArray[Int1, SShort](_))
    testBufferFromSize(DataBuffer[Int1, SShort](_))
    testBufferFromData(DataBuffer[Int1, SShort](_))
    testViewFromData(DataView[Int1, SShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Int1, SShort](_))
    testReadViewFromData(ReadDataView[Int1, SShort](_, _, _))
    
    testArrayFromSize(DataArray[Int1, UShort](_))
    testArrayFromData[Int1, UShort](DataArray[Int1, UShort](_))
    testBufferFromSize(DataBuffer[Int1, UShort](_))
    testBufferFromData(DataBuffer[Int1, UShort](_))
    testViewFromData(DataView[Int1, UShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Int1, UShort](_))
    testReadViewFromData(ReadDataView[Int1, UShort](_, _, _))
    
    testArrayFromSize(DataArray[Int1, SInt](_))
    testArrayFromData[Int1, SInt](DataArray[Int1, SInt](_))
    testBufferFromSize(DataBuffer[Int1, SInt](_))
    testBufferFromData(DataBuffer[Int1, SInt](_))
    testViewFromData(DataView[Int1, SInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Int1, SInt](_))
    testReadViewFromData(ReadDataView[Int1, SInt](_, _, _))
    
    testArrayFromSize(DataArray[Int1, UInt](_))
    testArrayFromData[Int1, UInt](DataArray[Int1, UInt](_))
    testBufferFromSize(DataBuffer[Int1, UInt](_))
    testBufferFromData(DataBuffer[Int1, UInt](_))
    testViewFromData(DataView[Int1, UInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Int1, UInt](_))
    testReadViewFromData(ReadDataView[Int1, UInt](_, _, _))
  }
}
