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
import simplex3d.math.floatm._
import simplex3d.buffer._
import simplex3d.buffer.floatm._

import Descriptors._
import FactoryTest._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Float1Test extends FunSuite {

  test("Factories") {
    testArrayFromSize(DataArray[Float1, SByte](_))
    testArrayFromData[Float1, SByte](DataArray[Float1, SByte](_))
    testBufferFromSize(DataBuffer[Float1, SByte](_))
    testBufferFromData(DataBuffer[Float1, SByte](_))
    testViewFromData(DataView[Float1, SByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Float1, SByte](_))
    testReadViewFromData(ReadDataView[Float1, SByte](_, _, _))
    
    testArrayFromSize(DataArray[Float1, UByte](_))
    testArrayFromData[Float1, UByte](DataArray[Float1, UByte](_))
    testBufferFromSize(DataBuffer[Float1, UByte](_))
    testBufferFromData(DataBuffer[Float1, UByte](_))
    testViewFromData(DataView[Float1, UByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Float1, UByte](_))
    testReadViewFromData(ReadDataView[Float1, UByte](_, _, _))
    
    testArrayFromSize(DataArray[Float1, SShort](_))
    testArrayFromData[Float1, SShort](DataArray[Float1, SShort](_))
    testBufferFromSize(DataBuffer[Float1, SShort](_))
    testBufferFromData(DataBuffer[Float1, SShort](_))
    testViewFromData(DataView[Float1, SShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Float1, SShort](_))
    testReadViewFromData(ReadDataView[Float1, SShort](_, _, _))
    
    testArrayFromSize(DataArray[Float1, UShort](_))
    testArrayFromData[Float1, UShort](DataArray[Float1, UShort](_))
    testBufferFromSize(DataBuffer[Float1, UShort](_))
    testBufferFromData(DataBuffer[Float1, UShort](_))
    testViewFromData(DataView[Float1, UShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Float1, UShort](_))
    testReadViewFromData(ReadDataView[Float1, UShort](_, _, _))
    
    testArrayFromSize(DataArray[Float1, SInt](_))
    testArrayFromData[Float1, SInt](DataArray[Float1, SInt](_))
    testBufferFromSize(DataBuffer[Float1, SInt](_))
    testBufferFromData(DataBuffer[Float1, SInt](_))
    testViewFromData(DataView[Float1, SInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Float1, SInt](_))
    testReadViewFromData(ReadDataView[Float1, SInt](_, _, _))
    
    testArrayFromSize(DataArray[Float1, UInt](_))
    testArrayFromData[Float1, UInt](DataArray[Float1, UInt](_))
    testBufferFromSize(DataBuffer[Float1, UInt](_))
    testBufferFromData(DataBuffer[Float1, UInt](_))
    testViewFromData(DataView[Float1, UInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Float1, UInt](_))
    testReadViewFromData(ReadDataView[Float1, UInt](_, _, _))
    
    testArrayFromSize(DataArray[Float1, HalfFloat](_))
    testArrayFromData[Float1, HalfFloat](DataArray[Float1, HalfFloat](_))
    testBufferFromSize(DataBuffer[Float1, HalfFloat](_))
    testBufferFromData(DataBuffer[Float1, HalfFloat](_))
    testViewFromData(DataView[Float1, HalfFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Float1, HalfFloat](_))
    testReadViewFromData(ReadDataView[Float1, HalfFloat](_, _, _))
    
    testArrayFromSize(DataArray[Float1, RawFloat](_))
    testArrayFromData[Float1, RawFloat](DataArray[Float1, RawFloat](_))
    testBufferFromSize(DataBuffer[Float1, RawFloat](_))
    testBufferFromData(DataBuffer[Float1, RawFloat](_))
    testViewFromData(DataView[Float1, RawFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Float1, RawFloat](_))
    testReadViewFromData(ReadDataView[Float1, RawFloat](_, _, _))
  }
}
