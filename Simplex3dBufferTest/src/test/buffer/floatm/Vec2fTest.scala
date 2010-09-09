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
class Vec2fTest extends FunSuite {

  test("Factories") {
    testArrayFromSize(DataArray[Vec2f, SByte](_))
    testArrayFromData[Vec2f, SByte](DataArray[Vec2f, SByte](_))
    testBufferFromSize(DataBuffer[Vec2f, SByte](_))
    testBufferFromData(DataBuffer[Vec2f, SByte](_))
    testViewFromData(DataView[Vec2f, SByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec2f, SByte](_))
    testReadViewFromData(ReadDataView[Vec2f, SByte](_, _, _))
    
    testArrayFromSize(DataArray[Vec2f, UByte](_))
    testArrayFromData[Vec2f, UByte](DataArray[Vec2f, UByte](_))
    testBufferFromSize(DataBuffer[Vec2f, UByte](_))
    testBufferFromData(DataBuffer[Vec2f, UByte](_))
    testViewFromData(DataView[Vec2f, UByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec2f, UByte](_))
    testReadViewFromData(ReadDataView[Vec2f, UByte](_, _, _))
    
    testArrayFromSize(DataArray[Vec2f, SShort](_))
    testArrayFromData[Vec2f, SShort](DataArray[Vec2f, SShort](_))
    testBufferFromSize(DataBuffer[Vec2f, SShort](_))
    testBufferFromData(DataBuffer[Vec2f, SShort](_))
    testViewFromData(DataView[Vec2f, SShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec2f, SShort](_))
    testReadViewFromData(ReadDataView[Vec2f, SShort](_, _, _))
    
    testArrayFromSize(DataArray[Vec2f, UShort](_))
    testArrayFromData[Vec2f, UShort](DataArray[Vec2f, UShort](_))
    testBufferFromSize(DataBuffer[Vec2f, UShort](_))
    testBufferFromData(DataBuffer[Vec2f, UShort](_))
    testViewFromData(DataView[Vec2f, UShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec2f, UShort](_))
    testReadViewFromData(ReadDataView[Vec2f, UShort](_, _, _))
    
    testArrayFromSize(DataArray[Vec2f, SInt](_))
    testArrayFromData[Vec2f, SInt](DataArray[Vec2f, SInt](_))
    testBufferFromSize(DataBuffer[Vec2f, SInt](_))
    testBufferFromData(DataBuffer[Vec2f, SInt](_))
    testViewFromData(DataView[Vec2f, SInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec2f, SInt](_))
    testReadViewFromData(ReadDataView[Vec2f, SInt](_, _, _))
    
    testArrayFromSize(DataArray[Vec2f, UInt](_))
    testArrayFromData[Vec2f, UInt](DataArray[Vec2f, UInt](_))
    testBufferFromSize(DataBuffer[Vec2f, UInt](_))
    testBufferFromData(DataBuffer[Vec2f, UInt](_))
    testViewFromData(DataView[Vec2f, UInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec2f, UInt](_))
    testReadViewFromData(ReadDataView[Vec2f, UInt](_, _, _))
    
    testArrayFromSize(DataArray[Vec2f, HalfFloat](_))
    testArrayFromData[Vec2f, HalfFloat](DataArray[Vec2f, HalfFloat](_))
    testBufferFromSize(DataBuffer[Vec2f, HalfFloat](_))
    testBufferFromData(DataBuffer[Vec2f, HalfFloat](_))
    testViewFromData(DataView[Vec2f, HalfFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec2f, HalfFloat](_))
    testReadViewFromData(ReadDataView[Vec2f, HalfFloat](_, _, _))
    
    testArrayFromSize(DataArray[Vec2f, RawFloat](_))
    testArrayFromData[Vec2f, RawFloat](DataArray[Vec2f, RawFloat](_))
    testBufferFromSize(DataBuffer[Vec2f, RawFloat](_))
    testBufferFromData(DataBuffer[Vec2f, RawFloat](_))
    testViewFromData(DataView[Vec2f, RawFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec2f, RawFloat](_))
    testReadViewFromData(ReadDataView[Vec2f, RawFloat](_, _, _))
  }
}
