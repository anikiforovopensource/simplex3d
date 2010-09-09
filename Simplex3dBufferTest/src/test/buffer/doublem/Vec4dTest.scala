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
package doublem

import org.scalatest._
import simplex3d.math.doublem._
import simplex3d.buffer._
import simplex3d.buffer.doublem._

import Descriptors._
import FactoryTest._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Vec4dTest extends FunSuite {

  test("Factories") {
    testArrayFromSize(DataArray[Vec4d, SByte](_))
    testArrayFromData[Vec4d, SByte](DataArray[Vec4d, SByte](_))
    testBufferFromSize(DataBuffer[Vec4d, SByte](_))
    testBufferFromData(DataBuffer[Vec4d, SByte](_))
    testViewFromData(DataView[Vec4d, SByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4d, SByte](_))
    testReadViewFromData(ReadDataView[Vec4d, SByte](_, _, _))
    
    testArrayFromSize(DataArray[Vec4d, UByte](_))
    testArrayFromData[Vec4d, UByte](DataArray[Vec4d, UByte](_))
    testBufferFromSize(DataBuffer[Vec4d, UByte](_))
    testBufferFromData(DataBuffer[Vec4d, UByte](_))
    testViewFromData(DataView[Vec4d, UByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4d, UByte](_))
    testReadViewFromData(ReadDataView[Vec4d, UByte](_, _, _))
    
    testArrayFromSize(DataArray[Vec4d, SShort](_))
    testArrayFromData[Vec4d, SShort](DataArray[Vec4d, SShort](_))
    testBufferFromSize(DataBuffer[Vec4d, SShort](_))
    testBufferFromData(DataBuffer[Vec4d, SShort](_))
    testViewFromData(DataView[Vec4d, SShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4d, SShort](_))
    testReadViewFromData(ReadDataView[Vec4d, SShort](_, _, _))
    
    testArrayFromSize(DataArray[Vec4d, UShort](_))
    testArrayFromData[Vec4d, UShort](DataArray[Vec4d, UShort](_))
    testBufferFromSize(DataBuffer[Vec4d, UShort](_))
    testBufferFromData(DataBuffer[Vec4d, UShort](_))
    testViewFromData(DataView[Vec4d, UShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4d, UShort](_))
    testReadViewFromData(ReadDataView[Vec4d, UShort](_, _, _))
    
    testArrayFromSize(DataArray[Vec4d, SInt](_))
    testArrayFromData[Vec4d, SInt](DataArray[Vec4d, SInt](_))
    testBufferFromSize(DataBuffer[Vec4d, SInt](_))
    testBufferFromData(DataBuffer[Vec4d, SInt](_))
    testViewFromData(DataView[Vec4d, SInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4d, SInt](_))
    testReadViewFromData(ReadDataView[Vec4d, SInt](_, _, _))
    
    testArrayFromSize(DataArray[Vec4d, UInt](_))
    testArrayFromData[Vec4d, UInt](DataArray[Vec4d, UInt](_))
    testBufferFromSize(DataBuffer[Vec4d, UInt](_))
    testBufferFromData(DataBuffer[Vec4d, UInt](_))
    testViewFromData(DataView[Vec4d, UInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4d, UInt](_))
    testReadViewFromData(ReadDataView[Vec4d, UInt](_, _, _))
    
    testArrayFromSize(DataArray[Vec4d, HalfFloat](_))
    testArrayFromData[Vec4d, HalfFloat](DataArray[Vec4d, HalfFloat](_))
    testBufferFromSize(DataBuffer[Vec4d, HalfFloat](_))
    testBufferFromData(DataBuffer[Vec4d, HalfFloat](_))
    testViewFromData(DataView[Vec4d, HalfFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4d, HalfFloat](_))
    testReadViewFromData(ReadDataView[Vec4d, HalfFloat](_, _, _))
    
    testArrayFromSize(DataArray[Vec4d, RawFloat](_))
    testArrayFromData[Vec4d, RawFloat](DataArray[Vec4d, RawFloat](_))
    testBufferFromSize(DataBuffer[Vec4d, RawFloat](_))
    testBufferFromData(DataBuffer[Vec4d, RawFloat](_))
    testViewFromData(DataView[Vec4d, RawFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4d, RawFloat](_))
    testReadViewFromData(ReadDataView[Vec4d, RawFloat](_, _, _))
    
    testArrayFromSize(DataArray[Vec4d, RawDouble](_))
    testArrayFromData[Vec4d, RawDouble](DataArray[Vec4d, RawDouble](_))
    testBufferFromSize(DataBuffer[Vec4d, RawDouble](_))
    testBufferFromData(DataBuffer[Vec4d, RawDouble](_))
    testViewFromData(DataView[Vec4d, RawDouble](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4d, RawDouble](_))
    testReadViewFromData(ReadDataView[Vec4d, RawDouble](_, _, _))
  }
}
