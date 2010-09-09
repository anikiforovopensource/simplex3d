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
class Double1Test extends FunSuite {

  test("Factories") {
    testArrayFromSize(DataArray[Double1, SByte](_))
    testArrayFromData[Double1, SByte](DataArray[Double1, SByte](_))
    testBufferFromSize(DataBuffer[Double1, SByte](_))
    testBufferFromData(DataBuffer[Double1, SByte](_))
    testViewFromData(DataView[Double1, SByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Double1, SByte](_))
    testReadViewFromData(ReadDataView[Double1, SByte](_, _, _))
    
    testArrayFromSize(DataArray[Double1, UByte](_))
    testArrayFromData[Double1, UByte](DataArray[Double1, UByte](_))
    testBufferFromSize(DataBuffer[Double1, UByte](_))
    testBufferFromData(DataBuffer[Double1, UByte](_))
    testViewFromData(DataView[Double1, UByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Double1, UByte](_))
    testReadViewFromData(ReadDataView[Double1, UByte](_, _, _))
    
    testArrayFromSize(DataArray[Double1, SShort](_))
    testArrayFromData[Double1, SShort](DataArray[Double1, SShort](_))
    testBufferFromSize(DataBuffer[Double1, SShort](_))
    testBufferFromData(DataBuffer[Double1, SShort](_))
    testViewFromData(DataView[Double1, SShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Double1, SShort](_))
    testReadViewFromData(ReadDataView[Double1, SShort](_, _, _))
    
    testArrayFromSize(DataArray[Double1, UShort](_))
    testArrayFromData[Double1, UShort](DataArray[Double1, UShort](_))
    testBufferFromSize(DataBuffer[Double1, UShort](_))
    testBufferFromData(DataBuffer[Double1, UShort](_))
    testViewFromData(DataView[Double1, UShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Double1, UShort](_))
    testReadViewFromData(ReadDataView[Double1, UShort](_, _, _))
    
    testArrayFromSize(DataArray[Double1, SInt](_))
    testArrayFromData[Double1, SInt](DataArray[Double1, SInt](_))
    testBufferFromSize(DataBuffer[Double1, SInt](_))
    testBufferFromData(DataBuffer[Double1, SInt](_))
    testViewFromData(DataView[Double1, SInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Double1, SInt](_))
    testReadViewFromData(ReadDataView[Double1, SInt](_, _, _))
    
    testArrayFromSize(DataArray[Double1, UInt](_))
    testArrayFromData[Double1, UInt](DataArray[Double1, UInt](_))
    testBufferFromSize(DataBuffer[Double1, UInt](_))
    testBufferFromData(DataBuffer[Double1, UInt](_))
    testViewFromData(DataView[Double1, UInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Double1, UInt](_))
    testReadViewFromData(ReadDataView[Double1, UInt](_, _, _))
    
    testArrayFromSize(DataArray[Double1, HalfFloat](_))
    testArrayFromData[Double1, HalfFloat](DataArray[Double1, HalfFloat](_))
    testBufferFromSize(DataBuffer[Double1, HalfFloat](_))
    testBufferFromData(DataBuffer[Double1, HalfFloat](_))
    testViewFromData(DataView[Double1, HalfFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Double1, HalfFloat](_))
    testReadViewFromData(ReadDataView[Double1, HalfFloat](_, _, _))
    
    testArrayFromSize(DataArray[Double1, RawFloat](_))
    testArrayFromData[Double1, RawFloat](DataArray[Double1, RawFloat](_))
    testBufferFromSize(DataBuffer[Double1, RawFloat](_))
    testBufferFromData(DataBuffer[Double1, RawFloat](_))
    testViewFromData(DataView[Double1, RawFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Double1, RawFloat](_))
    testReadViewFromData(ReadDataView[Double1, RawFloat](_, _, _))
    
    testArrayFromSize(DataArray[Double1, RawDouble](_))
    testArrayFromData[Double1, RawDouble](DataArray[Double1, RawDouble](_))
    testBufferFromSize(DataBuffer[Double1, RawDouble](_))
    testBufferFromData(DataBuffer[Double1, RawDouble](_))
    testViewFromData(DataView[Double1, RawDouble](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Double1, RawDouble](_))
    testReadViewFromData(ReadDataView[Double1, RawDouble](_, _, _))
  }
}
