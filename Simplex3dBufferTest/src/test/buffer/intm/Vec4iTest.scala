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
class Vec4iTest extends FunSuite {

  test("Factories") {
    testArrayFromSize(DataArray[Vec4i, SByte](_))
    testArrayFromData[Vec4i, SByte](DataArray[Vec4i, SByte](_))
    testBufferFromSize(DataBuffer[Vec4i, SByte](_))
    testBufferFromData(DataBuffer[Vec4i, SByte](_))
    testViewFromData(DataView[Vec4i, SByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4i, SByte](_))
    testReadViewFromData(ReadDataView[Vec4i, SByte](_, _, _))
    
    testArrayFromSize(DataArray[Vec4i, UByte](_))
    testArrayFromData[Vec4i, UByte](DataArray[Vec4i, UByte](_))
    testBufferFromSize(DataBuffer[Vec4i, UByte](_))
    testBufferFromData(DataBuffer[Vec4i, UByte](_))
    testViewFromData(DataView[Vec4i, UByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4i, UByte](_))
    testReadViewFromData(ReadDataView[Vec4i, UByte](_, _, _))
    
    testArrayFromSize(DataArray[Vec4i, SShort](_))
    testArrayFromData[Vec4i, SShort](DataArray[Vec4i, SShort](_))
    testBufferFromSize(DataBuffer[Vec4i, SShort](_))
    testBufferFromData(DataBuffer[Vec4i, SShort](_))
    testViewFromData(DataView[Vec4i, SShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4i, SShort](_))
    testReadViewFromData(ReadDataView[Vec4i, SShort](_, _, _))
    
    testArrayFromSize(DataArray[Vec4i, UShort](_))
    testArrayFromData[Vec4i, UShort](DataArray[Vec4i, UShort](_))
    testBufferFromSize(DataBuffer[Vec4i, UShort](_))
    testBufferFromData(DataBuffer[Vec4i, UShort](_))
    testViewFromData(DataView[Vec4i, UShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4i, UShort](_))
    testReadViewFromData(ReadDataView[Vec4i, UShort](_, _, _))
    
    testArrayFromSize(DataArray[Vec4i, SInt](_))
    testArrayFromData[Vec4i, SInt](DataArray[Vec4i, SInt](_))
    testBufferFromSize(DataBuffer[Vec4i, SInt](_))
    testBufferFromData(DataBuffer[Vec4i, SInt](_))
    testViewFromData(DataView[Vec4i, SInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4i, SInt](_))
    testReadViewFromData(ReadDataView[Vec4i, SInt](_, _, _))
    
    testArrayFromSize(DataArray[Vec4i, UInt](_))
    testArrayFromData[Vec4i, UInt](DataArray[Vec4i, UInt](_))
    testBufferFromSize(DataBuffer[Vec4i, UInt](_))
    testBufferFromData(DataBuffer[Vec4i, UInt](_))
    testViewFromData(DataView[Vec4i, UInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4i, UInt](_))
    testReadViewFromData(ReadDataView[Vec4i, UInt](_, _, _))
  }
}
