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
class Vec3iTest extends FunSuite {

  test("Factories") {
    testArrayFromSize(DataArray[Vec3i, SByte](_))
    testArrayFromData[Vec3i, SByte](DataArray[Vec3i, SByte](_))
    testBufferFromSize(DataBuffer[Vec3i, SByte](_))
    testBufferFromData(DataBuffer[Vec3i, SByte](_))
    testViewFromData(DataView[Vec3i, SByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec3i, SByte](_))
    testReadViewFromData(ReadDataView[Vec3i, SByte](_, _, _))
    
    testArrayFromSize(DataArray[Vec3i, UByte](_))
    testArrayFromData[Vec3i, UByte](DataArray[Vec3i, UByte](_))
    testBufferFromSize(DataBuffer[Vec3i, UByte](_))
    testBufferFromData(DataBuffer[Vec3i, UByte](_))
    testViewFromData(DataView[Vec3i, UByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec3i, UByte](_))
    testReadViewFromData(ReadDataView[Vec3i, UByte](_, _, _))
    
    testArrayFromSize(DataArray[Vec3i, SShort](_))
    testArrayFromData[Vec3i, SShort](DataArray[Vec3i, SShort](_))
    testBufferFromSize(DataBuffer[Vec3i, SShort](_))
    testBufferFromData(DataBuffer[Vec3i, SShort](_))
    testViewFromData(DataView[Vec3i, SShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec3i, SShort](_))
    testReadViewFromData(ReadDataView[Vec3i, SShort](_, _, _))
    
    testArrayFromSize(DataArray[Vec3i, UShort](_))
    testArrayFromData[Vec3i, UShort](DataArray[Vec3i, UShort](_))
    testBufferFromSize(DataBuffer[Vec3i, UShort](_))
    testBufferFromData(DataBuffer[Vec3i, UShort](_))
    testViewFromData(DataView[Vec3i, UShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec3i, UShort](_))
    testReadViewFromData(ReadDataView[Vec3i, UShort](_, _, _))
    
    testArrayFromSize(DataArray[Vec3i, SInt](_))
    testArrayFromData[Vec3i, SInt](DataArray[Vec3i, SInt](_))
    testBufferFromSize(DataBuffer[Vec3i, SInt](_))
    testBufferFromData(DataBuffer[Vec3i, SInt](_))
    testViewFromData(DataView[Vec3i, SInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec3i, SInt](_))
    testReadViewFromData(ReadDataView[Vec3i, SInt](_, _, _))
    
    testArrayFromSize(DataArray[Vec3i, UInt](_))
    testArrayFromData[Vec3i, UInt](DataArray[Vec3i, UInt](_))
    testBufferFromSize(DataBuffer[Vec3i, UInt](_))
    testBufferFromData(DataBuffer[Vec3i, UInt](_))
    testViewFromData(DataView[Vec3i, UInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec3i, UInt](_))
    testReadViewFromData(ReadDataView[Vec3i, UInt](_, _, _))
  }
}
