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
import ApplyUpdateTest._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Vec3dTest extends FunSuite {

  test("Factories") {
    testArrayFromSize(DataArray[Vec3d, SByte](_))
    testArrayFromData[Vec3d, SByte](DataArray[Vec3d, SByte](_))
    testBufferFromSize(DataBuffer[Vec3d, SByte](_))
    testBufferFromData(DataBuffer[Vec3d, SByte](_))
    testViewFromData(DataView[Vec3d, SByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec3d, SByte](_))
    testReadViewFromData(ReadDataView[Vec3d, SByte](_, _, _))
    testArrayFromCollection[Vec3d, SByte]((a: IndexedSeq[ReadVec3d]) => DataArray[Vec3d, SByte](a: _*))
    testArrayFromCollection[Vec3d, SByte]((a: IndexedSeq[ReadVec3d]) => DataArray[Vec3d, SByte](a))
    testBufferFromCollection[Vec3d, SByte]((a: IndexedSeq[ReadVec3d]) => DataBuffer[Vec3d, SByte](a: _*))
    testBufferFromCollection[Vec3d, SByte]((a: IndexedSeq[ReadVec3d]) => DataBuffer[Vec3d, SByte](a))

    testArrayFromSize(DataArray[Vec3d, UByte](_))
    testArrayFromData[Vec3d, UByte](DataArray[Vec3d, UByte](_))
    testBufferFromSize(DataBuffer[Vec3d, UByte](_))
    testBufferFromData(DataBuffer[Vec3d, UByte](_))
    testViewFromData(DataView[Vec3d, UByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec3d, UByte](_))
    testReadViewFromData(ReadDataView[Vec3d, UByte](_, _, _))
    testArrayFromCollection[Vec3d, UByte]((a: IndexedSeq[ReadVec3d]) => DataArray[Vec3d, UByte](a: _*))
    testArrayFromCollection[Vec3d, UByte]((a: IndexedSeq[ReadVec3d]) => DataArray[Vec3d, UByte](a))
    testBufferFromCollection[Vec3d, UByte]((a: IndexedSeq[ReadVec3d]) => DataBuffer[Vec3d, UByte](a: _*))
    testBufferFromCollection[Vec3d, UByte]((a: IndexedSeq[ReadVec3d]) => DataBuffer[Vec3d, UByte](a))

    testArrayFromSize(DataArray[Vec3d, SShort](_))
    testArrayFromData[Vec3d, SShort](DataArray[Vec3d, SShort](_))
    testBufferFromSize(DataBuffer[Vec3d, SShort](_))
    testBufferFromData(DataBuffer[Vec3d, SShort](_))
    testViewFromData(DataView[Vec3d, SShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec3d, SShort](_))
    testReadViewFromData(ReadDataView[Vec3d, SShort](_, _, _))
    testArrayFromCollection[Vec3d, SShort]((a: IndexedSeq[ReadVec3d]) => DataArray[Vec3d, SShort](a: _*))
    testArrayFromCollection[Vec3d, SShort]((a: IndexedSeq[ReadVec3d]) => DataArray[Vec3d, SShort](a))
    testBufferFromCollection[Vec3d, SShort]((a: IndexedSeq[ReadVec3d]) => DataBuffer[Vec3d, SShort](a: _*))
    testBufferFromCollection[Vec3d, SShort]((a: IndexedSeq[ReadVec3d]) => DataBuffer[Vec3d, SShort](a))

    testArrayFromSize(DataArray[Vec3d, UShort](_))
    testArrayFromData[Vec3d, UShort](DataArray[Vec3d, UShort](_))
    testBufferFromSize(DataBuffer[Vec3d, UShort](_))
    testBufferFromData(DataBuffer[Vec3d, UShort](_))
    testViewFromData(DataView[Vec3d, UShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec3d, UShort](_))
    testReadViewFromData(ReadDataView[Vec3d, UShort](_, _, _))
    testArrayFromCollection[Vec3d, UShort]((a: IndexedSeq[ReadVec3d]) => DataArray[Vec3d, UShort](a: _*))
    testArrayFromCollection[Vec3d, UShort]((a: IndexedSeq[ReadVec3d]) => DataArray[Vec3d, UShort](a))
    testBufferFromCollection[Vec3d, UShort]((a: IndexedSeq[ReadVec3d]) => DataBuffer[Vec3d, UShort](a: _*))
    testBufferFromCollection[Vec3d, UShort]((a: IndexedSeq[ReadVec3d]) => DataBuffer[Vec3d, UShort](a))

    testArrayFromSize(DataArray[Vec3d, SInt](_))
    testArrayFromData[Vec3d, SInt](DataArray[Vec3d, SInt](_))
    testBufferFromSize(DataBuffer[Vec3d, SInt](_))
    testBufferFromData(DataBuffer[Vec3d, SInt](_))
    testViewFromData(DataView[Vec3d, SInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec3d, SInt](_))
    testReadViewFromData(ReadDataView[Vec3d, SInt](_, _, _))
    testArrayFromCollection[Vec3d, SInt]((a: IndexedSeq[ReadVec3d]) => DataArray[Vec3d, SInt](a: _*))
    testArrayFromCollection[Vec3d, SInt]((a: IndexedSeq[ReadVec3d]) => DataArray[Vec3d, SInt](a))
    testBufferFromCollection[Vec3d, SInt]((a: IndexedSeq[ReadVec3d]) => DataBuffer[Vec3d, SInt](a: _*))
    testBufferFromCollection[Vec3d, SInt]((a: IndexedSeq[ReadVec3d]) => DataBuffer[Vec3d, SInt](a))

    testArrayFromSize(DataArray[Vec3d, UInt](_))
    testArrayFromData[Vec3d, UInt](DataArray[Vec3d, UInt](_))
    testBufferFromSize(DataBuffer[Vec3d, UInt](_))
    testBufferFromData(DataBuffer[Vec3d, UInt](_))
    testViewFromData(DataView[Vec3d, UInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec3d, UInt](_))
    testReadViewFromData(ReadDataView[Vec3d, UInt](_, _, _))
    testArrayFromCollection[Vec3d, UInt]((a: IndexedSeq[ReadVec3d]) => DataArray[Vec3d, UInt](a: _*))
    testArrayFromCollection[Vec3d, UInt]((a: IndexedSeq[ReadVec3d]) => DataArray[Vec3d, UInt](a))
    testBufferFromCollection[Vec3d, UInt]((a: IndexedSeq[ReadVec3d]) => DataBuffer[Vec3d, UInt](a: _*))
    testBufferFromCollection[Vec3d, UInt]((a: IndexedSeq[ReadVec3d]) => DataBuffer[Vec3d, UInt](a))
    
    testArrayFromSize(DataArray[Vec3d, HalfFloat](_))
    testArrayFromData[Vec3d, HalfFloat](DataArray[Vec3d, HalfFloat](_))
    testBufferFromSize(DataBuffer[Vec3d, HalfFloat](_))
    testBufferFromData(DataBuffer[Vec3d, HalfFloat](_))
    testViewFromData(DataView[Vec3d, HalfFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec3d, HalfFloat](_))
    testReadViewFromData(ReadDataView[Vec3d, HalfFloat](_, _, _))
    testArrayFromCollection[Vec3d, HalfFloat]((a: IndexedSeq[ReadVec3d]) => DataArray[Vec3d, HalfFloat](a: _*))
    testArrayFromCollection[Vec3d, HalfFloat]((a: IndexedSeq[ReadVec3d]) => DataArray[Vec3d, HalfFloat](a))
    testBufferFromCollection[Vec3d, HalfFloat]((a: IndexedSeq[ReadVec3d]) => DataBuffer[Vec3d, HalfFloat](a: _*))
    testBufferFromCollection[Vec3d, HalfFloat]((a: IndexedSeq[ReadVec3d]) => DataBuffer[Vec3d, HalfFloat](a))
    
    testArrayFromSize(DataArray[Vec3d, RawFloat](_))
    testArrayFromData[Vec3d, RawFloat](DataArray[Vec3d, RawFloat](_))
    testBufferFromSize(DataBuffer[Vec3d, RawFloat](_))
    testBufferFromData(DataBuffer[Vec3d, RawFloat](_))
    testViewFromData(DataView[Vec3d, RawFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec3d, RawFloat](_))
    testReadViewFromData(ReadDataView[Vec3d, RawFloat](_, _, _))
    testArrayFromCollection[Vec3d, RawFloat]((a: IndexedSeq[ReadVec3d]) => DataArray[Vec3d, RawFloat](a: _*))
    testArrayFromCollection[Vec3d, RawFloat]((a: IndexedSeq[ReadVec3d]) => DataArray[Vec3d, RawFloat](a))
    testBufferFromCollection[Vec3d, RawFloat]((a: IndexedSeq[ReadVec3d]) => DataBuffer[Vec3d, RawFloat](a: _*))
    testBufferFromCollection[Vec3d, RawFloat]((a: IndexedSeq[ReadVec3d]) => DataBuffer[Vec3d, RawFloat](a))
    
    testArrayFromSize(DataArray[Vec3d, RawDouble](_))
    testArrayFromData[Vec3d, RawDouble](DataArray[Vec3d, RawDouble](_))
    testBufferFromSize(DataBuffer[Vec3d, RawDouble](_))
    testBufferFromData(DataBuffer[Vec3d, RawDouble](_))
    testViewFromData(DataView[Vec3d, RawDouble](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec3d, RawDouble](_))
    testReadViewFromData(ReadDataView[Vec3d, RawDouble](_, _, _))
    testArrayFromCollection[Vec3d, RawDouble]((a: IndexedSeq[ReadVec3d]) => DataArray[Vec3d, RawDouble](a: _*))
    testArrayFromCollection[Vec3d, RawDouble]((a: IndexedSeq[ReadVec3d]) => DataArray[Vec3d, RawDouble](a))
    testBufferFromCollection[Vec3d, RawDouble]((a: IndexedSeq[ReadVec3d]) => DataBuffer[Vec3d, RawDouble](a: _*))
    testBufferFromCollection[Vec3d, RawDouble]((a: IndexedSeq[ReadVec3d]) => DataBuffer[Vec3d, RawDouble](a))
  }
  
    
  test("Apply/Update") {
    testApplyUpdateArray[Vec3d, SByte](DataArray[Vec3d, SByte](_))
    testApplyUpdateBuffer(DataBuffer[Vec3d, SByte](_))
    testApplyUpdateView(DataView[Vec3d, SByte](_, _, _))
    
    testApplyUpdateArray[Vec3d, UByte](DataArray[Vec3d, UByte](_))
    testApplyUpdateBuffer(DataBuffer[Vec3d, UByte](_))
    testApplyUpdateView(DataView[Vec3d, UByte](_, _, _))
    
    testApplyUpdateArray[Vec3d, SShort](DataArray[Vec3d, SShort](_))
    testApplyUpdateBuffer(DataBuffer[Vec3d, SShort](_))
    testApplyUpdateView(DataView[Vec3d, SShort](_, _, _))
    
    testApplyUpdateArray[Vec3d, UShort](DataArray[Vec3d, UShort](_))
    testApplyUpdateBuffer(DataBuffer[Vec3d, UShort](_))
    testApplyUpdateView(DataView[Vec3d, UShort](_, _, _))
    
    testApplyUpdateArray[Vec3d, SInt](DataArray[Vec3d, SInt](_))
    testApplyUpdateBuffer(DataBuffer[Vec3d, SInt](_))
    testApplyUpdateView(DataView[Vec3d, SInt](_, _, _))
    
    testApplyUpdateArray[Vec3d, UInt](DataArray[Vec3d, UInt](_))
    testApplyUpdateBuffer(DataBuffer[Vec3d, UInt](_))
    testApplyUpdateView(DataView[Vec3d, UInt](_, _, _))
    
    testApplyUpdateArray[Vec3d, HalfFloat](DataArray[Vec3d, HalfFloat](_))
    testApplyUpdateBuffer(DataBuffer[Vec3d, HalfFloat](_))
    testApplyUpdateView(DataView[Vec3d, HalfFloat](_, _, _))
    
    testApplyUpdateArray[Vec3d, RawFloat](DataArray[Vec3d, RawFloat](_))
    testApplyUpdateBuffer(DataBuffer[Vec3d, RawFloat](_))
    testApplyUpdateView(DataView[Vec3d, RawFloat](_, _, _))
    
    testApplyUpdateArray[Vec3d, RawDouble](DataArray[Vec3d, RawDouble](_))
    testApplyUpdateBuffer(DataBuffer[Vec3d, RawDouble](_))
    testApplyUpdateView(DataView[Vec3d, RawDouble](_, _, _))
  }
}
