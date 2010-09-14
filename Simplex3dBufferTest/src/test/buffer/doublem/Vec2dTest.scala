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
class Vec2dTest extends FunSuite {

  test("Factories") {
    testArrayFromSize(DataArray[Vec2d, SByte](_))
    testArrayFromData[Vec2d, SByte](DataArray[Vec2d, SByte](_))
    testBufferFromSize(DataBuffer[Vec2d, SByte](_))
    testBufferFromData(DataBuffer[Vec2d, SByte](_))
    testViewFromData(DataView[Vec2d, SByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec2d, SByte](_))
    testReadViewFromData(ReadDataView[Vec2d, SByte](_, _, _))
    testArrayFromCollection[Vec2d, SByte]((a: IndexedSeq[ReadVec2d]) => DataArray[Vec2d, SByte](a: _*))
    testArrayFromCollection[Vec2d, SByte]((a: IndexedSeq[ReadVec2d]) => DataArray[Vec2d, SByte](a))
    testBufferFromCollection[Vec2d, SByte]((a: IndexedSeq[ReadVec2d]) => DataBuffer[Vec2d, SByte](a: _*))
    testBufferFromCollection[Vec2d, SByte]((a: IndexedSeq[ReadVec2d]) => DataBuffer[Vec2d, SByte](a))

    testArrayFromSize(DataArray[Vec2d, UByte](_))
    testArrayFromData[Vec2d, UByte](DataArray[Vec2d, UByte](_))
    testBufferFromSize(DataBuffer[Vec2d, UByte](_))
    testBufferFromData(DataBuffer[Vec2d, UByte](_))
    testViewFromData(DataView[Vec2d, UByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec2d, UByte](_))
    testReadViewFromData(ReadDataView[Vec2d, UByte](_, _, _))
    testArrayFromCollection[Vec2d, UByte]((a: IndexedSeq[ReadVec2d]) => DataArray[Vec2d, UByte](a: _*))
    testArrayFromCollection[Vec2d, UByte]((a: IndexedSeq[ReadVec2d]) => DataArray[Vec2d, UByte](a))
    testBufferFromCollection[Vec2d, UByte]((a: IndexedSeq[ReadVec2d]) => DataBuffer[Vec2d, UByte](a: _*))
    testBufferFromCollection[Vec2d, UByte]((a: IndexedSeq[ReadVec2d]) => DataBuffer[Vec2d, UByte](a))

    testArrayFromSize(DataArray[Vec2d, SShort](_))
    testArrayFromData[Vec2d, SShort](DataArray[Vec2d, SShort](_))
    testBufferFromSize(DataBuffer[Vec2d, SShort](_))
    testBufferFromData(DataBuffer[Vec2d, SShort](_))
    testViewFromData(DataView[Vec2d, SShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec2d, SShort](_))
    testReadViewFromData(ReadDataView[Vec2d, SShort](_, _, _))
    testArrayFromCollection[Vec2d, SShort]((a: IndexedSeq[ReadVec2d]) => DataArray[Vec2d, SShort](a: _*))
    testArrayFromCollection[Vec2d, SShort]((a: IndexedSeq[ReadVec2d]) => DataArray[Vec2d, SShort](a))
    testBufferFromCollection[Vec2d, SShort]((a: IndexedSeq[ReadVec2d]) => DataBuffer[Vec2d, SShort](a: _*))
    testBufferFromCollection[Vec2d, SShort]((a: IndexedSeq[ReadVec2d]) => DataBuffer[Vec2d, SShort](a))

    testArrayFromSize(DataArray[Vec2d, UShort](_))
    testArrayFromData[Vec2d, UShort](DataArray[Vec2d, UShort](_))
    testBufferFromSize(DataBuffer[Vec2d, UShort](_))
    testBufferFromData(DataBuffer[Vec2d, UShort](_))
    testViewFromData(DataView[Vec2d, UShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec2d, UShort](_))
    testReadViewFromData(ReadDataView[Vec2d, UShort](_, _, _))
    testArrayFromCollection[Vec2d, UShort]((a: IndexedSeq[ReadVec2d]) => DataArray[Vec2d, UShort](a: _*))
    testArrayFromCollection[Vec2d, UShort]((a: IndexedSeq[ReadVec2d]) => DataArray[Vec2d, UShort](a))
    testBufferFromCollection[Vec2d, UShort]((a: IndexedSeq[ReadVec2d]) => DataBuffer[Vec2d, UShort](a: _*))
    testBufferFromCollection[Vec2d, UShort]((a: IndexedSeq[ReadVec2d]) => DataBuffer[Vec2d, UShort](a))

    testArrayFromSize(DataArray[Vec2d, SInt](_))
    testArrayFromData[Vec2d, SInt](DataArray[Vec2d, SInt](_))
    testBufferFromSize(DataBuffer[Vec2d, SInt](_))
    testBufferFromData(DataBuffer[Vec2d, SInt](_))
    testViewFromData(DataView[Vec2d, SInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec2d, SInt](_))
    testReadViewFromData(ReadDataView[Vec2d, SInt](_, _, _))
    testArrayFromCollection[Vec2d, SInt]((a: IndexedSeq[ReadVec2d]) => DataArray[Vec2d, SInt](a: _*))
    testArrayFromCollection[Vec2d, SInt]((a: IndexedSeq[ReadVec2d]) => DataArray[Vec2d, SInt](a))
    testBufferFromCollection[Vec2d, SInt]((a: IndexedSeq[ReadVec2d]) => DataBuffer[Vec2d, SInt](a: _*))
    testBufferFromCollection[Vec2d, SInt]((a: IndexedSeq[ReadVec2d]) => DataBuffer[Vec2d, SInt](a))

    testArrayFromSize(DataArray[Vec2d, UInt](_))
    testArrayFromData[Vec2d, UInt](DataArray[Vec2d, UInt](_))
    testBufferFromSize(DataBuffer[Vec2d, UInt](_))
    testBufferFromData(DataBuffer[Vec2d, UInt](_))
    testViewFromData(DataView[Vec2d, UInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec2d, UInt](_))
    testReadViewFromData(ReadDataView[Vec2d, UInt](_, _, _))
    testArrayFromCollection[Vec2d, UInt]((a: IndexedSeq[ReadVec2d]) => DataArray[Vec2d, UInt](a: _*))
    testArrayFromCollection[Vec2d, UInt]((a: IndexedSeq[ReadVec2d]) => DataArray[Vec2d, UInt](a))
    testBufferFromCollection[Vec2d, UInt]((a: IndexedSeq[ReadVec2d]) => DataBuffer[Vec2d, UInt](a: _*))
    testBufferFromCollection[Vec2d, UInt]((a: IndexedSeq[ReadVec2d]) => DataBuffer[Vec2d, UInt](a))
    
    testArrayFromSize(DataArray[Vec2d, HalfFloat](_))
    testArrayFromData[Vec2d, HalfFloat](DataArray[Vec2d, HalfFloat](_))
    testBufferFromSize(DataBuffer[Vec2d, HalfFloat](_))
    testBufferFromData(DataBuffer[Vec2d, HalfFloat](_))
    testViewFromData(DataView[Vec2d, HalfFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec2d, HalfFloat](_))
    testReadViewFromData(ReadDataView[Vec2d, HalfFloat](_, _, _))
    testArrayFromCollection[Vec2d, HalfFloat]((a: IndexedSeq[ReadVec2d]) => DataArray[Vec2d, HalfFloat](a: _*))
    testArrayFromCollection[Vec2d, HalfFloat]((a: IndexedSeq[ReadVec2d]) => DataArray[Vec2d, HalfFloat](a))
    testBufferFromCollection[Vec2d, HalfFloat]((a: IndexedSeq[ReadVec2d]) => DataBuffer[Vec2d, HalfFloat](a: _*))
    testBufferFromCollection[Vec2d, HalfFloat]((a: IndexedSeq[ReadVec2d]) => DataBuffer[Vec2d, HalfFloat](a))
    
    testArrayFromSize(DataArray[Vec2d, RawFloat](_))
    testArrayFromData[Vec2d, RawFloat](DataArray[Vec2d, RawFloat](_))
    testBufferFromSize(DataBuffer[Vec2d, RawFloat](_))
    testBufferFromData(DataBuffer[Vec2d, RawFloat](_))
    testViewFromData(DataView[Vec2d, RawFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec2d, RawFloat](_))
    testReadViewFromData(ReadDataView[Vec2d, RawFloat](_, _, _))
    testArrayFromCollection[Vec2d, RawFloat]((a: IndexedSeq[ReadVec2d]) => DataArray[Vec2d, RawFloat](a: _*))
    testArrayFromCollection[Vec2d, RawFloat]((a: IndexedSeq[ReadVec2d]) => DataArray[Vec2d, RawFloat](a))
    testBufferFromCollection[Vec2d, RawFloat]((a: IndexedSeq[ReadVec2d]) => DataBuffer[Vec2d, RawFloat](a: _*))
    testBufferFromCollection[Vec2d, RawFloat]((a: IndexedSeq[ReadVec2d]) => DataBuffer[Vec2d, RawFloat](a))
    
    testArrayFromSize(DataArray[Vec2d, RawDouble](_))
    testArrayFromData[Vec2d, RawDouble](DataArray[Vec2d, RawDouble](_))
    testBufferFromSize(DataBuffer[Vec2d, RawDouble](_))
    testBufferFromData(DataBuffer[Vec2d, RawDouble](_))
    testViewFromData(DataView[Vec2d, RawDouble](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec2d, RawDouble](_))
    testReadViewFromData(ReadDataView[Vec2d, RawDouble](_, _, _))
    testArrayFromCollection[Vec2d, RawDouble]((a: IndexedSeq[ReadVec2d]) => DataArray[Vec2d, RawDouble](a: _*))
    testArrayFromCollection[Vec2d, RawDouble]((a: IndexedSeq[ReadVec2d]) => DataArray[Vec2d, RawDouble](a))
    testBufferFromCollection[Vec2d, RawDouble]((a: IndexedSeq[ReadVec2d]) => DataBuffer[Vec2d, RawDouble](a: _*))
    testBufferFromCollection[Vec2d, RawDouble]((a: IndexedSeq[ReadVec2d]) => DataBuffer[Vec2d, RawDouble](a))
  }
}
