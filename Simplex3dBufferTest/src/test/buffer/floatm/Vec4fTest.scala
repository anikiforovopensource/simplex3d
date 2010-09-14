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
class Vec4fTest extends FunSuite {

  test("Factories") {
    testArrayFromSize(DataArray[Vec4f, SByte](_))
    testArrayFromData[Vec4f, SByte](DataArray[Vec4f, SByte](_))
    testBufferFromSize(DataBuffer[Vec4f, SByte](_))
    testBufferFromData(DataBuffer[Vec4f, SByte](_))
    testViewFromData(DataView[Vec4f, SByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4f, SByte](_))
    testReadViewFromData(ReadDataView[Vec4f, SByte](_, _, _))
    testArrayFromCollection[Vec4f, SByte]((a: IndexedSeq[ReadVec4f]) => DataArray[Vec4f, SByte](a: _*))
    testArrayFromCollection[Vec4f, SByte]((a: IndexedSeq[ReadVec4f]) => DataArray[Vec4f, SByte](a))
    testBufferFromCollection[Vec4f, SByte]((a: IndexedSeq[ReadVec4f]) => DataBuffer[Vec4f, SByte](a: _*))
    testBufferFromCollection[Vec4f, SByte]((a: IndexedSeq[ReadVec4f]) => DataBuffer[Vec4f, SByte](a))

    testArrayFromSize(DataArray[Vec4f, UByte](_))
    testArrayFromData[Vec4f, UByte](DataArray[Vec4f, UByte](_))
    testBufferFromSize(DataBuffer[Vec4f, UByte](_))
    testBufferFromData(DataBuffer[Vec4f, UByte](_))
    testViewFromData(DataView[Vec4f, UByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4f, UByte](_))
    testReadViewFromData(ReadDataView[Vec4f, UByte](_, _, _))
    testArrayFromCollection[Vec4f, UByte]((a: IndexedSeq[ReadVec4f]) => DataArray[Vec4f, UByte](a: _*))
    testArrayFromCollection[Vec4f, UByte]((a: IndexedSeq[ReadVec4f]) => DataArray[Vec4f, UByte](a))
    testBufferFromCollection[Vec4f, UByte]((a: IndexedSeq[ReadVec4f]) => DataBuffer[Vec4f, UByte](a: _*))
    testBufferFromCollection[Vec4f, UByte]((a: IndexedSeq[ReadVec4f]) => DataBuffer[Vec4f, UByte](a))

    testArrayFromSize(DataArray[Vec4f, SShort](_))
    testArrayFromData[Vec4f, SShort](DataArray[Vec4f, SShort](_))
    testBufferFromSize(DataBuffer[Vec4f, SShort](_))
    testBufferFromData(DataBuffer[Vec4f, SShort](_))
    testViewFromData(DataView[Vec4f, SShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4f, SShort](_))
    testReadViewFromData(ReadDataView[Vec4f, SShort](_, _, _))
    testArrayFromCollection[Vec4f, SShort]((a: IndexedSeq[ReadVec4f]) => DataArray[Vec4f, SShort](a: _*))
    testArrayFromCollection[Vec4f, SShort]((a: IndexedSeq[ReadVec4f]) => DataArray[Vec4f, SShort](a))
    testBufferFromCollection[Vec4f, SShort]((a: IndexedSeq[ReadVec4f]) => DataBuffer[Vec4f, SShort](a: _*))
    testBufferFromCollection[Vec4f, SShort]((a: IndexedSeq[ReadVec4f]) => DataBuffer[Vec4f, SShort](a))

    testArrayFromSize(DataArray[Vec4f, UShort](_))
    testArrayFromData[Vec4f, UShort](DataArray[Vec4f, UShort](_))
    testBufferFromSize(DataBuffer[Vec4f, UShort](_))
    testBufferFromData(DataBuffer[Vec4f, UShort](_))
    testViewFromData(DataView[Vec4f, UShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4f, UShort](_))
    testReadViewFromData(ReadDataView[Vec4f, UShort](_, _, _))
    testArrayFromCollection[Vec4f, UShort]((a: IndexedSeq[ReadVec4f]) => DataArray[Vec4f, UShort](a: _*))
    testArrayFromCollection[Vec4f, UShort]((a: IndexedSeq[ReadVec4f]) => DataArray[Vec4f, UShort](a))
    testBufferFromCollection[Vec4f, UShort]((a: IndexedSeq[ReadVec4f]) => DataBuffer[Vec4f, UShort](a: _*))
    testBufferFromCollection[Vec4f, UShort]((a: IndexedSeq[ReadVec4f]) => DataBuffer[Vec4f, UShort](a))

    testArrayFromSize(DataArray[Vec4f, SInt](_))
    testArrayFromData[Vec4f, SInt](DataArray[Vec4f, SInt](_))
    testBufferFromSize(DataBuffer[Vec4f, SInt](_))
    testBufferFromData(DataBuffer[Vec4f, SInt](_))
    testViewFromData(DataView[Vec4f, SInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4f, SInt](_))
    testReadViewFromData(ReadDataView[Vec4f, SInt](_, _, _))
    testArrayFromCollection[Vec4f, SInt]((a: IndexedSeq[ReadVec4f]) => DataArray[Vec4f, SInt](a: _*))
    testArrayFromCollection[Vec4f, SInt]((a: IndexedSeq[ReadVec4f]) => DataArray[Vec4f, SInt](a))
    testBufferFromCollection[Vec4f, SInt]((a: IndexedSeq[ReadVec4f]) => DataBuffer[Vec4f, SInt](a: _*))
    testBufferFromCollection[Vec4f, SInt]((a: IndexedSeq[ReadVec4f]) => DataBuffer[Vec4f, SInt](a))

    testArrayFromSize(DataArray[Vec4f, UInt](_))
    testArrayFromData[Vec4f, UInt](DataArray[Vec4f, UInt](_))
    testBufferFromSize(DataBuffer[Vec4f, UInt](_))
    testBufferFromData(DataBuffer[Vec4f, UInt](_))
    testViewFromData(DataView[Vec4f, UInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4f, UInt](_))
    testReadViewFromData(ReadDataView[Vec4f, UInt](_, _, _))
    testArrayFromCollection[Vec4f, UInt]((a: IndexedSeq[ReadVec4f]) => DataArray[Vec4f, UInt](a: _*))
    testArrayFromCollection[Vec4f, UInt]((a: IndexedSeq[ReadVec4f]) => DataArray[Vec4f, UInt](a))
    testBufferFromCollection[Vec4f, UInt]((a: IndexedSeq[ReadVec4f]) => DataBuffer[Vec4f, UInt](a: _*))
    testBufferFromCollection[Vec4f, UInt]((a: IndexedSeq[ReadVec4f]) => DataBuffer[Vec4f, UInt](a))
    
    testArrayFromSize(DataArray[Vec4f, HalfFloat](_))
    testArrayFromData[Vec4f, HalfFloat](DataArray[Vec4f, HalfFloat](_))
    testBufferFromSize(DataBuffer[Vec4f, HalfFloat](_))
    testBufferFromData(DataBuffer[Vec4f, HalfFloat](_))
    testViewFromData(DataView[Vec4f, HalfFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4f, HalfFloat](_))
    testReadViewFromData(ReadDataView[Vec4f, HalfFloat](_, _, _))
    testArrayFromCollection[Vec4f, HalfFloat]((a: IndexedSeq[ReadVec4f]) => DataArray[Vec4f, HalfFloat](a: _*))
    testArrayFromCollection[Vec4f, HalfFloat]((a: IndexedSeq[ReadVec4f]) => DataArray[Vec4f, HalfFloat](a))
    testBufferFromCollection[Vec4f, HalfFloat]((a: IndexedSeq[ReadVec4f]) => DataBuffer[Vec4f, HalfFloat](a: _*))
    testBufferFromCollection[Vec4f, HalfFloat]((a: IndexedSeq[ReadVec4f]) => DataBuffer[Vec4f, HalfFloat](a))
    
    testArrayFromSize(DataArray[Vec4f, RawFloat](_))
    testArrayFromData[Vec4f, RawFloat](DataArray[Vec4f, RawFloat](_))
    testBufferFromSize(DataBuffer[Vec4f, RawFloat](_))
    testBufferFromData(DataBuffer[Vec4f, RawFloat](_))
    testViewFromData(DataView[Vec4f, RawFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4f, RawFloat](_))
    testReadViewFromData(ReadDataView[Vec4f, RawFloat](_, _, _))
    testArrayFromCollection[Vec4f, RawFloat]((a: IndexedSeq[ReadVec4f]) => DataArray[Vec4f, RawFloat](a: _*))
    testArrayFromCollection[Vec4f, RawFloat]((a: IndexedSeq[ReadVec4f]) => DataArray[Vec4f, RawFloat](a))
    testBufferFromCollection[Vec4f, RawFloat]((a: IndexedSeq[ReadVec4f]) => DataBuffer[Vec4f, RawFloat](a: _*))
    testBufferFromCollection[Vec4f, RawFloat]((a: IndexedSeq[ReadVec4f]) => DataBuffer[Vec4f, RawFloat](a))
  }
}
