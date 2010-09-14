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
    testArrayFromCollection[Double1, SByte]((a: IndexedSeq[Double]) => DataArray[Double1, SByte](a: _*))
    testArrayFromCollection[Double1, SByte]((a: IndexedSeq[Double]) => DataArray[Double1, SByte](a))
    testBufferFromCollection[Double1, SByte]((a: IndexedSeq[Double]) => DataBuffer[Double1, SByte](a: _*))
    testBufferFromCollection[Double1, SByte]((a: IndexedSeq[Double]) => DataBuffer[Double1, SByte](a))

    testArrayFromSize(DataArray[Double1, UByte](_))
    testArrayFromData[Double1, UByte](DataArray[Double1, UByte](_))
    testBufferFromSize(DataBuffer[Double1, UByte](_))
    testBufferFromData(DataBuffer[Double1, UByte](_))
    testViewFromData(DataView[Double1, UByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Double1, UByte](_))
    testReadViewFromData(ReadDataView[Double1, UByte](_, _, _))
    testArrayFromCollection[Double1, UByte]((a: IndexedSeq[Double]) => DataArray[Double1, UByte](a: _*))
    testArrayFromCollection[Double1, UByte]((a: IndexedSeq[Double]) => DataArray[Double1, UByte](a))
    testBufferFromCollection[Double1, UByte]((a: IndexedSeq[Double]) => DataBuffer[Double1, UByte](a: _*))
    testBufferFromCollection[Double1, UByte]((a: IndexedSeq[Double]) => DataBuffer[Double1, UByte](a))

    testArrayFromSize(DataArray[Double1, SShort](_))
    testArrayFromData[Double1, SShort](DataArray[Double1, SShort](_))
    testBufferFromSize(DataBuffer[Double1, SShort](_))
    testBufferFromData(DataBuffer[Double1, SShort](_))
    testViewFromData(DataView[Double1, SShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Double1, SShort](_))
    testReadViewFromData(ReadDataView[Double1, SShort](_, _, _))
    testArrayFromCollection[Double1, SShort]((a: IndexedSeq[Double]) => DataArray[Double1, SShort](a: _*))
    testArrayFromCollection[Double1, SShort]((a: IndexedSeq[Double]) => DataArray[Double1, SShort](a))
    testBufferFromCollection[Double1, SShort]((a: IndexedSeq[Double]) => DataBuffer[Double1, SShort](a: _*))
    testBufferFromCollection[Double1, SShort]((a: IndexedSeq[Double]) => DataBuffer[Double1, SShort](a))

    testArrayFromSize(DataArray[Double1, UShort](_))
    testArrayFromData[Double1, UShort](DataArray[Double1, UShort](_))
    testBufferFromSize(DataBuffer[Double1, UShort](_))
    testBufferFromData(DataBuffer[Double1, UShort](_))
    testViewFromData(DataView[Double1, UShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Double1, UShort](_))
    testReadViewFromData(ReadDataView[Double1, UShort](_, _, _))
    testArrayFromCollection[Double1, UShort]((a: IndexedSeq[Double]) => DataArray[Double1, UShort](a: _*))
    testArrayFromCollection[Double1, UShort]((a: IndexedSeq[Double]) => DataArray[Double1, UShort](a))
    testBufferFromCollection[Double1, UShort]((a: IndexedSeq[Double]) => DataBuffer[Double1, UShort](a: _*))
    testBufferFromCollection[Double1, UShort]((a: IndexedSeq[Double]) => DataBuffer[Double1, UShort](a))

    testArrayFromSize(DataArray[Double1, SInt](_))
    testArrayFromData[Double1, SInt](DataArray[Double1, SInt](_))
    testBufferFromSize(DataBuffer[Double1, SInt](_))
    testBufferFromData(DataBuffer[Double1, SInt](_))
    testViewFromData(DataView[Double1, SInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Double1, SInt](_))
    testReadViewFromData(ReadDataView[Double1, SInt](_, _, _))
    testArrayFromCollection[Double1, SInt]((a: IndexedSeq[Double]) => DataArray[Double1, SInt](a: _*))
    testArrayFromCollection[Double1, SInt]((a: IndexedSeq[Double]) => DataArray[Double1, SInt](a))
    testBufferFromCollection[Double1, SInt]((a: IndexedSeq[Double]) => DataBuffer[Double1, SInt](a: _*))
    testBufferFromCollection[Double1, SInt]((a: IndexedSeq[Double]) => DataBuffer[Double1, SInt](a))

    testArrayFromSize(DataArray[Double1, UInt](_))
    testArrayFromData[Double1, UInt](DataArray[Double1, UInt](_))
    testBufferFromSize(DataBuffer[Double1, UInt](_))
    testBufferFromData(DataBuffer[Double1, UInt](_))
    testViewFromData(DataView[Double1, UInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Double1, UInt](_))
    testReadViewFromData(ReadDataView[Double1, UInt](_, _, _))
    testArrayFromCollection[Double1, UInt]((a: IndexedSeq[Double]) => DataArray[Double1, UInt](a: _*))
    testArrayFromCollection[Double1, UInt]((a: IndexedSeq[Double]) => DataArray[Double1, UInt](a))
    testBufferFromCollection[Double1, UInt]((a: IndexedSeq[Double]) => DataBuffer[Double1, UInt](a: _*))
    testBufferFromCollection[Double1, UInt]((a: IndexedSeq[Double]) => DataBuffer[Double1, UInt](a))
    
    testArrayFromSize(DataArray[Double1, HalfFloat](_))
    testArrayFromData[Double1, HalfFloat](DataArray[Double1, HalfFloat](_))
    testBufferFromSize(DataBuffer[Double1, HalfFloat](_))
    testBufferFromData(DataBuffer[Double1, HalfFloat](_))
    testViewFromData(DataView[Double1, HalfFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Double1, HalfFloat](_))
    testReadViewFromData(ReadDataView[Double1, HalfFloat](_, _, _))
    testArrayFromCollection[Double1, HalfFloat]((a: IndexedSeq[Double]) => DataArray[Double1, HalfFloat](a: _*))
    testArrayFromCollection[Double1, HalfFloat]((a: IndexedSeq[Double]) => DataArray[Double1, HalfFloat](a))
    testBufferFromCollection[Double1, HalfFloat]((a: IndexedSeq[Double]) => DataBuffer[Double1, HalfFloat](a: _*))
    testBufferFromCollection[Double1, HalfFloat]((a: IndexedSeq[Double]) => DataBuffer[Double1, HalfFloat](a))
    
    testArrayFromSize(DataArray[Double1, RawFloat](_))
    testArrayFromData[Double1, RawFloat](DataArray[Double1, RawFloat](_))
    testBufferFromSize(DataBuffer[Double1, RawFloat](_))
    testBufferFromData(DataBuffer[Double1, RawFloat](_))
    testViewFromData(DataView[Double1, RawFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Double1, RawFloat](_))
    testReadViewFromData(ReadDataView[Double1, RawFloat](_, _, _))
    testArrayFromCollection[Double1, RawFloat]((a: IndexedSeq[Double]) => DataArray[Double1, RawFloat](a: _*))
    testArrayFromCollection[Double1, RawFloat]((a: IndexedSeq[Double]) => DataArray[Double1, RawFloat](a))
    testBufferFromCollection[Double1, RawFloat]((a: IndexedSeq[Double]) => DataBuffer[Double1, RawFloat](a: _*))
    testBufferFromCollection[Double1, RawFloat]((a: IndexedSeq[Double]) => DataBuffer[Double1, RawFloat](a))
    
    testArrayFromSize(DataArray[Double1, RawDouble](_))
    testArrayFromData[Double1, RawDouble](DataArray[Double1, RawDouble](_))
    testBufferFromSize(DataBuffer[Double1, RawDouble](_))
    testBufferFromData(DataBuffer[Double1, RawDouble](_))
    testViewFromData(DataView[Double1, RawDouble](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Double1, RawDouble](_))
    testReadViewFromData(ReadDataView[Double1, RawDouble](_, _, _))
    testArrayFromCollection[Double1, RawDouble]((a: IndexedSeq[Double]) => DataArray[Double1, RawDouble](a: _*))
    testArrayFromCollection[Double1, RawDouble]((a: IndexedSeq[Double]) => DataArray[Double1, RawDouble](a))
    testBufferFromCollection[Double1, RawDouble]((a: IndexedSeq[Double]) => DataBuffer[Double1, RawDouble](a: _*))
    testBufferFromCollection[Double1, RawDouble]((a: IndexedSeq[Double]) => DataBuffer[Double1, RawDouble](a))
  }
}
