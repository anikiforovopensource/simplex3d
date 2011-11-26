/*
 * Simplex3dData - Test Package
 * Copyright (C) 2010-2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dDataTest.
 *
 * Simplex3dDataTest is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dDataTest is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.test.data
package float

import org.scalatest._
import simplex3d.math.floatx._
import simplex3d.data._
import simplex3d.data.float._

import Descriptors._
import FactoryTestUtil._
import ApplyUpdateTestUtil._
import SubCopyTestUtil._


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
    testArrayFromCollection[Vec2f, SByte]((a: IndexedSeq[ReadVec2f]) => DataArray[Vec2f, SByte](a: _*))
    testBufferFromCollection[Vec2f, SByte]((a: IndexedSeq[ReadVec2f]) => DataBuffer[Vec2f, SByte](a: _*))

    testArrayFromSize(DataArray[Vec2f, UByte](_))
    testArrayFromData[Vec2f, UByte](DataArray[Vec2f, UByte](_))
    testBufferFromSize(DataBuffer[Vec2f, UByte](_))
    testBufferFromData(DataBuffer[Vec2f, UByte](_))
    testViewFromData(DataView[Vec2f, UByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec2f, UByte](_))
    testReadViewFromData(ReadDataView[Vec2f, UByte](_, _, _))
    testArrayFromCollection[Vec2f, UByte]((a: IndexedSeq[ReadVec2f]) => DataArray[Vec2f, UByte](a: _*))
    testBufferFromCollection[Vec2f, UByte]((a: IndexedSeq[ReadVec2f]) => DataBuffer[Vec2f, UByte](a: _*))

    testArrayFromSize(DataArray[Vec2f, SShort](_))
    testArrayFromData[Vec2f, SShort](DataArray[Vec2f, SShort](_))
    testBufferFromSize(DataBuffer[Vec2f, SShort](_))
    testBufferFromData(DataBuffer[Vec2f, SShort](_))
    testViewFromData(DataView[Vec2f, SShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec2f, SShort](_))
    testReadViewFromData(ReadDataView[Vec2f, SShort](_, _, _))
    testArrayFromCollection[Vec2f, SShort]((a: IndexedSeq[ReadVec2f]) => DataArray[Vec2f, SShort](a: _*))
    testBufferFromCollection[Vec2f, SShort]((a: IndexedSeq[ReadVec2f]) => DataBuffer[Vec2f, SShort](a: _*))

    testArrayFromSize(DataArray[Vec2f, UShort](_))
    testArrayFromData[Vec2f, UShort](DataArray[Vec2f, UShort](_))
    testBufferFromSize(DataBuffer[Vec2f, UShort](_))
    testBufferFromData(DataBuffer[Vec2f, UShort](_))
    testViewFromData(DataView[Vec2f, UShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec2f, UShort](_))
    testReadViewFromData(ReadDataView[Vec2f, UShort](_, _, _))
    testArrayFromCollection[Vec2f, UShort]((a: IndexedSeq[ReadVec2f]) => DataArray[Vec2f, UShort](a: _*))
    testBufferFromCollection[Vec2f, UShort]((a: IndexedSeq[ReadVec2f]) => DataBuffer[Vec2f, UShort](a: _*))

    testArrayFromSize(DataArray[Vec2f, SInt](_))
    testArrayFromData[Vec2f, SInt](DataArray[Vec2f, SInt](_))
    testBufferFromSize(DataBuffer[Vec2f, SInt](_))
    testBufferFromData(DataBuffer[Vec2f, SInt](_))
    testViewFromData(DataView[Vec2f, SInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec2f, SInt](_))
    testReadViewFromData(ReadDataView[Vec2f, SInt](_, _, _))
    testArrayFromCollection[Vec2f, SInt]((a: IndexedSeq[ReadVec2f]) => DataArray[Vec2f, SInt](a: _*))
    testBufferFromCollection[Vec2f, SInt]((a: IndexedSeq[ReadVec2f]) => DataBuffer[Vec2f, SInt](a: _*))

    testArrayFromSize(DataArray[Vec2f, UInt](_))
    testArrayFromData[Vec2f, UInt](DataArray[Vec2f, UInt](_))
    testBufferFromSize(DataBuffer[Vec2f, UInt](_))
    testBufferFromData(DataBuffer[Vec2f, UInt](_))
    testViewFromData(DataView[Vec2f, UInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec2f, UInt](_))
    testReadViewFromData(ReadDataView[Vec2f, UInt](_, _, _))
    testArrayFromCollection[Vec2f, UInt]((a: IndexedSeq[ReadVec2f]) => DataArray[Vec2f, UInt](a: _*))
    testBufferFromCollection[Vec2f, UInt]((a: IndexedSeq[ReadVec2f]) => DataBuffer[Vec2f, UInt](a: _*))
    
    testArrayFromSize(DataArray[Vec2f, HFloat](_))
    testArrayFromData[Vec2f, HFloat](DataArray[Vec2f, HFloat](_))
    testBufferFromSize(DataBuffer[Vec2f, HFloat](_))
    testBufferFromData(DataBuffer[Vec2f, HFloat](_))
    testViewFromData(DataView[Vec2f, HFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec2f, HFloat](_))
    testReadViewFromData(ReadDataView[Vec2f, HFloat](_, _, _))
    testArrayFromCollection[Vec2f, HFloat]((a: IndexedSeq[ReadVec2f]) => DataArray[Vec2f, HFloat](a: _*))
    testBufferFromCollection[Vec2f, HFloat]((a: IndexedSeq[ReadVec2f]) => DataBuffer[Vec2f, HFloat](a: _*))
    
    testArrayFromSize(DataArray[Vec2f, RFloat](_))
    testArrayFromData[Vec2f, RFloat](DataArray[Vec2f, RFloat](_))
    testBufferFromSize(DataBuffer[Vec2f, RFloat](_))
    testBufferFromData(DataBuffer[Vec2f, RFloat](_))
    testViewFromData(DataView[Vec2f, RFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec2f, RFloat](_))
    testReadViewFromData(ReadDataView[Vec2f, RFloat](_, _, _))
    testArrayFromCollection[Vec2f, RFloat]((a: IndexedSeq[ReadVec2f]) => DataArray[Vec2f, RFloat](a: _*))
    testBufferFromCollection[Vec2f, RFloat]((a: IndexedSeq[ReadVec2f]) => DataBuffer[Vec2f, RFloat](a: _*))
  }

  test("Apply/Update") {
    testApplyUpdateArray[Vec2f, SByte](DataArray[Vec2f, SByte](_))
    testApplyUpdateBuffer(DataBuffer[Vec2f, SByte](_))
    testApplyUpdateView(DataView[Vec2f, SByte](_, _, _))
    
    testApplyUpdateArray[Vec2f, UByte](DataArray[Vec2f, UByte](_))
    testApplyUpdateBuffer(DataBuffer[Vec2f, UByte](_))
    testApplyUpdateView(DataView[Vec2f, UByte](_, _, _))
    
    testApplyUpdateArray[Vec2f, SShort](DataArray[Vec2f, SShort](_))
    testApplyUpdateBuffer(DataBuffer[Vec2f, SShort](_))
    testApplyUpdateView(DataView[Vec2f, SShort](_, _, _))
    
    testApplyUpdateArray[Vec2f, UShort](DataArray[Vec2f, UShort](_))
    testApplyUpdateBuffer(DataBuffer[Vec2f, UShort](_))
    testApplyUpdateView(DataView[Vec2f, UShort](_, _, _))
    
    testApplyUpdateArray[Vec2f, SInt](DataArray[Vec2f, SInt](_))
    testApplyUpdateBuffer(DataBuffer[Vec2f, SInt](_))
    testApplyUpdateView(DataView[Vec2f, SInt](_, _, _))
    
    testApplyUpdateArray[Vec2f, UInt](DataArray[Vec2f, UInt](_))
    testApplyUpdateBuffer(DataBuffer[Vec2f, UInt](_))
    testApplyUpdateView(DataView[Vec2f, UInt](_, _, _))
    
    testApplyUpdateArray[Vec2f, HFloat](DataArray[Vec2f, HFloat](_))
    testApplyUpdateBuffer(DataBuffer[Vec2f, HFloat](_))
    testApplyUpdateView(DataView[Vec2f, HFloat](_, _, _))
    
    testApplyUpdateArray[Vec2f, RFloat](DataArray[Vec2f, RFloat](_))
    testApplyUpdateBuffer(DataBuffer[Vec2f, RFloat](_))
    testApplyUpdateView(DataView[Vec2f, RFloat](_, _, _))
  }
  
  test("Sub Copy") {
    testSubCopy(DataSeq[Vec2f, UByte])
    testSubCopy(DataSeq[Vec2f, SByte])
    testSubCopy(DataSeq[Vec2f, UShort])
    testSubCopy(DataSeq[Vec2f, SShort])
    testSubCopy(DataSeq[Vec2f, UInt])
    testSubCopy(DataSeq[Vec2f, SInt])
    testSubCopy(DataSeq[Vec2f, HFloat])
    testSubCopy(DataSeq[Vec2f, RFloat])
  }
}
