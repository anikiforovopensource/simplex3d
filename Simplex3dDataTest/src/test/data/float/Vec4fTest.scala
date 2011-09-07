/*
 * Simplex3d, DataTest package
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

package test.data
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
    testBufferFromCollection[Vec4f, SByte]((a: IndexedSeq[ReadVec4f]) => DataBuffer[Vec4f, SByte](a: _*))

    testArrayFromSize(DataArray[Vec4f, UByte](_))
    testArrayFromData[Vec4f, UByte](DataArray[Vec4f, UByte](_))
    testBufferFromSize(DataBuffer[Vec4f, UByte](_))
    testBufferFromData(DataBuffer[Vec4f, UByte](_))
    testViewFromData(DataView[Vec4f, UByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4f, UByte](_))
    testReadViewFromData(ReadDataView[Vec4f, UByte](_, _, _))
    testArrayFromCollection[Vec4f, UByte]((a: IndexedSeq[ReadVec4f]) => DataArray[Vec4f, UByte](a: _*))
    testBufferFromCollection[Vec4f, UByte]((a: IndexedSeq[ReadVec4f]) => DataBuffer[Vec4f, UByte](a: _*))

    testArrayFromSize(DataArray[Vec4f, SShort](_))
    testArrayFromData[Vec4f, SShort](DataArray[Vec4f, SShort](_))
    testBufferFromSize(DataBuffer[Vec4f, SShort](_))
    testBufferFromData(DataBuffer[Vec4f, SShort](_))
    testViewFromData(DataView[Vec4f, SShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4f, SShort](_))
    testReadViewFromData(ReadDataView[Vec4f, SShort](_, _, _))
    testArrayFromCollection[Vec4f, SShort]((a: IndexedSeq[ReadVec4f]) => DataArray[Vec4f, SShort](a: _*))
    testBufferFromCollection[Vec4f, SShort]((a: IndexedSeq[ReadVec4f]) => DataBuffer[Vec4f, SShort](a: _*))

    testArrayFromSize(DataArray[Vec4f, UShort](_))
    testArrayFromData[Vec4f, UShort](DataArray[Vec4f, UShort](_))
    testBufferFromSize(DataBuffer[Vec4f, UShort](_))
    testBufferFromData(DataBuffer[Vec4f, UShort](_))
    testViewFromData(DataView[Vec4f, UShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4f, UShort](_))
    testReadViewFromData(ReadDataView[Vec4f, UShort](_, _, _))
    testArrayFromCollection[Vec4f, UShort]((a: IndexedSeq[ReadVec4f]) => DataArray[Vec4f, UShort](a: _*))
    testBufferFromCollection[Vec4f, UShort]((a: IndexedSeq[ReadVec4f]) => DataBuffer[Vec4f, UShort](a: _*))

    testArrayFromSize(DataArray[Vec4f, SInt](_))
    testArrayFromData[Vec4f, SInt](DataArray[Vec4f, SInt](_))
    testBufferFromSize(DataBuffer[Vec4f, SInt](_))
    testBufferFromData(DataBuffer[Vec4f, SInt](_))
    testViewFromData(DataView[Vec4f, SInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4f, SInt](_))
    testReadViewFromData(ReadDataView[Vec4f, SInt](_, _, _))
    testArrayFromCollection[Vec4f, SInt]((a: IndexedSeq[ReadVec4f]) => DataArray[Vec4f, SInt](a: _*))
    testBufferFromCollection[Vec4f, SInt]((a: IndexedSeq[ReadVec4f]) => DataBuffer[Vec4f, SInt](a: _*))

    testArrayFromSize(DataArray[Vec4f, UInt](_))
    testArrayFromData[Vec4f, UInt](DataArray[Vec4f, UInt](_))
    testBufferFromSize(DataBuffer[Vec4f, UInt](_))
    testBufferFromData(DataBuffer[Vec4f, UInt](_))
    testViewFromData(DataView[Vec4f, UInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4f, UInt](_))
    testReadViewFromData(ReadDataView[Vec4f, UInt](_, _, _))
    testArrayFromCollection[Vec4f, UInt]((a: IndexedSeq[ReadVec4f]) => DataArray[Vec4f, UInt](a: _*))
    testBufferFromCollection[Vec4f, UInt]((a: IndexedSeq[ReadVec4f]) => DataBuffer[Vec4f, UInt](a: _*))
    
    testArrayFromSize(DataArray[Vec4f, HFloat](_))
    testArrayFromData[Vec4f, HFloat](DataArray[Vec4f, HFloat](_))
    testBufferFromSize(DataBuffer[Vec4f, HFloat](_))
    testBufferFromData(DataBuffer[Vec4f, HFloat](_))
    testViewFromData(DataView[Vec4f, HFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4f, HFloat](_))
    testReadViewFromData(ReadDataView[Vec4f, HFloat](_, _, _))
    testArrayFromCollection[Vec4f, HFloat]((a: IndexedSeq[ReadVec4f]) => DataArray[Vec4f, HFloat](a: _*))
    testBufferFromCollection[Vec4f, HFloat]((a: IndexedSeq[ReadVec4f]) => DataBuffer[Vec4f, HFloat](a: _*))
    
    testArrayFromSize(DataArray[Vec4f, RFloat](_))
    testArrayFromData[Vec4f, RFloat](DataArray[Vec4f, RFloat](_))
    testBufferFromSize(DataBuffer[Vec4f, RFloat](_))
    testBufferFromData(DataBuffer[Vec4f, RFloat](_))
    testViewFromData(DataView[Vec4f, RFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4f, RFloat](_))
    testReadViewFromData(ReadDataView[Vec4f, RFloat](_, _, _))
    testArrayFromCollection[Vec4f, RFloat]((a: IndexedSeq[ReadVec4f]) => DataArray[Vec4f, RFloat](a: _*))
    testBufferFromCollection[Vec4f, RFloat]((a: IndexedSeq[ReadVec4f]) => DataBuffer[Vec4f, RFloat](a: _*))
  }
  
  test("Apply/Update") {
    testApplyUpdateArray[Vec4f, SByte](DataArray[Vec4f, SByte](_))
    testApplyUpdateBuffer(DataBuffer[Vec4f, SByte](_))
    testApplyUpdateView(DataView[Vec4f, SByte](_, _, _))
    
    testApplyUpdateArray[Vec4f, UByte](DataArray[Vec4f, UByte](_))
    testApplyUpdateBuffer(DataBuffer[Vec4f, UByte](_))
    testApplyUpdateView(DataView[Vec4f, UByte](_, _, _))
    
    testApplyUpdateArray[Vec4f, SShort](DataArray[Vec4f, SShort](_))
    testApplyUpdateBuffer(DataBuffer[Vec4f, SShort](_))
    testApplyUpdateView(DataView[Vec4f, SShort](_, _, _))
    
    testApplyUpdateArray[Vec4f, UShort](DataArray[Vec4f, UShort](_))
    testApplyUpdateBuffer(DataBuffer[Vec4f, UShort](_))
    testApplyUpdateView(DataView[Vec4f, UShort](_, _, _))
    
    testApplyUpdateArray[Vec4f, SInt](DataArray[Vec4f, SInt](_))
    testApplyUpdateBuffer(DataBuffer[Vec4f, SInt](_))
    testApplyUpdateView(DataView[Vec4f, SInt](_, _, _))
    
    testApplyUpdateArray[Vec4f, UInt](DataArray[Vec4f, UInt](_))
    testApplyUpdateBuffer(DataBuffer[Vec4f, UInt](_))
    testApplyUpdateView(DataView[Vec4f, UInt](_, _, _))
    
    testApplyUpdateArray[Vec4f, HFloat](DataArray[Vec4f, HFloat](_))
    testApplyUpdateBuffer(DataBuffer[Vec4f, HFloat](_))
    testApplyUpdateView(DataView[Vec4f, HFloat](_, _, _))
    
    testApplyUpdateArray[Vec4f, RFloat](DataArray[Vec4f, RFloat](_))
    testApplyUpdateBuffer(DataBuffer[Vec4f, RFloat](_))
    testApplyUpdateView(DataView[Vec4f, RFloat](_, _, _))
  }
  
  test("Sub Copy") {
    testSubCopy(DataSeq[Vec4f, UByte])
    testSubCopy(DataSeq[Vec4f, SByte])
    testSubCopy(DataSeq[Vec4f, UShort])
    testSubCopy(DataSeq[Vec4f, SShort])
    testSubCopy(DataSeq[Vec4f, UInt])
    testSubCopy(DataSeq[Vec4f, SInt])
    testSubCopy(DataSeq[Vec4f, HFloat])
    testSubCopy(DataSeq[Vec4f, RFloat])
  }
}
