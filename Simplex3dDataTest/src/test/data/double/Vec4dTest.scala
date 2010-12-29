/*
 * Simplex3d, DataTest package
 * Copyright (C) 2010, Simplex3d Team
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
package double

import org.scalatest._
import simplex3d.math.doublex._
import simplex3d.data._
import simplex3d.data.double._

import Descriptors._
import FactoryTestUtil._
import ApplyUpdateTestUtil._
import CopyTestUtil._


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
    testArrayFromCollection[Vec4d, SByte]((a: IndexedSeq[ReadVec4d]) => DataArray[Vec4d, SByte](a: _*))
    testBufferFromCollection[Vec4d, SByte]((a: IndexedSeq[ReadVec4d]) => DataBuffer[Vec4d, SByte](a: _*))

    testArrayFromSize(DataArray[Vec4d, UByte](_))
    testArrayFromData[Vec4d, UByte](DataArray[Vec4d, UByte](_))
    testBufferFromSize(DataBuffer[Vec4d, UByte](_))
    testBufferFromData(DataBuffer[Vec4d, UByte](_))
    testViewFromData(DataView[Vec4d, UByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4d, UByte](_))
    testReadViewFromData(ReadDataView[Vec4d, UByte](_, _, _))
    testArrayFromCollection[Vec4d, UByte]((a: IndexedSeq[ReadVec4d]) => DataArray[Vec4d, UByte](a: _*))
    testBufferFromCollection[Vec4d, UByte]((a: IndexedSeq[ReadVec4d]) => DataBuffer[Vec4d, UByte](a: _*))

    testArrayFromSize(DataArray[Vec4d, SShort](_))
    testArrayFromData[Vec4d, SShort](DataArray[Vec4d, SShort](_))
    testBufferFromSize(DataBuffer[Vec4d, SShort](_))
    testBufferFromData(DataBuffer[Vec4d, SShort](_))
    testViewFromData(DataView[Vec4d, SShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4d, SShort](_))
    testReadViewFromData(ReadDataView[Vec4d, SShort](_, _, _))
    testArrayFromCollection[Vec4d, SShort]((a: IndexedSeq[ReadVec4d]) => DataArray[Vec4d, SShort](a: _*))
    testBufferFromCollection[Vec4d, SShort]((a: IndexedSeq[ReadVec4d]) => DataBuffer[Vec4d, SShort](a: _*))

    testArrayFromSize(DataArray[Vec4d, UShort](_))
    testArrayFromData[Vec4d, UShort](DataArray[Vec4d, UShort](_))
    testBufferFromSize(DataBuffer[Vec4d, UShort](_))
    testBufferFromData(DataBuffer[Vec4d, UShort](_))
    testViewFromData(DataView[Vec4d, UShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4d, UShort](_))
    testReadViewFromData(ReadDataView[Vec4d, UShort](_, _, _))
    testArrayFromCollection[Vec4d, UShort]((a: IndexedSeq[ReadVec4d]) => DataArray[Vec4d, UShort](a: _*))
    testBufferFromCollection[Vec4d, UShort]((a: IndexedSeq[ReadVec4d]) => DataBuffer[Vec4d, UShort](a: _*))

    testArrayFromSize(DataArray[Vec4d, SInt](_))
    testArrayFromData[Vec4d, SInt](DataArray[Vec4d, SInt](_))
    testBufferFromSize(DataBuffer[Vec4d, SInt](_))
    testBufferFromData(DataBuffer[Vec4d, SInt](_))
    testViewFromData(DataView[Vec4d, SInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4d, SInt](_))
    testReadViewFromData(ReadDataView[Vec4d, SInt](_, _, _))
    testArrayFromCollection[Vec4d, SInt]((a: IndexedSeq[ReadVec4d]) => DataArray[Vec4d, SInt](a: _*))
    testBufferFromCollection[Vec4d, SInt]((a: IndexedSeq[ReadVec4d]) => DataBuffer[Vec4d, SInt](a: _*))

    testArrayFromSize(DataArray[Vec4d, UInt](_))
    testArrayFromData[Vec4d, UInt](DataArray[Vec4d, UInt](_))
    testBufferFromSize(DataBuffer[Vec4d, UInt](_))
    testBufferFromData(DataBuffer[Vec4d, UInt](_))
    testViewFromData(DataView[Vec4d, UInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4d, UInt](_))
    testReadViewFromData(ReadDataView[Vec4d, UInt](_, _, _))
    testArrayFromCollection[Vec4d, UInt]((a: IndexedSeq[ReadVec4d]) => DataArray[Vec4d, UInt](a: _*))
    testBufferFromCollection[Vec4d, UInt]((a: IndexedSeq[ReadVec4d]) => DataBuffer[Vec4d, UInt](a: _*))
    
    testArrayFromSize(DataArray[Vec4d, HFloat](_))
    testArrayFromData[Vec4d, HFloat](DataArray[Vec4d, HFloat](_))
    testBufferFromSize(DataBuffer[Vec4d, HFloat](_))
    testBufferFromData(DataBuffer[Vec4d, HFloat](_))
    testViewFromData(DataView[Vec4d, HFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4d, HFloat](_))
    testReadViewFromData(ReadDataView[Vec4d, HFloat](_, _, _))
    testArrayFromCollection[Vec4d, HFloat]((a: IndexedSeq[ReadVec4d]) => DataArray[Vec4d, HFloat](a: _*))
    testBufferFromCollection[Vec4d, HFloat]((a: IndexedSeq[ReadVec4d]) => DataBuffer[Vec4d, HFloat](a: _*))
    
    testArrayFromSize(DataArray[Vec4d, RFloat](_))
    testArrayFromData[Vec4d, RFloat](DataArray[Vec4d, RFloat](_))
    testBufferFromSize(DataBuffer[Vec4d, RFloat](_))
    testBufferFromData(DataBuffer[Vec4d, RFloat](_))
    testViewFromData(DataView[Vec4d, RFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4d, RFloat](_))
    testReadViewFromData(ReadDataView[Vec4d, RFloat](_, _, _))
    testArrayFromCollection[Vec4d, RFloat]((a: IndexedSeq[ReadVec4d]) => DataArray[Vec4d, RFloat](a: _*))
    testBufferFromCollection[Vec4d, RFloat]((a: IndexedSeq[ReadVec4d]) => DataBuffer[Vec4d, RFloat](a: _*))
    
    testArrayFromSize(DataArray[Vec4d, RDouble](_))
    testArrayFromData[Vec4d, RDouble](DataArray[Vec4d, RDouble](_))
    testBufferFromSize(DataBuffer[Vec4d, RDouble](_))
    testBufferFromData(DataBuffer[Vec4d, RDouble](_))
    testViewFromData(DataView[Vec4d, RDouble](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec4d, RDouble](_))
    testReadViewFromData(ReadDataView[Vec4d, RDouble](_, _, _))
    testArrayFromCollection[Vec4d, RDouble]((a: IndexedSeq[ReadVec4d]) => DataArray[Vec4d, RDouble](a: _*))
    testBufferFromCollection[Vec4d, RDouble]((a: IndexedSeq[ReadVec4d]) => DataBuffer[Vec4d, RDouble](a: _*))
  }
  
  test("Copy") {
    testCopy(DataSeq[Vec4d, UByte])
    testCopy(DataSeq[Vec4d, SByte])
    testCopy(DataSeq[Vec4d, UShort])
    testCopy(DataSeq[Vec4d, SShort])
    testCopy(DataSeq[Vec4d, UInt])
    testCopy(DataSeq[Vec4d, SInt])
    testCopy(DataSeq[Vec4d, HFloat])
    testCopy(DataSeq[Vec4d, RFloat])
    testCopy(DataSeq[Vec4d, RDouble])
  }
  
  test("Apply/Update") {
    testApplyUpdateArray[Vec4d, SByte](DataArray[Vec4d, SByte](_))
    testApplyUpdateBuffer(DataBuffer[Vec4d, SByte](_))
    testApplyUpdateView(DataView[Vec4d, SByte](_, _, _))
    
    testApplyUpdateArray[Vec4d, UByte](DataArray[Vec4d, UByte](_))
    testApplyUpdateBuffer(DataBuffer[Vec4d, UByte](_))
    testApplyUpdateView(DataView[Vec4d, UByte](_, _, _))
    
    testApplyUpdateArray[Vec4d, SShort](DataArray[Vec4d, SShort](_))
    testApplyUpdateBuffer(DataBuffer[Vec4d, SShort](_))
    testApplyUpdateView(DataView[Vec4d, SShort](_, _, _))
    
    testApplyUpdateArray[Vec4d, UShort](DataArray[Vec4d, UShort](_))
    testApplyUpdateBuffer(DataBuffer[Vec4d, UShort](_))
    testApplyUpdateView(DataView[Vec4d, UShort](_, _, _))
    
    testApplyUpdateArray[Vec4d, SInt](DataArray[Vec4d, SInt](_))
    testApplyUpdateBuffer(DataBuffer[Vec4d, SInt](_))
    testApplyUpdateView(DataView[Vec4d, SInt](_, _, _))
    
    testApplyUpdateArray[Vec4d, UInt](DataArray[Vec4d, UInt](_))
    testApplyUpdateBuffer(DataBuffer[Vec4d, UInt](_))
    testApplyUpdateView(DataView[Vec4d, UInt](_, _, _))
    
    testApplyUpdateArray[Vec4d, HFloat](DataArray[Vec4d, HFloat](_))
    testApplyUpdateBuffer(DataBuffer[Vec4d, HFloat](_))
    testApplyUpdateView(DataView[Vec4d, HFloat](_, _, _))
    
    testApplyUpdateArray[Vec4d, RFloat](DataArray[Vec4d, RFloat](_))
    testApplyUpdateBuffer(DataBuffer[Vec4d, RFloat](_))
    testApplyUpdateView(DataView[Vec4d, RFloat](_, _, _))
    
    testApplyUpdateArray[Vec4d, RDouble](DataArray[Vec4d, RDouble](_))
    testApplyUpdateBuffer(DataBuffer[Vec4d, RDouble](_))
    testApplyUpdateView(DataView[Vec4d, RDouble](_, _, _))
  }
}
