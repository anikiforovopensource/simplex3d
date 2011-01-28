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
    testBufferFromCollection[Vec2d, SByte]((a: IndexedSeq[ReadVec2d]) => DataBuffer[Vec2d, SByte](a: _*))

    testArrayFromSize(DataArray[Vec2d, UByte](_))
    testArrayFromData[Vec2d, UByte](DataArray[Vec2d, UByte](_))
    testBufferFromSize(DataBuffer[Vec2d, UByte](_))
    testBufferFromData(DataBuffer[Vec2d, UByte](_))
    testViewFromData(DataView[Vec2d, UByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec2d, UByte](_))
    testReadViewFromData(ReadDataView[Vec2d, UByte](_, _, _))
    testArrayFromCollection[Vec2d, UByte]((a: IndexedSeq[ReadVec2d]) => DataArray[Vec2d, UByte](a: _*))
    testBufferFromCollection[Vec2d, UByte]((a: IndexedSeq[ReadVec2d]) => DataBuffer[Vec2d, UByte](a: _*))

    testArrayFromSize(DataArray[Vec2d, SShort](_))
    testArrayFromData[Vec2d, SShort](DataArray[Vec2d, SShort](_))
    testBufferFromSize(DataBuffer[Vec2d, SShort](_))
    testBufferFromData(DataBuffer[Vec2d, SShort](_))
    testViewFromData(DataView[Vec2d, SShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec2d, SShort](_))
    testReadViewFromData(ReadDataView[Vec2d, SShort](_, _, _))
    testArrayFromCollection[Vec2d, SShort]((a: IndexedSeq[ReadVec2d]) => DataArray[Vec2d, SShort](a: _*))
    testBufferFromCollection[Vec2d, SShort]((a: IndexedSeq[ReadVec2d]) => DataBuffer[Vec2d, SShort](a: _*))

    testArrayFromSize(DataArray[Vec2d, UShort](_))
    testArrayFromData[Vec2d, UShort](DataArray[Vec2d, UShort](_))
    testBufferFromSize(DataBuffer[Vec2d, UShort](_))
    testBufferFromData(DataBuffer[Vec2d, UShort](_))
    testViewFromData(DataView[Vec2d, UShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec2d, UShort](_))
    testReadViewFromData(ReadDataView[Vec2d, UShort](_, _, _))
    testArrayFromCollection[Vec2d, UShort]((a: IndexedSeq[ReadVec2d]) => DataArray[Vec2d, UShort](a: _*))
    testBufferFromCollection[Vec2d, UShort]((a: IndexedSeq[ReadVec2d]) => DataBuffer[Vec2d, UShort](a: _*))

    testArrayFromSize(DataArray[Vec2d, SInt](_))
    testArrayFromData[Vec2d, SInt](DataArray[Vec2d, SInt](_))
    testBufferFromSize(DataBuffer[Vec2d, SInt](_))
    testBufferFromData(DataBuffer[Vec2d, SInt](_))
    testViewFromData(DataView[Vec2d, SInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec2d, SInt](_))
    testReadViewFromData(ReadDataView[Vec2d, SInt](_, _, _))
    testArrayFromCollection[Vec2d, SInt]((a: IndexedSeq[ReadVec2d]) => DataArray[Vec2d, SInt](a: _*))
    testBufferFromCollection[Vec2d, SInt]((a: IndexedSeq[ReadVec2d]) => DataBuffer[Vec2d, SInt](a: _*))

    testArrayFromSize(DataArray[Vec2d, UInt](_))
    testArrayFromData[Vec2d, UInt](DataArray[Vec2d, UInt](_))
    testBufferFromSize(DataBuffer[Vec2d, UInt](_))
    testBufferFromData(DataBuffer[Vec2d, UInt](_))
    testViewFromData(DataView[Vec2d, UInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec2d, UInt](_))
    testReadViewFromData(ReadDataView[Vec2d, UInt](_, _, _))
    testArrayFromCollection[Vec2d, UInt]((a: IndexedSeq[ReadVec2d]) => DataArray[Vec2d, UInt](a: _*))
    testBufferFromCollection[Vec2d, UInt]((a: IndexedSeq[ReadVec2d]) => DataBuffer[Vec2d, UInt](a: _*))
    
    testArrayFromSize(DataArray[Vec2d, HFloat](_))
    testArrayFromData[Vec2d, HFloat](DataArray[Vec2d, HFloat](_))
    testBufferFromSize(DataBuffer[Vec2d, HFloat](_))
    testBufferFromData(DataBuffer[Vec2d, HFloat](_))
    testViewFromData(DataView[Vec2d, HFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec2d, HFloat](_))
    testReadViewFromData(ReadDataView[Vec2d, HFloat](_, _, _))
    testArrayFromCollection[Vec2d, HFloat]((a: IndexedSeq[ReadVec2d]) => DataArray[Vec2d, HFloat](a: _*))
    testBufferFromCollection[Vec2d, HFloat]((a: IndexedSeq[ReadVec2d]) => DataBuffer[Vec2d, HFloat](a: _*))
    
    testArrayFromSize(DataArray[Vec2d, RFloat](_))
    testArrayFromData[Vec2d, RFloat](DataArray[Vec2d, RFloat](_))
    testBufferFromSize(DataBuffer[Vec2d, RFloat](_))
    testBufferFromData(DataBuffer[Vec2d, RFloat](_))
    testViewFromData(DataView[Vec2d, RFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec2d, RFloat](_))
    testReadViewFromData(ReadDataView[Vec2d, RFloat](_, _, _))
    testArrayFromCollection[Vec2d, RFloat]((a: IndexedSeq[ReadVec2d]) => DataArray[Vec2d, RFloat](a: _*))
    testBufferFromCollection[Vec2d, RFloat]((a: IndexedSeq[ReadVec2d]) => DataBuffer[Vec2d, RFloat](a: _*))
    
    testArrayFromSize(DataArray[Vec2d, RDouble](_))
    testArrayFromData[Vec2d, RDouble](DataArray[Vec2d, RDouble](_))
    testBufferFromSize(DataBuffer[Vec2d, RDouble](_))
    testBufferFromData(DataBuffer[Vec2d, RDouble](_))
    testViewFromData(DataView[Vec2d, RDouble](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec2d, RDouble](_))
    testReadViewFromData(ReadDataView[Vec2d, RDouble](_, _, _))
    testArrayFromCollection[Vec2d, RDouble]((a: IndexedSeq[ReadVec2d]) => DataArray[Vec2d, RDouble](a: _*))
    testBufferFromCollection[Vec2d, RDouble]((a: IndexedSeq[ReadVec2d]) => DataBuffer[Vec2d, RDouble](a: _*))
  }
  
  test("Copy") {
    testCopy(DataSeq[Vec2d, UByte])
    testCopy(DataSeq[Vec2d, SByte])
    testCopy(DataSeq[Vec2d, UShort])
    testCopy(DataSeq[Vec2d, SShort])
    testCopy(DataSeq[Vec2d, UInt])
    testCopy(DataSeq[Vec2d, SInt])
    testCopy(DataSeq[Vec2d, HFloat])
    testCopy(DataSeq[Vec2d, RFloat])
    testCopy(DataSeq[Vec2d, RDouble])
  }
  
  test("Apply/Update") {
    testApplyUpdateArray[Vec2d, SByte](DataArray[Vec2d, SByte](_))
    testApplyUpdateBuffer(DataBuffer[Vec2d, SByte](_))
    testApplyUpdateView(DataView[Vec2d, SByte](_, _, _))
    
    testApplyUpdateArray[Vec2d, UByte](DataArray[Vec2d, UByte](_))
    testApplyUpdateBuffer(DataBuffer[Vec2d, UByte](_))
    testApplyUpdateView(DataView[Vec2d, UByte](_, _, _))
    
    testApplyUpdateArray[Vec2d, SShort](DataArray[Vec2d, SShort](_))
    testApplyUpdateBuffer(DataBuffer[Vec2d, SShort](_))
    testApplyUpdateView(DataView[Vec2d, SShort](_, _, _))
    
    testApplyUpdateArray[Vec2d, UShort](DataArray[Vec2d, UShort](_))
    testApplyUpdateBuffer(DataBuffer[Vec2d, UShort](_))
    testApplyUpdateView(DataView[Vec2d, UShort](_, _, _))
    
    testApplyUpdateArray[Vec2d, SInt](DataArray[Vec2d, SInt](_))
    testApplyUpdateBuffer(DataBuffer[Vec2d, SInt](_))
    testApplyUpdateView(DataView[Vec2d, SInt](_, _, _))
    
    testApplyUpdateArray[Vec2d, UInt](DataArray[Vec2d, UInt](_))
    testApplyUpdateBuffer(DataBuffer[Vec2d, UInt](_))
    testApplyUpdateView(DataView[Vec2d, UInt](_, _, _))
    
    testApplyUpdateArray[Vec2d, HFloat](DataArray[Vec2d, HFloat](_))
    testApplyUpdateBuffer(DataBuffer[Vec2d, HFloat](_))
    testApplyUpdateView(DataView[Vec2d, HFloat](_, _, _))
    
    testApplyUpdateArray[Vec2d, RFloat](DataArray[Vec2d, RFloat](_))
    testApplyUpdateBuffer(DataBuffer[Vec2d, RFloat](_))
    testApplyUpdateView(DataView[Vec2d, RFloat](_, _, _))
    
    testApplyUpdateArray[Vec2d, RDouble](DataArray[Vec2d, RDouble](_))
    testApplyUpdateBuffer(DataBuffer[Vec2d, RDouble](_))
    testApplyUpdateView(DataView[Vec2d, RDouble](_, _, _))
  }
}
