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
    testBufferFromCollection[Vec3d, SByte]((a: IndexedSeq[ReadVec3d]) => DataBuffer[Vec3d, SByte](a: _*))

    testArrayFromSize(DataArray[Vec3d, UByte](_))
    testArrayFromData[Vec3d, UByte](DataArray[Vec3d, UByte](_))
    testBufferFromSize(DataBuffer[Vec3d, UByte](_))
    testBufferFromData(DataBuffer[Vec3d, UByte](_))
    testViewFromData(DataView[Vec3d, UByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec3d, UByte](_))
    testReadViewFromData(ReadDataView[Vec3d, UByte](_, _, _))
    testArrayFromCollection[Vec3d, UByte]((a: IndexedSeq[ReadVec3d]) => DataArray[Vec3d, UByte](a: _*))
    testBufferFromCollection[Vec3d, UByte]((a: IndexedSeq[ReadVec3d]) => DataBuffer[Vec3d, UByte](a: _*))

    testArrayFromSize(DataArray[Vec3d, SShort](_))
    testArrayFromData[Vec3d, SShort](DataArray[Vec3d, SShort](_))
    testBufferFromSize(DataBuffer[Vec3d, SShort](_))
    testBufferFromData(DataBuffer[Vec3d, SShort](_))
    testViewFromData(DataView[Vec3d, SShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec3d, SShort](_))
    testReadViewFromData(ReadDataView[Vec3d, SShort](_, _, _))
    testArrayFromCollection[Vec3d, SShort]((a: IndexedSeq[ReadVec3d]) => DataArray[Vec3d, SShort](a: _*))
    testBufferFromCollection[Vec3d, SShort]((a: IndexedSeq[ReadVec3d]) => DataBuffer[Vec3d, SShort](a: _*))

    testArrayFromSize(DataArray[Vec3d, UShort](_))
    testArrayFromData[Vec3d, UShort](DataArray[Vec3d, UShort](_))
    testBufferFromSize(DataBuffer[Vec3d, UShort](_))
    testBufferFromData(DataBuffer[Vec3d, UShort](_))
    testViewFromData(DataView[Vec3d, UShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec3d, UShort](_))
    testReadViewFromData(ReadDataView[Vec3d, UShort](_, _, _))
    testArrayFromCollection[Vec3d, UShort]((a: IndexedSeq[ReadVec3d]) => DataArray[Vec3d, UShort](a: _*))
    testBufferFromCollection[Vec3d, UShort]((a: IndexedSeq[ReadVec3d]) => DataBuffer[Vec3d, UShort](a: _*))

    testArrayFromSize(DataArray[Vec3d, SInt](_))
    testArrayFromData[Vec3d, SInt](DataArray[Vec3d, SInt](_))
    testBufferFromSize(DataBuffer[Vec3d, SInt](_))
    testBufferFromData(DataBuffer[Vec3d, SInt](_))
    testViewFromData(DataView[Vec3d, SInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec3d, SInt](_))
    testReadViewFromData(ReadDataView[Vec3d, SInt](_, _, _))
    testArrayFromCollection[Vec3d, SInt]((a: IndexedSeq[ReadVec3d]) => DataArray[Vec3d, SInt](a: _*))
    testBufferFromCollection[Vec3d, SInt]((a: IndexedSeq[ReadVec3d]) => DataBuffer[Vec3d, SInt](a: _*))

    testArrayFromSize(DataArray[Vec3d, UInt](_))
    testArrayFromData[Vec3d, UInt](DataArray[Vec3d, UInt](_))
    testBufferFromSize(DataBuffer[Vec3d, UInt](_))
    testBufferFromData(DataBuffer[Vec3d, UInt](_))
    testViewFromData(DataView[Vec3d, UInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec3d, UInt](_))
    testReadViewFromData(ReadDataView[Vec3d, UInt](_, _, _))
    testArrayFromCollection[Vec3d, UInt]((a: IndexedSeq[ReadVec3d]) => DataArray[Vec3d, UInt](a: _*))
    testBufferFromCollection[Vec3d, UInt]((a: IndexedSeq[ReadVec3d]) => DataBuffer[Vec3d, UInt](a: _*))
    
    testArrayFromSize(DataArray[Vec3d, HFloat](_))
    testArrayFromData[Vec3d, HFloat](DataArray[Vec3d, HFloat](_))
    testBufferFromSize(DataBuffer[Vec3d, HFloat](_))
    testBufferFromData(DataBuffer[Vec3d, HFloat](_))
    testViewFromData(DataView[Vec3d, HFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec3d, HFloat](_))
    testReadViewFromData(ReadDataView[Vec3d, HFloat](_, _, _))
    testArrayFromCollection[Vec3d, HFloat]((a: IndexedSeq[ReadVec3d]) => DataArray[Vec3d, HFloat](a: _*))
    testBufferFromCollection[Vec3d, HFloat]((a: IndexedSeq[ReadVec3d]) => DataBuffer[Vec3d, HFloat](a: _*))
    
    testArrayFromSize(DataArray[Vec3d, RFloat](_))
    testArrayFromData[Vec3d, RFloat](DataArray[Vec3d, RFloat](_))
    testBufferFromSize(DataBuffer[Vec3d, RFloat](_))
    testBufferFromData(DataBuffer[Vec3d, RFloat](_))
    testViewFromData(DataView[Vec3d, RFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec3d, RFloat](_))
    testReadViewFromData(ReadDataView[Vec3d, RFloat](_, _, _))
    testArrayFromCollection[Vec3d, RFloat]((a: IndexedSeq[ReadVec3d]) => DataArray[Vec3d, RFloat](a: _*))
    testBufferFromCollection[Vec3d, RFloat]((a: IndexedSeq[ReadVec3d]) => DataBuffer[Vec3d, RFloat](a: _*))
    
    testArrayFromSize(DataArray[Vec3d, RDouble](_))
    testArrayFromData[Vec3d, RDouble](DataArray[Vec3d, RDouble](_))
    testBufferFromSize(DataBuffer[Vec3d, RDouble](_))
    testBufferFromData(DataBuffer[Vec3d, RDouble](_))
    testViewFromData(DataView[Vec3d, RDouble](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec3d, RDouble](_))
    testReadViewFromData(ReadDataView[Vec3d, RDouble](_, _, _))
    testArrayFromCollection[Vec3d, RDouble]((a: IndexedSeq[ReadVec3d]) => DataArray[Vec3d, RDouble](a: _*))
    testBufferFromCollection[Vec3d, RDouble]((a: IndexedSeq[ReadVec3d]) => DataBuffer[Vec3d, RDouble](a: _*))
  }
  
  test("Copy") {
    testCopy(DataSeq[Vec3d, UByte])
    testCopy(DataSeq[Vec3d, SByte])
    testCopy(DataSeq[Vec3d, UShort])
    testCopy(DataSeq[Vec3d, SShort])
    testCopy(DataSeq[Vec3d, UInt])
    testCopy(DataSeq[Vec3d, SInt])
    testCopy(DataSeq[Vec3d, HFloat])
    testCopy(DataSeq[Vec3d, RFloat])
    testCopy(DataSeq[Vec3d, RDouble])
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
    
    testApplyUpdateArray[Vec3d, HFloat](DataArray[Vec3d, HFloat](_))
    testApplyUpdateBuffer(DataBuffer[Vec3d, HFloat](_))
    testApplyUpdateView(DataView[Vec3d, HFloat](_, _, _))
    
    testApplyUpdateArray[Vec3d, RFloat](DataArray[Vec3d, RFloat](_))
    testApplyUpdateBuffer(DataBuffer[Vec3d, RFloat](_))
    testApplyUpdateView(DataView[Vec3d, RFloat](_, _, _))
    
    testApplyUpdateArray[Vec3d, RDouble](DataArray[Vec3d, RDouble](_))
    testApplyUpdateBuffer(DataBuffer[Vec3d, RDouble](_))
    testApplyUpdateView(DataView[Vec3d, RDouble](_, _, _))
  }
}
