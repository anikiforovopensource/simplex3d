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

import org.scalatest._
import simplex3d.math._
import simplex3d.data._

import Descriptors._
import FactoryTestUtil._
import ApplyUpdateTestUtil._
import SubCopyTestUtil._


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
    testArrayFromCollection[Vec3i, SByte]((a: IndexedSeq[ReadVec3i]) => DataArray[Vec3i, SByte](a: _*))
    testBufferFromCollection[Vec3i, SByte]((a: IndexedSeq[ReadVec3i]) => DataBuffer[Vec3i, SByte](a: _*))

    testArrayFromSize(DataArray[Vec3i, UByte](_))
    testArrayFromData[Vec3i, UByte](DataArray[Vec3i, UByte](_))
    testBufferFromSize(DataBuffer[Vec3i, UByte](_))
    testBufferFromData(DataBuffer[Vec3i, UByte](_))
    testViewFromData(DataView[Vec3i, UByte](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec3i, UByte](_))
    testReadViewFromData(ReadDataView[Vec3i, UByte](_, _, _))
    testArrayFromCollection[Vec3i, UByte]((a: IndexedSeq[ReadVec3i]) => DataArray[Vec3i, UByte](a: _*))
    testBufferFromCollection[Vec3i, UByte]((a: IndexedSeq[ReadVec3i]) => DataBuffer[Vec3i, UByte](a: _*))

    testArrayFromSize(DataArray[Vec3i, SShort](_))
    testArrayFromData[Vec3i, SShort](DataArray[Vec3i, SShort](_))
    testBufferFromSize(DataBuffer[Vec3i, SShort](_))
    testBufferFromData(DataBuffer[Vec3i, SShort](_))
    testViewFromData(DataView[Vec3i, SShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec3i, SShort](_))
    testReadViewFromData(ReadDataView[Vec3i, SShort](_, _, _))
    testArrayFromCollection[Vec3i, SShort]((a: IndexedSeq[ReadVec3i]) => DataArray[Vec3i, SShort](a: _*))
    testBufferFromCollection[Vec3i, SShort]((a: IndexedSeq[ReadVec3i]) => DataBuffer[Vec3i, SShort](a: _*))

    testArrayFromSize(DataArray[Vec3i, UShort](_))
    testArrayFromData[Vec3i, UShort](DataArray[Vec3i, UShort](_))
    testBufferFromSize(DataBuffer[Vec3i, UShort](_))
    testBufferFromData(DataBuffer[Vec3i, UShort](_))
    testViewFromData(DataView[Vec3i, UShort](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec3i, UShort](_))
    testReadViewFromData(ReadDataView[Vec3i, UShort](_, _, _))
    testArrayFromCollection[Vec3i, UShort]((a: IndexedSeq[ReadVec3i]) => DataArray[Vec3i, UShort](a: _*))
    testBufferFromCollection[Vec3i, UShort]((a: IndexedSeq[ReadVec3i]) => DataBuffer[Vec3i, UShort](a: _*))

    testArrayFromSize(DataArray[Vec3i, SInt](_))
    testArrayFromData[Vec3i, SInt](DataArray[Vec3i, SInt](_))
    testBufferFromSize(DataBuffer[Vec3i, SInt](_))
    testBufferFromData(DataBuffer[Vec3i, SInt](_))
    testViewFromData(DataView[Vec3i, SInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec3i, SInt](_))
    testReadViewFromData(ReadDataView[Vec3i, SInt](_, _, _))
    testArrayFromCollection[Vec3i, SInt]((a: IndexedSeq[ReadVec3i]) => DataArray[Vec3i, SInt](a: _*))
    testBufferFromCollection[Vec3i, SInt]((a: IndexedSeq[ReadVec3i]) => DataBuffer[Vec3i, SInt](a: _*))

    testArrayFromSize(DataArray[Vec3i, UInt](_))
    testArrayFromData[Vec3i, UInt](DataArray[Vec3i, UInt](_))
    testBufferFromSize(DataBuffer[Vec3i, UInt](_))
    testBufferFromData(DataBuffer[Vec3i, UInt](_))
    testViewFromData(DataView[Vec3i, UInt](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Vec3i, UInt](_))
    testReadViewFromData(ReadDataView[Vec3i, UInt](_, _, _))
    testArrayFromCollection[Vec3i, UInt]((a: IndexedSeq[ReadVec3i]) => DataArray[Vec3i, UInt](a: _*))
    testBufferFromCollection[Vec3i, UInt]((a: IndexedSeq[ReadVec3i]) => DataBuffer[Vec3i, UInt](a: _*))
  }
  
  test("Apply/Update") {
    testApplyUpdateArray[Vec3i, SByte](DataArray[Vec3i, SByte](_))
    testApplyUpdateBuffer(DataBuffer[Vec3i, SByte](_))
    testApplyUpdateView(DataView[Vec3i, SByte](_, _, _))
    
    testApplyUpdateArray[Vec3i, UByte](DataArray[Vec3i, UByte](_))
    testApplyUpdateBuffer(DataBuffer[Vec3i, UByte](_))
    testApplyUpdateView(DataView[Vec3i, UByte](_, _, _))
    
    testApplyUpdateArray[Vec3i, SShort](DataArray[Vec3i, SShort](_))
    testApplyUpdateBuffer(DataBuffer[Vec3i, SShort](_))
    testApplyUpdateView(DataView[Vec3i, SShort](_, _, _))
    
    testApplyUpdateArray[Vec3i, UShort](DataArray[Vec3i, UShort](_))
    testApplyUpdateBuffer(DataBuffer[Vec3i, UShort](_))
    testApplyUpdateView(DataView[Vec3i, UShort](_, _, _))
    
    testApplyUpdateArray[Vec3i, SInt](DataArray[Vec3i, SInt](_))
    testApplyUpdateBuffer(DataBuffer[Vec3i, SInt](_))
    testApplyUpdateView(DataView[Vec3i, SInt](_, _, _))
    
    testApplyUpdateArray[Vec3i, UInt](DataArray[Vec3i, UInt](_))
    testApplyUpdateBuffer(DataBuffer[Vec3i, UInt](_))
    testApplyUpdateView(DataView[Vec3i, UInt](_, _, _))
  }
  
  test("Sub Copy") {
    testSubCopy(DataSeq[Vec3i, UByte])
    testSubCopy(DataSeq[Vec3i, SByte])
    testSubCopy(DataSeq[Vec3i, UShort])
    testSubCopy(DataSeq[Vec3i, SShort])
    testSubCopy(DataSeq[Vec3i, UInt])
    testSubCopy(DataSeq[Vec3i, SInt])
  }
}
