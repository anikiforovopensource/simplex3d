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

package test.data
package double

import org.scalatest._
import simplex3d.math.doublex._
import simplex3d.data._
import simplex3d.data.double._

import Descriptors._
import FactoryTestUtil._
import ApplyUpdateTestUtil._
import SubCopyTestUtil._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Mat2x3dTest extends FunSuite {

  test("Factories") {
    testArrayFromSize(DataArray[Mat2x3d, RFloat](_))
    testArrayFromData[Mat2x3d, RFloat](DataArray[Mat2x3d, RFloat](_))
    testBufferFromSize(DataBuffer[Mat2x3d, RFloat](_))
    testBufferFromData(DataBuffer[Mat2x3d, RFloat](_))
    testViewFromData(DataView[Mat2x3d, RFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Mat2x3d, RFloat](_))
    testReadViewFromData(ReadDataView[Mat2x3d, RFloat](_, _, _))
    testArrayFromCollection[Mat2x3d, RFloat]((a: IndexedSeq[ReadMat2x3d]) => DataArray[Mat2x3d, RFloat](a: _*))
    testBufferFromCollection[Mat2x3d, RFloat]((a: IndexedSeq[ReadMat2x3d]) => DataBuffer[Mat2x3d, RFloat](a: _*))

    testArrayFromSize(DataArray[Mat2x3d, RDouble](_))
    testArrayFromData[Mat2x3d, RDouble](DataArray[Mat2x3d, RDouble](_))
    testBufferFromSize(DataBuffer[Mat2x3d, RDouble](_))
    testBufferFromData(DataBuffer[Mat2x3d, RDouble](_))
    testViewFromData(DataView[Mat2x3d, RDouble](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Mat2x3d, RDouble](_))
    testReadViewFromData(ReadDataView[Mat2x3d, RDouble](_, _, _))
    testArrayFromCollection[Mat2x3d, RDouble]((a: IndexedSeq[ReadMat2x3d]) => DataArray[Mat2x3d, RDouble](a: _*))
    testBufferFromCollection[Mat2x3d, RDouble]((a: IndexedSeq[ReadMat2x3d]) => DataBuffer[Mat2x3d, RDouble](a: _*))
  }
  
  test("Apply/Update") {
    testApplyUpdateArray[Mat2x3d, RFloat](DataArray[Mat2x3d, RFloat](_))
    testApplyUpdateBuffer(DataBuffer[Mat2x3d, RFloat](_))
    testApplyUpdateView(DataView[Mat2x3d, RFloat](_, _, _))

    testApplyUpdateArray[Mat2x3d, RDouble](DataArray[Mat2x3d, RDouble](_))
    testApplyUpdateBuffer(DataBuffer[Mat2x3d, RDouble](_))
    testApplyUpdateView(DataView[Mat2x3d, RDouble](_, _, _))
  }
  
  test("Sub Copy") {
    testSubCopy(DataSeq[Mat2x3d, RFloat])
    testSubCopy(DataSeq[Mat2x3d, RDouble])
  }
}
