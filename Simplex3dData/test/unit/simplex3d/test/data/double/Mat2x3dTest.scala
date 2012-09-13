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
class Mat3x2dTest extends FunSuite {

  test("Factories") {
    testArrayFromSize(DataArray[Mat3x2d, RFloat](_))
    testArrayFromData[Mat3x2d, RFloat](DataArray[Mat3x2d, RFloat](_))
    testBufferFromSize(DataBuffer[Mat3x2d, RFloat](_))
    testBufferFromData(DataBuffer[Mat3x2d, RFloat](_))
    testViewFromData(DataView[Mat3x2d, RFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Mat3x2d, RFloat](_))
    testReadViewFromData(ReadDataView[Mat3x2d, RFloat](_, _, _))
    testArrayFromCollection[Mat3x2d, RFloat]((a: IndexedSeq[ReadMat3x2d]) => DataArray[Mat3x2d, RFloat](a: _*))
    testBufferFromCollection[Mat3x2d, RFloat]((a: IndexedSeq[ReadMat3x2d]) => DataBuffer[Mat3x2d, RFloat](a: _*))

    testArrayFromSize(DataArray[Mat3x2d, RDouble](_))
    testArrayFromData[Mat3x2d, RDouble](DataArray[Mat3x2d, RDouble](_))
    testBufferFromSize(DataBuffer[Mat3x2d, RDouble](_))
    testBufferFromData(DataBuffer[Mat3x2d, RDouble](_))
    testViewFromData(DataView[Mat3x2d, RDouble](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Mat3x2d, RDouble](_))
    testReadViewFromData(ReadDataView[Mat3x2d, RDouble](_, _, _))
    testArrayFromCollection[Mat3x2d, RDouble]((a: IndexedSeq[ReadMat3x2d]) => DataArray[Mat3x2d, RDouble](a: _*))
    testBufferFromCollection[Mat3x2d, RDouble]((a: IndexedSeq[ReadMat3x2d]) => DataBuffer[Mat3x2d, RDouble](a: _*))
  }
  
  test("Apply/Update") {
    testApplyUpdateArray[Mat3x2d, RFloat](DataArray[Mat3x2d, RFloat](_))
    testApplyUpdateBuffer(DataBuffer[Mat3x2d, RFloat](_))
    testApplyUpdateView(DataView[Mat3x2d, RFloat](_, _, _))

    testApplyUpdateArray[Mat3x2d, RDouble](DataArray[Mat3x2d, RDouble](_))
    testApplyUpdateBuffer(DataBuffer[Mat3x2d, RDouble](_))
    testApplyUpdateView(DataView[Mat3x2d, RDouble](_, _, _))
  }
  
  test("Sub Copy") {
    testSubCopy(DataFactory[Mat3x2d, RFloat])
    testSubCopy(DataFactory[Mat3x2d, RDouble])
  }
}
