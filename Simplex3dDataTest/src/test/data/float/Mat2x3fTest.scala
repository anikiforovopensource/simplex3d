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
import CopyTestUtil._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Mat2x3fTest extends FunSuite {

  test("Factories") {
    testArrayFromSize(DataArray[Mat2x3f, RFloat](_))
    testArrayFromData[Mat2x3f, RFloat](DataArray[Mat2x3f, RFloat](_))
    testBufferFromSize(DataBuffer[Mat2x3f, RFloat](_))
    testBufferFromData(DataBuffer[Mat2x3f, RFloat](_))
    testViewFromData(DataView[Mat2x3f, RFloat](_, _, _))
    testReadBufferFromData(ReadDataBuffer[Mat2x3f, RFloat](_))
    testReadViewFromData(ReadDataView[Mat2x3f, RFloat](_, _, _))
    testArrayFromCollection[Mat2x3f, RFloat]((a: IndexedSeq[ReadMat2x3f]) => DataArray[Mat2x3f, RFloat](a: _*))
    testBufferFromCollection[Mat2x3f, RFloat]((a: IndexedSeq[ReadMat2x3f]) => DataBuffer[Mat2x3f, RFloat](a: _*))
  }
  
  test("Copy") {
    testCopy(DataSeq[Mat2x3f, RFloat])
  }
  
  test("Apply/Update") {
    testApplyUpdateArray[Mat2x3f, RFloat](DataArray[Mat2x3f, RFloat](_))
    testApplyUpdateBuffer(DataBuffer[Mat2x3f, RFloat](_))
    testApplyUpdateView(DataView[Mat2x3f, RFloat](_, _, _))
  }
}
