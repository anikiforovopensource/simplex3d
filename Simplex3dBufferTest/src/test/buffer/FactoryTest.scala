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

import org.scalatest._

import simplex3d.buffer.{allocateDirectBuffer => alloc, _}
import simplex3d.buffer._
import TestUtil._
import AttributeTest._


/**
 * @author Aleksey Nikiforov (lex)
 */
object FactoryTest extends FunSuite {

  def testFactory1[E <: MetaElement, R <: RawData](
    factory: (Int) => DataArray[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    testArray(factory(0), false, wrapArray(genArray(0, descriptor)))(descriptor)
    testArray(factory(1), false, wrapArray(genArray(1, descriptor)))(descriptor)
    testArray(factory(50), false, wrapArray(genArray(50, descriptor)))(descriptor)
  }

  def testFactory2[E <: MetaElement, R <: RawData](
    factory: (R#ArrayType) => DataArray[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    val a0 = genArray[R#ArrayType](0, descriptor)
    val a1 = genArray[R#ArrayType](1, descriptor)
    val a50 = genArray[R#ArrayType](50, descriptor)

    testArray(factory(a0), false, wrapArray(a0))(descriptor)
    testArray(factory(a1), false, wrapArray(a1))(descriptor)
    testArray(factory(a50), false, wrapArray(a50))(descriptor)
  }
  
/*
// Test Factory
  test object factories; test Read object factories;

  mkDataArray(size: Int)
  mkDataArray(array: R#ArrayType)
  mkDataBuffer(size: Int)
  mkDataBuffer(byteBuffer: ByteBuffer)
  mkDataView(byteBuffer: ByteBuffer, offset: Int,stride: Int)
  mkReadDataBuffer(byteBuffer: ByteBuffer)
  mkReadDataView(byteBuffer: ByteBuffer, offset: Int,stride: Int)

  varargFactories
  test Sequence Cast
  sharesStoreObject
*/
}
