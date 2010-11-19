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

import java.io._
import java.nio._
import org.scalatest._
import simplex3d.math._
import simplex3d.math.floatm._
import simplex3d.math.doublem._
import simplex3d.buffer._
import simplex3d.buffer.floatm._
import simplex3d.buffer.doublem._

import TestUtil._
import AttributeTest._


/**
 * @author Aleksey Nikiforov (lex)
 */
object SerializationTest extends FunSuite {

  def testSerialization[E <: MetaElement, R <: RawData](
    factory: (R#ArrayType) => DataArray[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    val size = 10

    val array = factory(genRandomArray(size, descriptor))

    val bytes = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bytes)
    out.writeObject(array)
    out.writeObject(array.asReadOnlySeq())
    out.close()

    val in = new ObjectInputStream(new ByteArrayInputStream(bytes.toByteArray()))

    val rwArray = in.readObject().asInstanceOf[DataArray[E, R]]
    testArray(rwArray, false, array.buffer())(descriptor)

    val roArray = in.readObject().asInstanceOf[ReadDataArray[E, R]]
    testArray(roArray, true, array.buffer())(descriptor)

    in.close()
  }

  def testCase[E <: MetaElement, R <: RawData](
    factory: (R#ArrayType) => DataArray[E, R]
  )(implicit descriptor: Descriptor[E, R]) = {
    (factory, descriptor)
  }

  def testInterleavedSerialization[E <: MetaElement, R <: RawData](
    pairs: ((R#ArrayType) => DataArray[E, R], Descriptor[E, R])*
  )(implicit descriptor: Descriptor[E, R]) {
    val size = 10

    val arrays = (
      for ((factory, descritor) <- pairs) yield {
        factory(genRandomArray(size, descriptor))
      }
    )

    val rwdata = InterleavedData(interleaveAll(arrays: _*)(size))
    val rodata = InterleavedData(rwdata.map(_.asReadOnlySeq()))

    val bytes = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bytes)
    out.writeObject(rwdata)
    out.writeObject(rodata)
    out.close()

    val in = new ObjectInputStream(new ByteArrayInputStream(bytes.toByteArray()))

    val rwrestored = in.readObject().asInstanceOf[InterleavedData]
    //testInterleaved(rwrestored)

    val rorestored = in.readObject().asInstanceOf[InterleavedData]
    //testInterleaved(rorestored)

    in.close()
  }
}
