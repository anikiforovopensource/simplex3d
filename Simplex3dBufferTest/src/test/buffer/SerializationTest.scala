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
    val buffer = array.copyAsDataBuffer()

    val bytes = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bytes)
    out.writeObject(array)
    out.writeObject(buffer)
    out.close()

    val in = new ObjectInputStream(new ByteArrayInputStream(bytes.toByteArray()))
    val sarray = in.readObject().asInstanceOf[DataArray[E, R]]
    val sbuffer = in.readObject().asInstanceOf[DataBuffer[E, R]]
    in.close()
  }
}
