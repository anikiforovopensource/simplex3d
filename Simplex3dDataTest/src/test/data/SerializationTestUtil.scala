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

import java.io._
import org.scalatest._
import simplex3d.data._

import TestUtil._
import AttributeTestUtil._


/**
 * @author Aleksey Nikiforov (lex)
 */
object SerializationTestUtil extends FunSuite {

  def testSerialization[E <: Meta, R <: Raw](
    factory: (R#Array) => DataArray[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    val size = 10

    val array = factory(genRandomArray(size, descriptor))

    val bytes = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bytes)
    out.writeObject(array)
    out.writeObject(array.asReadOnly())
    out.close()

    val in = new ObjectInputStream(new ByteArrayInputStream(bytes.toByteArray()))

    val rwArray = in.readObject().asInstanceOf[DataArray[E, R]]
    testArray(rwArray, false, array.buffer())(descriptor)

    val roArray = in.readObject().asInstanceOf[ReadDataArray[E, R]]
    testArray(roArray, true, array.buffer())(descriptor)

    in.close()
  }
}
