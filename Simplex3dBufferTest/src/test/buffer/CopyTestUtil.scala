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
import org.scalatest._
import simplex3d.buffer._

import TestUtil._
import AttributeTestUtil._


/**
 * @author Aleksey Nikiforov (lex)
 */
object CopyTestUtil extends FunSuite {

  def testCopy() {

  }
}
/*
// Test Copy
  copyAsDataArray()
  copyAsDataBuffer()
  copyAsDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int)

  put(index: Int, seq: Seq[E#Element], first: Int, count: Int)
  put(index: Int, seq: Seq[E#Element])
  put(seq: Seq[E#Element])

  put(index: Int, src: inContiguousSeq[E#Component, _], srcOffset: Int, srcStride: Int, count: Int)
  put(index: Int, src: inContiguousSeq[E#Component, _])
  put(src: inContiguousSeq[E#Component, _])

  put(index: Int, src: inDataSeq[E, _], first: Int, count: Int)
  put(index: Int, src: inDataSeq[E, _])
  put(src: inDataSeq[E, _])
*/
