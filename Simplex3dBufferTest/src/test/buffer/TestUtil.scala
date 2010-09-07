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

import java.nio._
import simplex3d.buffer._


/**
 * @author Aleksey Nikiforov (lex)
 */
object TestUtil {

  private def rawLengh(rawType: Int) :Int = {
    import RawData._
    rawType match {
      case SByte => 1
      case UByte => 1
      case SShort => 2
      case UShort => 2
      case SInt => 4
      case UInt => 4
      case HalfFloat => 2
      case RawFloat => 4
      case RawDouble => 8
    }
  }

  def testSeq(
    seq: DataSeq[_ <: MetaElement, RawData],
    descriptor: Descriptor,
    data: Buffer
  ) {
    assert(seq.elemManifest == descriptor.elementManifest)
    assert(seq.componentManifest == descriptor.componentManifest)
    assert(seq.components == descriptor.components)
    assert(seq.rawType == descriptor.rawType)
    assert(seq.normalized == descriptor.normalized)

    assert(rawLength(seq.rawType) == seq.bytesPerRawComponent)
    assert(seq.bytesPerRawComponent*seq.asBuffer.limit == byteSize)
    assert(seq.bytesPerRawComponent*seq.offset == seq.byteOffet)
    assert(seq.bytesPerRawComponent*seq.stride == seq.byteStride)
    
    /*
    asReadOnlyBuffer
    sharesStoreObject

    bindingBuffer(offset: Int)

    size
    length == size

    backingSeq
    isReadOnly()
    asReadOnlySeq()
    asBuffer()
    */
  }
  
/*
// TestArray
  backingSeq
  asReadOnlySeq()
  array
  offset
  stride

// TestBuffer
  backingSeq
  asReadOnlySeq()
  offset
  stride

// TestView
  backingSeq
  asReadOnlySeq()
  offset
  stride

// Test applyUpdate
  apply(i: Int)
  update(i: Int, v: S)

// Test Factory
  test object factories;
  test Read object factories;

  mkDataArray(size: Int)
  mkDataArray(array: R#ArrayType)
  mkDataBuffer(size: Int)
  mkDataBuffer(byteBuffer: ByteBuffer)
  mkDataView(byteBuffer: ByteBuffer, offset: Int,stride: Int)
  mkReadDataBuffer(byteBuffer: ByteBuffer)
  mkReadDataView(byteBuffer: ByteBuffer, offset: Int,stride: Int)

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
}
