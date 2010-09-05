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

import simplex3d.math._
import simplex3d.math.intm._
import simplex3d.math.floatm._
import simplex3d.buffer.{allocateDirectBuffer => alloc, _}
import simplex3d.buffer.intm._
import simplex3d.buffer.floatm._


/**
 * @author Aleksey Nikiforov (lex)
 */
class FactoryTest extends FunSuite {

  def testSizeArrayFactory[E <: MetaElement, R <: RawData](
    bySize: Int => DataArray[E, R]
  ) {

  }

  def testDataArrayFactory[E <: MetaElement, R <: RawData](
    fromArray: R#ArrayType => DataArray[E, R]
  ) {

  }

  def checkDataArray(
    size: Int, raw: Int, seq: DataArray[_ <: MetaElement, RawData]
  ) {
    assert(size == seq.size)
    assert(seq.components == seq.stride)
    assert(raw == seq.rawType)

    {
      val buff = seq.asReadOnlyBuffer()
      assert(buff.limit == buff.capacity)
      assert(size == (buff.capacity - seq.offset)/seq.stride)
    }

    {
      val buff = seq.asBuffer()
      assert(buff.limit == buff.capacity)
      assert(size == (buff.capacity - seq.offset)/seq.stride)
    }

    seq.array match { case a: Array[_] => assert(size == a.length/seq.stride) }
  }

  test("Factory, from size") {
    def checkContiguous(
      size: Int, seq: ContiguousSeq[_ <: MetaElement, _ <: RawData]
    ) {
      assert(size == seq.size)

      {
        val buff = seq.asReadOnlyBuffer()
        assert(buff.limit == buff.capacity)
        assert((seq.offset + seq.size*seq.stride) == buff.capacity)
        //XXX split array and buffer and add: assert(buff.isDirect)
      }

      {
        val buff = seq.asBuffer()
        assert(buff.limit == buff.capacity)
        assert((seq.offset + seq.size*seq.stride) == buff.capacity)
      }
    }

    def checkView(
      offset: Int, stride: Int, size: Int,
      seq: DataView[_ <: MetaElement, _ <: RawData]
    ) {
      def capacityLowBound(seq: DataView[_ <: MetaElement, _ <: RawData]) = {
        seq.offset + (seq.size - 1)*seq.stride + seq.components
      }

      //XXXassert(size == seq.size)
      assert(offset == seq.offset)
      assert(stride == seq.stride)

      {
        val buff = seq.asReadOnlyBuffer()
        assert(buff.limit == buff.capacity)
        //XXXassert(capacityLowBound(seq) <= buff.capacity)
        assert(buff.isDirect)
      }

      {
        val buff = seq.asBuffer()
        assert(buff.limit == buff.capacity)
        //XXXassert(capacityLowBound(seq) <= buff.capacity)
        assert(buff.isDirect)
      }
    }
    
    def testContiguous(size: Int) {
      checkContiguous(size, IndexArray[UByte](size))
      checkContiguous(size, IndexArray[UShort](size))
      checkContiguous(size, IndexArray[UInt](size))

      checkContiguous(size, DataArray[Int1, SByte](size))
      checkContiguous(size, DataArray[Int1, UByte](size))
      checkContiguous(size, DataArray[Int1, SShort](size))
      checkContiguous(size, DataArray[Int1, UShort](size))
      checkContiguous(size, DataArray[Int1, SInt](size))
      checkContiguous(size, DataArray[Int1, UInt](size))

      checkContiguous(size, DataArray[Vec2i, SByte](size))
      checkContiguous(size, DataArray[Vec2i, UByte](size))
      checkContiguous(size, DataArray[Vec2i, SShort](size))
      checkContiguous(size, DataArray[Vec2i, UShort](size))
      checkContiguous(size, DataArray[Vec2i, SInt](size))
      checkContiguous(size, DataArray[Vec2i, UInt](size))

      checkContiguous(size, DataArray[Vec3i, SByte](size))
      checkContiguous(size, DataArray[Vec3i, UByte](size))
      checkContiguous(size, DataArray[Vec3i, SShort](size))
      checkContiguous(size, DataArray[Vec3i, UShort](size))
      checkContiguous(size, DataArray[Vec3i, SInt](size))
      checkContiguous(size, DataArray[Vec3i, UInt](size))

      checkContiguous(size, DataArray[Vec4i, SByte](size))
      checkContiguous(size, DataArray[Vec4i, UByte](size))
      checkContiguous(size, DataArray[Vec4i, SShort](size))
      checkContiguous(size, DataArray[Vec4i, UShort](size))
      checkContiguous(size, DataArray[Vec4i, SInt](size))
      checkContiguous(size, DataArray[Vec4i, UInt](size))

      checkContiguous(size, DataArray[Float1, SByte](size))
      checkContiguous(size, DataArray[Float1, UByte](size))
      checkContiguous(size, DataArray[Float1, SShort](size))
      checkContiguous(size, DataArray[Float1, UShort](size))
      checkContiguous(size, DataArray[Float1, SInt](size))
      checkContiguous(size, DataArray[Float1, UInt](size))
      checkContiguous(size, DataArray[Float1, HalfFloat](size))
      checkContiguous(size, DataArray[Float1, RawFloat](size))

      checkContiguous(size, DataArray[Vec2f, SByte](size))
      checkContiguous(size, DataArray[Vec2f, UByte](size))
      checkContiguous(size, DataArray[Vec2f, SShort](size))
      checkContiguous(size, DataArray[Vec2f, UShort](size))
      checkContiguous(size, DataArray[Vec2f, SInt](size))
      checkContiguous(size, DataArray[Vec2f, UInt](size))
      checkContiguous(size, DataArray[Vec2f, HalfFloat](size))
      checkContiguous(size, DataArray[Vec2f, RawFloat](size))

      checkContiguous(size, DataArray[Vec3f, SByte](size))
      checkContiguous(size, DataArray[Vec3f, UByte](size))
      checkContiguous(size, DataArray[Vec3f, SShort](size))
      checkContiguous(size, DataArray[Vec3f, UShort](size))
      checkContiguous(size, DataArray[Vec3f, SInt](size))
      checkContiguous(size, DataArray[Vec3f, UInt](size))
      checkContiguous(size, DataArray[Vec3f, HalfFloat](size))
      checkContiguous(size, DataArray[Vec3f, RawFloat](size))

      checkContiguous(size, DataArray[Vec4f, SByte](size))
      checkContiguous(size, DataArray[Vec4f, UByte](size))
      checkContiguous(size, DataArray[Vec4f, SShort](size))
      checkContiguous(size, DataArray[Vec4f, UShort](size))
      checkContiguous(size, DataArray[Vec4f, SInt](size))
      checkContiguous(size, DataArray[Vec4f, UInt](size))
      checkContiguous(size, DataArray[Vec4f, HalfFloat](size))
      checkContiguous(size, DataArray[Vec4f, RawFloat](size))


      checkContiguous(size, IndexBuffer[UByte](size))
      checkContiguous(size, IndexBuffer[UShort](size))
      checkContiguous(size, IndexBuffer[UInt](size))

      checkContiguous(size, DataBuffer[Int1, SByte](size))
      checkContiguous(size, DataBuffer[Int1, UByte](size))
      checkContiguous(size, DataBuffer[Int1, SShort](size))
      checkContiguous(size, DataBuffer[Int1, UShort](size))
      checkContiguous(size, DataBuffer[Int1, SInt](size))
      checkContiguous(size, DataBuffer[Int1, UInt](size))

      checkContiguous(size, DataBuffer[Vec2i, SByte](size))
      checkContiguous(size, DataBuffer[Vec2i, UByte](size))
      checkContiguous(size, DataBuffer[Vec2i, SShort](size))
      checkContiguous(size, DataBuffer[Vec2i, UShort](size))
      checkContiguous(size, DataBuffer[Vec2i, SInt](size))
      checkContiguous(size, DataBuffer[Vec2i, UInt](size))

      checkContiguous(size, DataBuffer[Vec3i, SByte](size))
      checkContiguous(size, DataBuffer[Vec3i, UByte](size))
      checkContiguous(size, DataBuffer[Vec3i, SShort](size))
      checkContiguous(size, DataBuffer[Vec3i, UShort](size))
      checkContiguous(size, DataBuffer[Vec3i, SInt](size))
      checkContiguous(size, DataBuffer[Vec3i, UInt](size))

      checkContiguous(size, DataBuffer[Vec4i, SByte](size))
      checkContiguous(size, DataBuffer[Vec4i, UByte](size))
      checkContiguous(size, DataBuffer[Vec4i, SShort](size))
      checkContiguous(size, DataBuffer[Vec4i, UShort](size))
      checkContiguous(size, DataBuffer[Vec4i, SInt](size))
      checkContiguous(size, DataBuffer[Vec4i, UInt](size))

      checkContiguous(size, DataBuffer[Float1, SByte](size))
      checkContiguous(size, DataBuffer[Float1, UByte](size))
      checkContiguous(size, DataBuffer[Float1, SShort](size))
      checkContiguous(size, DataBuffer[Float1, UShort](size))
      checkContiguous(size, DataBuffer[Float1, SInt](size))
      checkContiguous(size, DataBuffer[Float1, UInt](size))
      checkContiguous(size, DataBuffer[Float1, HalfFloat](size))
      checkContiguous(size, DataBuffer[Float1, RawFloat](size))

      checkContiguous(size, DataBuffer[Vec2f, SByte](size))
      checkContiguous(size, DataBuffer[Vec2f, UByte](size))
      checkContiguous(size, DataBuffer[Vec2f, SShort](size))
      checkContiguous(size, DataBuffer[Vec2f, UShort](size))
      checkContiguous(size, DataBuffer[Vec2f, SInt](size))
      checkContiguous(size, DataBuffer[Vec2f, UInt](size))
      checkContiguous(size, DataBuffer[Vec2f, HalfFloat](size))
      checkContiguous(size, DataBuffer[Vec2f, RawFloat](size))

      checkContiguous(size, DataBuffer[Vec3f, SByte](size))
      checkContiguous(size, DataBuffer[Vec3f, UByte](size))
      checkContiguous(size, DataBuffer[Vec3f, SShort](size))
      checkContiguous(size, DataBuffer[Vec3f, UShort](size))
      checkContiguous(size, DataBuffer[Vec3f, SInt](size))
      checkContiguous(size, DataBuffer[Vec3f, UInt](size))
      checkContiguous(size, DataBuffer[Vec3f, HalfFloat](size))
      checkContiguous(size, DataBuffer[Vec3f, RawFloat](size))

      checkContiguous(size, DataBuffer[Vec4f, SByte](size))
      checkContiguous(size, DataBuffer[Vec4f, UByte](size))
      checkContiguous(size, DataBuffer[Vec4f, SShort](size))
      checkContiguous(size, DataBuffer[Vec4f, UShort](size))
      checkContiguous(size, DataBuffer[Vec4f, SInt](size))
      checkContiguous(size, DataBuffer[Vec4f, UInt](size))
      checkContiguous(size, DataBuffer[Vec4f, HalfFloat](size))
      checkContiguous(size, DataBuffer[Vec4f, RawFloat](size))
    }

    def testView(offset: Int = 0, stride: Int = 0, size: Int = 0) {
      val off = offset
      val st = stride
      val sz = size

      checkView(off, st, sz, DataView[Int1, SByte](alloc(sz), off, st))
      checkView(off, st, sz, DataView[Int1, UByte](alloc(sz), off, st))
      checkView(off, st, sz, DataView[Int1, SShort](alloc(sz*2), off, st))
      checkView(off, st, sz, DataView[Int1, UShort](alloc(sz*2), off, st))
      checkView(off, st, sz, DataView[Int1, SInt](alloc(sz*4), off, st))
      checkView(off, st, sz, DataView[Int1, UInt](alloc(sz*4), off, st))

      checkView(off, st, sz, DataView[Vec2i, SByte](alloc(sz), off, st))
      checkView(off, st, sz, DataView[Vec2i, UByte](alloc(sz), off, st))
      checkView(off, st, sz, DataView[Vec2i, SShort](alloc(sz*2), off, st))
      checkView(off, st, sz, DataView[Vec2i, UShort](alloc(sz*2), off, st))
      checkView(off, st, sz, DataView[Vec2i, SInt](alloc(sz*4), off, st))
      checkView(off, st, sz, DataView[Vec2i, UInt](alloc(sz*4), off, st))

      checkView(off, st, sz, DataView[Vec3i, SByte](alloc(sz), off, st))
      checkView(off, st, sz, DataView[Vec3i, UByte](alloc(sz), off, st))
      checkView(off, st, sz, DataView[Vec3i, SShort](alloc(sz*2), off, st))
      checkView(off, st, sz, DataView[Vec3i, UShort](alloc(sz*2), off, st))
      checkView(off, st, sz, DataView[Vec3i, SInt](alloc(sz*4), off, st))
      checkView(off, st, sz, DataView[Vec3i, UInt](alloc(sz*4), off, st))

      checkView(off, st, sz, DataView[Vec4i, SByte](alloc(sz), off, st))
      checkView(off, st, sz, DataView[Vec4i, UByte](alloc(sz), off, st))
      checkView(off, st, sz, DataView[Vec4i, SShort](alloc(sz*2), off, st))
      checkView(off, st, sz, DataView[Vec4i, UShort](alloc(sz*2), off, st))
      checkView(off, st, sz, DataView[Vec4i, SInt](alloc(sz*4), off, st))
      checkView(off, st, sz, DataView[Vec4i, UInt](alloc(sz*4), off, st))

      checkView(off, st, sz, DataView[Float1, SByte](alloc(sz), off, st))
      checkView(off, st, sz, DataView[Float1, UByte](alloc(sz), off, st))
      checkView(off, st, sz, DataView[Float1, SShort](alloc(sz*2), off, st))
      checkView(off, st, sz, DataView[Float1, UShort](alloc(sz*2), off, st))
      checkView(off, st, sz, DataView[Float1, SInt](alloc(sz*4), off, st))
      checkView(off, st, sz, DataView[Float1, UInt](alloc(sz*4), off, st))
      checkView(off, st, sz, DataView[Float1, HalfFloat](alloc(sz*2), off, st))
      checkView(off, st, sz, DataView[Float1, RawFloat](alloc(sz*4), off, st))

      checkView(off, st, sz, DataView[Vec2f, SByte](alloc(sz), off, st))
      checkView(off, st, sz, DataView[Vec2f, UByte](alloc(sz), off, st))
      checkView(off, st, sz, DataView[Vec2f, SShort](alloc(sz*2), off, st))
      checkView(off, st, sz, DataView[Vec2f, UShort](alloc(sz*2), off, st))
      checkView(off, st, sz, DataView[Vec2f, SInt](alloc(sz*4), off, st))
      checkView(off, st, sz, DataView[Vec2f, UInt](alloc(sz*4), off, st))
      checkView(off, st, sz, DataView[Vec2f, HalfFloat](alloc(sz*2), off, st))
      checkView(off, st, sz, DataView[Vec2f, RawFloat](alloc(sz*4), off, st))

      checkView(off, st, sz, DataView[Vec3f, SByte](alloc(sz), off, st))
      checkView(off, st, sz, DataView[Vec3f, UByte](alloc(sz), off, st))
      checkView(off, st, sz, DataView[Vec3f, SShort](alloc(sz*2), off, st))
      checkView(off, st, sz, DataView[Vec3f, UShort](alloc(sz*2), off, st))
      checkView(off, st, sz, DataView[Vec3f, SInt](alloc(sz*4), off, st))
      checkView(off, st, sz, DataView[Vec3f, UInt](alloc(sz*4), off, st))
      checkView(off, st, sz, DataView[Vec3f, HalfFloat](alloc(sz*2), off, st))
      checkView(off, st, sz, DataView[Vec3f, RawFloat](alloc(sz*4), off, st))

      checkView(off, st, sz, DataView[Vec4f, SByte](alloc(sz), off, st))
      checkView(off, st, sz, DataView[Vec4f, UByte](alloc(sz), off, st))
      checkView(off, st, sz, DataView[Vec4f, SShort](alloc(sz*2), off, st))
      checkView(off, st, sz, DataView[Vec4f, UShort](alloc(sz*2), off, st))
      checkView(off, st, sz, DataView[Vec4f, SInt](alloc(sz*4), off, st))
      checkView(off, st, sz, DataView[Vec4f, UInt](alloc(sz*4), off, st))
      checkView(off, st, sz, DataView[Vec4f, HalfFloat](alloc(sz*2), off, st))
      checkView(off, st, sz, DataView[Vec4f, RawFloat](alloc(sz*4), off, st))
    }

    //XXXtest illegal args on factories
    intercept[Exception] { DataArray[Float1, RawFloat](-1) }
    intercept[Exception] { DataBuffer[Float1, RawFloat](-1) }
    //XXXintercept[IllegalArgumentException] { DataView[Float1, RawFloat](ByteBuffer.wrap(new Array[Byte](0)), 0, 1) }
    intercept[IllegalArgumentException] { DataView[Float1, RawFloat](alloc(0), -1, 1) }
    intercept[IllegalArgumentException] { DataView[Float1, RawFloat](alloc(0), 1, 0) }
    intercept[IllegalArgumentException] { DataView[Float1, RawFloat](alloc(0), 0, -1) }
    intercept[IllegalArgumentException] { DataView[Float1, RawFloat](alloc(0), 0, 0) }

    
    testContiguous(0)
    testContiguous(1)
    testContiguous(10)
    testContiguous(100)

    testView(offset = 0, stride = 1, size = 0)
    testView(offset = 0, stride = 2, size = 0)
    testView(offset = 0, stride = 5, size = 0)
    testView(offset = 0, stride = 10, size = 0)

    testView(offset = 0, stride = 1, size = 1)
    testView(offset = 0, stride = 2, size = 1)
    testView(offset = 0, stride = 5, size = 1)
    testView(offset = 0, stride = 10, size = 1)

    testView(offset = 0, stride = 1, size = 10)
    testView(offset = 0, stride = 2, size = 10)
    testView(offset = 0, stride = 5, size = 10)
    testView(offset = 0, stride = 10, size = 10)

    testView(offset = 1, stride = 1, size = 10)
    testView(offset = 1, stride = 2, size = 10)
    testView(offset = 1, stride = 5, size = 10)
    testView(offset = 1, stride = 10, size = 10)

    testView(offset = 0, stride = 1, size = 100)
    testView(offset = 0, stride = 2, size = 100)
    testView(offset = 0, stride = 5, size = 100)
    testView(offset = 0, stride = 10, size = 100)

    testView(offset = 1, stride = 1, size = 100)
    testView(offset = 1, stride = 2, size = 100)
    testView(offset = 1, stride = 5, size = 100)
    testView(offset = 1, stride = 10, size = 100)

    testView(offset = 10, stride = 1, size = 100)
    testView(offset = 10, stride = 2, size = 100)
    testView(offset = 10, stride = 5, size = 100)
    testView(offset = 10, stride = 10, size = 100)
  }
}
