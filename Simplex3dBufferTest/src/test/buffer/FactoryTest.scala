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

import simplex3d.math.intm._
import simplex3d.math.floatm._

import simplex3d.buffer._
import simplex3d.buffer.intm._
import simplex3d.buffer.floatm._


/**
 * @author Aleksey Nikiforov (lex)
 */
class FactoryTest extends FunSuite {
  test("Factory, from size") {
    def checkSeqSize(size: Int, seq: inDataSeq[_ <: ElemType, _ <: RawType]) {
      assert(size == seq.size)
      val buff = seq.asReadOnlyBuffer()
      assert(buff.limit == buff.capacity)
      assert((seq.offset + seq.size*seq.stride) == buff.capacity)
    }
    
    def testContiguous(size: Int) {
      checkSeqSize(size, IndexArray[UByte](size))
      checkSeqSize(size, IndexArray[UShort](size))
      checkSeqSize(size, IndexArray[UInt](size))

      checkSeqSize(size, DataArray[Int1, SByte](size))
      checkSeqSize(size, DataArray[Int1, UByte](size))
      checkSeqSize(size, DataArray[Int1, SShort](size))
      checkSeqSize(size, DataArray[Int1, UShort](size))
      checkSeqSize(size, DataArray[Int1, SInt](size))
      checkSeqSize(size, DataArray[Int1, UInt](size))

      checkSeqSize(size, DataArray[Vec2i, SByte](size))
      checkSeqSize(size, DataArray[Vec2i, UByte](size))
      checkSeqSize(size, DataArray[Vec2i, SShort](size))
      checkSeqSize(size, DataArray[Vec2i, UShort](size))
      checkSeqSize(size, DataArray[Vec2i, SInt](size))
      checkSeqSize(size, DataArray[Vec2i, UInt](size))

      checkSeqSize(size, DataArray[Vec3i, SByte](size))
      checkSeqSize(size, DataArray[Vec3i, UByte](size))
      checkSeqSize(size, DataArray[Vec3i, SShort](size))
      checkSeqSize(size, DataArray[Vec3i, UShort](size))
      checkSeqSize(size, DataArray[Vec3i, SInt](size))
      checkSeqSize(size, DataArray[Vec3i, UInt](size))

      checkSeqSize(size, DataArray[Vec4i, SByte](size))
      checkSeqSize(size, DataArray[Vec4i, UByte](size))
      checkSeqSize(size, DataArray[Vec4i, SShort](size))
      checkSeqSize(size, DataArray[Vec4i, UShort](size))
      checkSeqSize(size, DataArray[Vec4i, SInt](size))
      checkSeqSize(size, DataArray[Vec4i, UInt](size))

      checkSeqSize(size, DataArray[Float1, SByte](size))
      checkSeqSize(size, DataArray[Float1, UByte](size))
      checkSeqSize(size, DataArray[Float1, NSByte](size))
      checkSeqSize(size, DataArray[Float1, NUByte](size))
      checkSeqSize(size, DataArray[Float1, SShort](size))
      checkSeqSize(size, DataArray[Float1, UShort](size))
      checkSeqSize(size, DataArray[Float1, NSShort](size))
      checkSeqSize(size, DataArray[Float1, NUShort](size))
      checkSeqSize(size, DataArray[Float1, SInt](size))
      checkSeqSize(size, DataArray[Float1, UInt](size))
      checkSeqSize(size, DataArray[Float1, NSInt](size))
      checkSeqSize(size, DataArray[Float1, NUInt](size))
      checkSeqSize(size, DataArray[Float1, HalfFloat](size))
      checkSeqSize(size, DataArray[Float1, RawFloat](size))

      checkSeqSize(size, DataArray[Vec2f, SByte](size))
      checkSeqSize(size, DataArray[Vec2f, UByte](size))
      checkSeqSize(size, DataArray[Vec2f, NSByte](size))
      checkSeqSize(size, DataArray[Vec2f, NUByte](size))
      checkSeqSize(size, DataArray[Vec2f, SShort](size))
      checkSeqSize(size, DataArray[Vec2f, UShort](size))
      checkSeqSize(size, DataArray[Vec2f, NSShort](size))
      checkSeqSize(size, DataArray[Vec2f, NUShort](size))
      checkSeqSize(size, DataArray[Vec2f, SInt](size))
      checkSeqSize(size, DataArray[Vec2f, UInt](size))
      checkSeqSize(size, DataArray[Vec2f, NSInt](size))
      checkSeqSize(size, DataArray[Vec2f, NUInt](size))
      checkSeqSize(size, DataArray[Vec2f, HalfFloat](size))
      checkSeqSize(size, DataArray[Vec2f, RawFloat](size))

      checkSeqSize(size, DataArray[Vec3f, SByte](size))
      checkSeqSize(size, DataArray[Vec3f, UByte](size))
      checkSeqSize(size, DataArray[Vec3f, NSByte](size))
      checkSeqSize(size, DataArray[Vec3f, NUByte](size))
      checkSeqSize(size, DataArray[Vec3f, SShort](size))
      checkSeqSize(size, DataArray[Vec3f, UShort](size))
      checkSeqSize(size, DataArray[Vec3f, NSShort](size))
      checkSeqSize(size, DataArray[Vec3f, NUShort](size))
      checkSeqSize(size, DataArray[Vec3f, SInt](size))
      checkSeqSize(size, DataArray[Vec3f, UInt](size))
      checkSeqSize(size, DataArray[Vec3f, NSInt](size))
      checkSeqSize(size, DataArray[Vec3f, NUInt](size))
      checkSeqSize(size, DataArray[Vec3f, HalfFloat](size))
      checkSeqSize(size, DataArray[Vec3f, RawFloat](size))

      checkSeqSize(size, DataArray[Vec4f, SByte](size))
      checkSeqSize(size, DataArray[Vec4f, UByte](size))
      checkSeqSize(size, DataArray[Vec4f, NSByte](size))
      checkSeqSize(size, DataArray[Vec4f, NUByte](size))
      checkSeqSize(size, DataArray[Vec4f, SShort](size))
      checkSeqSize(size, DataArray[Vec4f, UShort](size))
      checkSeqSize(size, DataArray[Vec4f, NSShort](size))
      checkSeqSize(size, DataArray[Vec4f, NUShort](size))
      checkSeqSize(size, DataArray[Vec4f, SInt](size))
      checkSeqSize(size, DataArray[Vec4f, UInt](size))
      checkSeqSize(size, DataArray[Vec4f, NSInt](size))
      checkSeqSize(size, DataArray[Vec4f, NUInt](size))
      checkSeqSize(size, DataArray[Vec4f, HalfFloat](size))
      checkSeqSize(size, DataArray[Vec4f, RawFloat](size))
    }

    testContiguous(0)
    testContiguous(1)
    testContiguous(10)
    testContiguous(100)
  }
}
