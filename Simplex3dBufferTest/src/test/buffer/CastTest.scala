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
import scala.reflect._
import org.scalatest._
import simplex3d.math.intm._
import simplex3d.math.floatm._
import simplex3d.math.doublem._
import simplex3d.buffer._
import simplex3d.buffer.RawType._
import simplex3d.buffer.intm._
import simplex3d.buffer.floatm._
import simplex3d.buffer.doublem._

import TestUtil._
import AttributeTest._


/**
 * @author Aleksey Nikiforov (lex)
 */
object CastTest extends FunSuite {

  def testArrayCast[E <: MetaElement, R <: RawData](
    factory: (R#ArrayType) => DataArray[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    for (size <- 0 to 9) {
      val seq = factory(genRandomArray(size, descriptor))

      seq.rawType match {
        case SByte => testSByteArrayCast(seq.asInstanceOf[DataArray[_, SByte]])
        case UByte => testUByteArrayCast(seq.asInstanceOf[DataArray[_, UByte]])
        case SShort => testSShortArrayCast(seq.asInstanceOf[DataArray[_, SShort]])
        case UShort => testUShortArrayCast(seq.asInstanceOf[DataArray[_, UShort]])
        case SInt => testSIntArrayCast(seq.asInstanceOf[DataArray[_, SInt]])
        case UInt => testUIntArrayCast(seq.asInstanceOf[DataArray[_, UInt]])
        case HalfFloat => testHalfFloatArrayCast(seq.asInstanceOf[DataArray[_, HalfFloat]])
        case RawFloat => testRawFloatArrayCast(seq.asInstanceOf[DataArray[_, RawFloat]])
        case RawDouble => testRawDoubleArrayCast(seq.asInstanceOf[DataArray[_, RawDouble]])
      }
    }
  }

  private def testArrayCast[E <: MetaElement, R <: RawData](
    original: ReadDataArray[_, _],
    cast: ReadDataArray[E, R],
    readOnly: Boolean,
    data: Buffer
  )(implicit descriptor: Descriptor[E, R]) {
    testArray(cast, readOnly, data)(descriptor)
    assert(original.sharesStoreObject(cast))
    assert(cast.sharesStoreObject(original))
  }

  private def testSByteArrayCast(da: DataArray[_, SByte]) {
    val data = da.asBuffer()

    testArrayCast(da, DataArray[Int1, SByte](da), false, data)(Descriptors.Int1SByte)
    testArrayCast(da, DataArray[Vec2i, SByte](da), false, data)(Descriptors.Vec2iSByte)
    testArrayCast(da, DataArray[Vec3i, SByte](da), false, data)(Descriptors.Vec3iSByte)
    testArrayCast(da, DataArray[Vec4i, SByte](da), false, data)(Descriptors.Vec4iSByte)

    testArrayCast(da, DataArray[Float1, SByte](da), false, data)(Descriptors.Float1SByte)
    testArrayCast(da, DataArray[Vec2f, SByte](da), false, data)(Descriptors.Vec2fSByte)
    testArrayCast(da, DataArray[Vec3f, SByte](da), false, data)(Descriptors.Vec3fSByte)
    testArrayCast(da, DataArray[Vec4f, SByte](da), false, data)(Descriptors.Vec4fSByte)
    
    testArrayCast(da, DataArray[Double1, SByte](da), false, data)(Descriptors.Double1SByte)
    testArrayCast(da, DataArray[Vec2d, SByte](da), false, data)(Descriptors.Vec2dSByte)
    testArrayCast(da, DataArray[Vec3d, SByte](da), false, data)(Descriptors.Vec3dSByte)
    testArrayCast(da, DataArray[Vec4d, SByte](da), false, data)(Descriptors.Vec4dSByte)

    
    testArrayCast(da, ReadDataArray[Int1, SByte](da), false, data)(Descriptors.Int1SByte)
    testArrayCast(da, ReadDataArray[Vec2i, SByte](da), false, data)(Descriptors.Vec2iSByte)
    testArrayCast(da, ReadDataArray[Vec3i, SByte](da), false, data)(Descriptors.Vec3iSByte)
    testArrayCast(da, ReadDataArray[Vec4i, SByte](da), false, data)(Descriptors.Vec4iSByte)

    testArrayCast(da, ReadDataArray[Float1, SByte](da), false, data)(Descriptors.Float1SByte)
    testArrayCast(da, ReadDataArray[Vec2f, SByte](da), false, data)(Descriptors.Vec2fSByte)
    testArrayCast(da, ReadDataArray[Vec3f, SByte](da), false, data)(Descriptors.Vec3fSByte)
    testArrayCast(da, ReadDataArray[Vec4f, SByte](da), false, data)(Descriptors.Vec4fSByte)
    
    testArrayCast(da, ReadDataArray[Double1, SByte](da), false, data)(Descriptors.Double1SByte)
    testArrayCast(da, ReadDataArray[Vec2d, SByte](da), false, data)(Descriptors.Vec2dSByte)
    testArrayCast(da, ReadDataArray[Vec3d, SByte](da), false, data)(Descriptors.Vec3dSByte)
    testArrayCast(da, ReadDataArray[Vec4d, SByte](da), false, data)(Descriptors.Vec4dSByte)


    val ro = da.asReadOnlySeq()
    intercept[IllegalArgumentException] { DataArray[Int1, SByte](ro.asInstanceOf[DataArray[_, SByte]]) }
    
    testArrayCast(ro, ReadDataArray[Int1, SByte](ro), true, data)(Descriptors.Int1SByte)
    testArrayCast(ro, ReadDataArray[Vec2i, SByte](ro), true, data)(Descriptors.Vec2iSByte)
    testArrayCast(ro, ReadDataArray[Vec3i, SByte](ro), true, data)(Descriptors.Vec3iSByte)
    testArrayCast(ro, ReadDataArray[Vec4i, SByte](ro), true, data)(Descriptors.Vec4iSByte)

    testArrayCast(ro, ReadDataArray[Float1, SByte](ro), true, data)(Descriptors.Float1SByte)
    testArrayCast(ro, ReadDataArray[Vec2f, SByte](ro), true, data)(Descriptors.Vec2fSByte)
    testArrayCast(ro, ReadDataArray[Vec3f, SByte](ro), true, data)(Descriptors.Vec3fSByte)
    testArrayCast(ro, ReadDataArray[Vec4f, SByte](ro), true, data)(Descriptors.Vec4fSByte)
    
    testArrayCast(ro, ReadDataArray[Double1, SByte](ro), true, data)(Descriptors.Double1SByte)
    testArrayCast(ro, ReadDataArray[Vec2d, SByte](ro), true, data)(Descriptors.Vec2dSByte)
    testArrayCast(ro, ReadDataArray[Vec3d, SByte](ro), true, data)(Descriptors.Vec3dSByte)
    testArrayCast(ro, ReadDataArray[Vec4d, SByte](ro), true, data)(Descriptors.Vec4dSByte)
  }
  
  private def testUByteArrayCast(da: DataArray[_, UByte]) {
    val data = da.asBuffer()
    
    testArrayCast(da, DataArray[Int1, UByte](da), false, data)(Descriptors.Int1UByte)
    testArrayCast(da, DataArray[Vec2i, UByte](da), false, data)(Descriptors.Vec2iUByte)
    testArrayCast(da, DataArray[Vec3i, UByte](da), false, data)(Descriptors.Vec3iUByte)
    testArrayCast(da, DataArray[Vec4i, UByte](da), false, data)(Descriptors.Vec4iUByte)

    testArrayCast(da, IndexArray[UByte](da), false, data)(Descriptors.Int1UByte)

    testArrayCast(da, DataArray[Float1, UByte](da), false, data)(Descriptors.Float1UByte)
    testArrayCast(da, DataArray[Vec2f, UByte](da), false, data)(Descriptors.Vec2fUByte)
    testArrayCast(da, DataArray[Vec3f, UByte](da), false, data)(Descriptors.Vec3fUByte)
    testArrayCast(da, DataArray[Vec4f, UByte](da), false, data)(Descriptors.Vec4fUByte)
    
    testArrayCast(da, DataArray[Double1, UByte](da), false, data)(Descriptors.Double1UByte)
    testArrayCast(da, DataArray[Vec2d, UByte](da), false, data)(Descriptors.Vec2dUByte)
    testArrayCast(da, DataArray[Vec3d, UByte](da), false, data)(Descriptors.Vec3dUByte)
    testArrayCast(da, DataArray[Vec4d, UByte](da), false, data)(Descriptors.Vec4dUByte)

    
    testArrayCast(da, ReadDataArray[Int1, UByte](da), false, data)(Descriptors.Int1UByte)
    testArrayCast(da, ReadDataArray[Vec2i, UByte](da), false, data)(Descriptors.Vec2iUByte)
    testArrayCast(da, ReadDataArray[Vec3i, UByte](da), false, data)(Descriptors.Vec3iUByte)
    testArrayCast(da, ReadDataArray[Vec4i, UByte](da), false, data)(Descriptors.Vec4iUByte)

    testArrayCast(da, ReadIndexArray[UByte](da), false, data)(Descriptors.Int1UByte)

    testArrayCast(da, ReadDataArray[Float1, UByte](da), false, data)(Descriptors.Float1UByte)
    testArrayCast(da, ReadDataArray[Vec2f, UByte](da), false, data)(Descriptors.Vec2fUByte)
    testArrayCast(da, ReadDataArray[Vec3f, UByte](da), false, data)(Descriptors.Vec3fUByte)
    testArrayCast(da, ReadDataArray[Vec4f, UByte](da), false, data)(Descriptors.Vec4fUByte)
    
    testArrayCast(da, ReadDataArray[Double1, UByte](da), false, data)(Descriptors.Double1UByte)
    testArrayCast(da, ReadDataArray[Vec2d, UByte](da), false, data)(Descriptors.Vec2dUByte)
    testArrayCast(da, ReadDataArray[Vec3d, UByte](da), false, data)(Descriptors.Vec3dUByte)
    testArrayCast(da, ReadDataArray[Vec4d, UByte](da), false, data)(Descriptors.Vec4dUByte)


    val ro = da.asReadOnlySeq()
    intercept[IllegalArgumentException] { DataArray[Int1, UByte](ro.asInstanceOf[DataArray[_, UByte]]) }
    intercept[IllegalArgumentException] { IndexArray[UByte](ro.asInstanceOf[DataArray[_, UByte]]) }
    
    testArrayCast(ro, ReadDataArray[Int1, UByte](ro), true, data)(Descriptors.Int1UByte)
    testArrayCast(ro, ReadDataArray[Vec2i, UByte](ro), true, data)(Descriptors.Vec2iUByte)
    testArrayCast(ro, ReadDataArray[Vec3i, UByte](ro), true, data)(Descriptors.Vec3iUByte)
    testArrayCast(ro, ReadDataArray[Vec4i, UByte](ro), true, data)(Descriptors.Vec4iUByte)

    testArrayCast(ro, ReadIndexArray[UByte](ro), true, data)(Descriptors.Int1UByte)

    testArrayCast(ro, ReadDataArray[Float1, UByte](ro), true, data)(Descriptors.Float1UByte)
    testArrayCast(ro, ReadDataArray[Vec2f, UByte](ro), true, data)(Descriptors.Vec2fUByte)
    testArrayCast(ro, ReadDataArray[Vec3f, UByte](ro), true, data)(Descriptors.Vec3fUByte)
    testArrayCast(ro, ReadDataArray[Vec4f, UByte](ro), true, data)(Descriptors.Vec4fUByte)
    
    testArrayCast(ro, ReadDataArray[Double1, UByte](ro), true, data)(Descriptors.Double1UByte)
    testArrayCast(ro, ReadDataArray[Vec2d, UByte](ro), true, data)(Descriptors.Vec2dUByte)
    testArrayCast(ro, ReadDataArray[Vec3d, UByte](ro), true, data)(Descriptors.Vec3dUByte)
    testArrayCast(ro, ReadDataArray[Vec4d, UByte](ro), true, data)(Descriptors.Vec4dUByte)
  }

  private def testSShortArrayCast(da: DataArray[_, SShort]) {
    val data = da.asBuffer()

    testArrayCast(da, DataArray[Int1, SShort](da), false, data)(Descriptors.Int1SShort)
    testArrayCast(da, DataArray[Vec2i, SShort](da), false, data)(Descriptors.Vec2iSShort)
    testArrayCast(da, DataArray[Vec3i, SShort](da), false, data)(Descriptors.Vec3iSShort)
    testArrayCast(da, DataArray[Vec4i, SShort](da), false, data)(Descriptors.Vec4iSShort)

    testArrayCast(da, DataArray[Float1, SShort](da), false, data)(Descriptors.Float1SShort)
    testArrayCast(da, DataArray[Vec2f, SShort](da), false, data)(Descriptors.Vec2fSShort)
    testArrayCast(da, DataArray[Vec3f, SShort](da), false, data)(Descriptors.Vec3fSShort)
    testArrayCast(da, DataArray[Vec4f, SShort](da), false, data)(Descriptors.Vec4fSShort)
    
    testArrayCast(da, DataArray[Double1, SShort](da), false, data)(Descriptors.Double1SShort)
    testArrayCast(da, DataArray[Vec2d, SShort](da), false, data)(Descriptors.Vec2dSShort)
    testArrayCast(da, DataArray[Vec3d, SShort](da), false, data)(Descriptors.Vec3dSShort)
    testArrayCast(da, DataArray[Vec4d, SShort](da), false, data)(Descriptors.Vec4dSShort)

    
    testArrayCast(da, ReadDataArray[Int1, SShort](da), false, data)(Descriptors.Int1SShort)
    testArrayCast(da, ReadDataArray[Vec2i, SShort](da), false, data)(Descriptors.Vec2iSShort)
    testArrayCast(da, ReadDataArray[Vec3i, SShort](da), false, data)(Descriptors.Vec3iSShort)
    testArrayCast(da, ReadDataArray[Vec4i, SShort](da), false, data)(Descriptors.Vec4iSShort)

    testArrayCast(da, ReadDataArray[Float1, SShort](da), false, data)(Descriptors.Float1SShort)
    testArrayCast(da, ReadDataArray[Vec2f, SShort](da), false, data)(Descriptors.Vec2fSShort)
    testArrayCast(da, ReadDataArray[Vec3f, SShort](da), false, data)(Descriptors.Vec3fSShort)
    testArrayCast(da, ReadDataArray[Vec4f, SShort](da), false, data)(Descriptors.Vec4fSShort)
    
    testArrayCast(da, ReadDataArray[Double1, SShort](da), false, data)(Descriptors.Double1SShort)
    testArrayCast(da, ReadDataArray[Vec2d, SShort](da), false, data)(Descriptors.Vec2dSShort)
    testArrayCast(da, ReadDataArray[Vec3d, SShort](da), false, data)(Descriptors.Vec3dSShort)
    testArrayCast(da, ReadDataArray[Vec4d, SShort](da), false, data)(Descriptors.Vec4dSShort)


    val ro = da.asReadOnlySeq()
    intercept[IllegalArgumentException] { DataArray[Int1, SShort](ro.asInstanceOf[DataArray[_, SShort]]) }
    
    testArrayCast(ro, ReadDataArray[Int1, SShort](ro), true, data)(Descriptors.Int1SShort)
    testArrayCast(ro, ReadDataArray[Vec2i, SShort](ro), true, data)(Descriptors.Vec2iSShort)
    testArrayCast(ro, ReadDataArray[Vec3i, SShort](ro), true, data)(Descriptors.Vec3iSShort)
    testArrayCast(ro, ReadDataArray[Vec4i, SShort](ro), true, data)(Descriptors.Vec4iSShort)

    testArrayCast(ro, ReadDataArray[Float1, SShort](ro), true, data)(Descriptors.Float1SShort)
    testArrayCast(ro, ReadDataArray[Vec2f, SShort](ro), true, data)(Descriptors.Vec2fSShort)
    testArrayCast(ro, ReadDataArray[Vec3f, SShort](ro), true, data)(Descriptors.Vec3fSShort)
    testArrayCast(ro, ReadDataArray[Vec4f, SShort](ro), true, data)(Descriptors.Vec4fSShort)
    
    testArrayCast(ro, ReadDataArray[Double1, SShort](ro), true, data)(Descriptors.Double1SShort)
    testArrayCast(ro, ReadDataArray[Vec2d, SShort](ro), true, data)(Descriptors.Vec2dSShort)
    testArrayCast(ro, ReadDataArray[Vec3d, SShort](ro), true, data)(Descriptors.Vec3dSShort)
    testArrayCast(ro, ReadDataArray[Vec4d, SShort](ro), true, data)(Descriptors.Vec4dSShort)
  }
  
  private def testUShortArrayCast(da: DataArray[_, UShort]) {
    val data = da.asBuffer()
    
    testArrayCast(da, DataArray[Int1, UShort](da), false, data)(Descriptors.Int1UShort)
    testArrayCast(da, DataArray[Vec2i, UShort](da), false, data)(Descriptors.Vec2iUShort)
    testArrayCast(da, DataArray[Vec3i, UShort](da), false, data)(Descriptors.Vec3iUShort)
    testArrayCast(da, DataArray[Vec4i, UShort](da), false, data)(Descriptors.Vec4iUShort)

    testArrayCast(da, IndexArray[UShort](da), false, data)(Descriptors.Int1UShort)

    testArrayCast(da, DataArray[Float1, UShort](da), false, data)(Descriptors.Float1UShort)
    testArrayCast(da, DataArray[Vec2f, UShort](da), false, data)(Descriptors.Vec2fUShort)
    testArrayCast(da, DataArray[Vec3f, UShort](da), false, data)(Descriptors.Vec3fUShort)
    testArrayCast(da, DataArray[Vec4f, UShort](da), false, data)(Descriptors.Vec4fUShort)
    
    testArrayCast(da, DataArray[Double1, UShort](da), false, data)(Descriptors.Double1UShort)
    testArrayCast(da, DataArray[Vec2d, UShort](da), false, data)(Descriptors.Vec2dUShort)
    testArrayCast(da, DataArray[Vec3d, UShort](da), false, data)(Descriptors.Vec3dUShort)
    testArrayCast(da, DataArray[Vec4d, UShort](da), false, data)(Descriptors.Vec4dUShort)

    
    testArrayCast(da, ReadDataArray[Int1, UShort](da), false, data)(Descriptors.Int1UShort)
    testArrayCast(da, ReadDataArray[Vec2i, UShort](da), false, data)(Descriptors.Vec2iUShort)
    testArrayCast(da, ReadDataArray[Vec3i, UShort](da), false, data)(Descriptors.Vec3iUShort)
    testArrayCast(da, ReadDataArray[Vec4i, UShort](da), false, data)(Descriptors.Vec4iUShort)

    testArrayCast(da, ReadIndexArray[UShort](da), false, data)(Descriptors.Int1UShort)

    testArrayCast(da, ReadDataArray[Float1, UShort](da), false, data)(Descriptors.Float1UShort)
    testArrayCast(da, ReadDataArray[Vec2f, UShort](da), false, data)(Descriptors.Vec2fUShort)
    testArrayCast(da, ReadDataArray[Vec3f, UShort](da), false, data)(Descriptors.Vec3fUShort)
    testArrayCast(da, ReadDataArray[Vec4f, UShort](da), false, data)(Descriptors.Vec4fUShort)
    
    testArrayCast(da, ReadDataArray[Double1, UShort](da), false, data)(Descriptors.Double1UShort)
    testArrayCast(da, ReadDataArray[Vec2d, UShort](da), false, data)(Descriptors.Vec2dUShort)
    testArrayCast(da, ReadDataArray[Vec3d, UShort](da), false, data)(Descriptors.Vec3dUShort)
    testArrayCast(da, ReadDataArray[Vec4d, UShort](da), false, data)(Descriptors.Vec4dUShort)


    val ro = da.asReadOnlySeq()
    intercept[IllegalArgumentException] { DataArray[Int1, UShort](ro.asInstanceOf[DataArray[_, UShort]]) }
    intercept[IllegalArgumentException] { IndexArray[UShort](ro.asInstanceOf[DataArray[_, UShort]]) }
    
    testArrayCast(ro, ReadDataArray[Int1, UShort](ro), true, data)(Descriptors.Int1UShort)
    testArrayCast(ro, ReadDataArray[Vec2i, UShort](ro), true, data)(Descriptors.Vec2iUShort)
    testArrayCast(ro, ReadDataArray[Vec3i, UShort](ro), true, data)(Descriptors.Vec3iUShort)
    testArrayCast(ro, ReadDataArray[Vec4i, UShort](ro), true, data)(Descriptors.Vec4iUShort)

    testArrayCast(ro, ReadIndexArray[UShort](ro), true, data)(Descriptors.Int1UShort)

    testArrayCast(ro, ReadDataArray[Float1, UShort](ro), true, data)(Descriptors.Float1UShort)
    testArrayCast(ro, ReadDataArray[Vec2f, UShort](ro), true, data)(Descriptors.Vec2fUShort)
    testArrayCast(ro, ReadDataArray[Vec3f, UShort](ro), true, data)(Descriptors.Vec3fUShort)
    testArrayCast(ro, ReadDataArray[Vec4f, UShort](ro), true, data)(Descriptors.Vec4fUShort)
    
    testArrayCast(ro, ReadDataArray[Double1, UShort](ro), true, data)(Descriptors.Double1UShort)
    testArrayCast(ro, ReadDataArray[Vec2d, UShort](ro), true, data)(Descriptors.Vec2dUShort)
    testArrayCast(ro, ReadDataArray[Vec3d, UShort](ro), true, data)(Descriptors.Vec3dUShort)
    testArrayCast(ro, ReadDataArray[Vec4d, UShort](ro), true, data)(Descriptors.Vec4dUShort)
  }
  
  private def testSIntArrayCast(da: DataArray[_, SInt]) {
    val data = da.asBuffer()

    testArrayCast(da, DataArray[Int1, SInt](da), false, data)(Descriptors.Int1SInt)
    testArrayCast(da, DataArray[Vec2i, SInt](da), false, data)(Descriptors.Vec2iSInt)
    testArrayCast(da, DataArray[Vec3i, SInt](da), false, data)(Descriptors.Vec3iSInt)
    testArrayCast(da, DataArray[Vec4i, SInt](da), false, data)(Descriptors.Vec4iSInt)

    testArrayCast(da, DataArray[Float1, SInt](da), false, data)(Descriptors.Float1SInt)
    testArrayCast(da, DataArray[Vec2f, SInt](da), false, data)(Descriptors.Vec2fSInt)
    testArrayCast(da, DataArray[Vec3f, SInt](da), false, data)(Descriptors.Vec3fSInt)
    testArrayCast(da, DataArray[Vec4f, SInt](da), false, data)(Descriptors.Vec4fSInt)
    
    testArrayCast(da, DataArray[Double1, SInt](da), false, data)(Descriptors.Double1SInt)
    testArrayCast(da, DataArray[Vec2d, SInt](da), false, data)(Descriptors.Vec2dSInt)
    testArrayCast(da, DataArray[Vec3d, SInt](da), false, data)(Descriptors.Vec3dSInt)
    testArrayCast(da, DataArray[Vec4d, SInt](da), false, data)(Descriptors.Vec4dSInt)

    
    testArrayCast(da, ReadDataArray[Int1, SInt](da), false, data)(Descriptors.Int1SInt)
    testArrayCast(da, ReadDataArray[Vec2i, SInt](da), false, data)(Descriptors.Vec2iSInt)
    testArrayCast(da, ReadDataArray[Vec3i, SInt](da), false, data)(Descriptors.Vec3iSInt)
    testArrayCast(da, ReadDataArray[Vec4i, SInt](da), false, data)(Descriptors.Vec4iSInt)

    testArrayCast(da, ReadDataArray[Float1, SInt](da), false, data)(Descriptors.Float1SInt)
    testArrayCast(da, ReadDataArray[Vec2f, SInt](da), false, data)(Descriptors.Vec2fSInt)
    testArrayCast(da, ReadDataArray[Vec3f, SInt](da), false, data)(Descriptors.Vec3fSInt)
    testArrayCast(da, ReadDataArray[Vec4f, SInt](da), false, data)(Descriptors.Vec4fSInt)
    
    testArrayCast(da, ReadDataArray[Double1, SInt](da), false, data)(Descriptors.Double1SInt)
    testArrayCast(da, ReadDataArray[Vec2d, SInt](da), false, data)(Descriptors.Vec2dSInt)
    testArrayCast(da, ReadDataArray[Vec3d, SInt](da), false, data)(Descriptors.Vec3dSInt)
    testArrayCast(da, ReadDataArray[Vec4d, SInt](da), false, data)(Descriptors.Vec4dSInt)


    val ro = da.asReadOnlySeq()
    intercept[IllegalArgumentException] { DataArray[Int1, SInt](ro.asInstanceOf[DataArray[_, SInt]]) }
    
    testArrayCast(ro, ReadDataArray[Int1, SInt](ro), true, data)(Descriptors.Int1SInt)
    testArrayCast(ro, ReadDataArray[Vec2i, SInt](ro), true, data)(Descriptors.Vec2iSInt)
    testArrayCast(ro, ReadDataArray[Vec3i, SInt](ro), true, data)(Descriptors.Vec3iSInt)
    testArrayCast(ro, ReadDataArray[Vec4i, SInt](ro), true, data)(Descriptors.Vec4iSInt)

    testArrayCast(ro, ReadDataArray[Float1, SInt](ro), true, data)(Descriptors.Float1SInt)
    testArrayCast(ro, ReadDataArray[Vec2f, SInt](ro), true, data)(Descriptors.Vec2fSInt)
    testArrayCast(ro, ReadDataArray[Vec3f, SInt](ro), true, data)(Descriptors.Vec3fSInt)
    testArrayCast(ro, ReadDataArray[Vec4f, SInt](ro), true, data)(Descriptors.Vec4fSInt)
    
    testArrayCast(ro, ReadDataArray[Double1, SInt](ro), true, data)(Descriptors.Double1SInt)
    testArrayCast(ro, ReadDataArray[Vec2d, SInt](ro), true, data)(Descriptors.Vec2dSInt)
    testArrayCast(ro, ReadDataArray[Vec3d, SInt](ro), true, data)(Descriptors.Vec3dSInt)
    testArrayCast(ro, ReadDataArray[Vec4d, SInt](ro), true, data)(Descriptors.Vec4dSInt)
  }
  
  private def testUIntArrayCast(da: DataArray[_, UInt]) {
    val data = da.asBuffer()
    
    testArrayCast(da, DataArray[Int1, UInt](da), false, data)(Descriptors.Int1UInt)
    testArrayCast(da, DataArray[Vec2i, UInt](da), false, data)(Descriptors.Vec2iUInt)
    testArrayCast(da, DataArray[Vec3i, UInt](da), false, data)(Descriptors.Vec3iUInt)
    testArrayCast(da, DataArray[Vec4i, UInt](da), false, data)(Descriptors.Vec4iUInt)

    testArrayCast(da, IndexArray[UInt](da), false, data)(Descriptors.Int1UInt)

    testArrayCast(da, DataArray[Float1, UInt](da), false, data)(Descriptors.Float1UInt)
    testArrayCast(da, DataArray[Vec2f, UInt](da), false, data)(Descriptors.Vec2fUInt)
    testArrayCast(da, DataArray[Vec3f, UInt](da), false, data)(Descriptors.Vec3fUInt)
    testArrayCast(da, DataArray[Vec4f, UInt](da), false, data)(Descriptors.Vec4fUInt)
    
    testArrayCast(da, DataArray[Double1, UInt](da), false, data)(Descriptors.Double1UInt)
    testArrayCast(da, DataArray[Vec2d, UInt](da), false, data)(Descriptors.Vec2dUInt)
    testArrayCast(da, DataArray[Vec3d, UInt](da), false, data)(Descriptors.Vec3dUInt)
    testArrayCast(da, DataArray[Vec4d, UInt](da), false, data)(Descriptors.Vec4dUInt)

    
    testArrayCast(da, ReadDataArray[Int1, UInt](da), false, data)(Descriptors.Int1UInt)
    testArrayCast(da, ReadDataArray[Vec2i, UInt](da), false, data)(Descriptors.Vec2iUInt)
    testArrayCast(da, ReadDataArray[Vec3i, UInt](da), false, data)(Descriptors.Vec3iUInt)
    testArrayCast(da, ReadDataArray[Vec4i, UInt](da), false, data)(Descriptors.Vec4iUInt)

    testArrayCast(da, ReadIndexArray[UInt](da), false, data)(Descriptors.Int1UInt)

    testArrayCast(da, ReadDataArray[Float1, UInt](da), false, data)(Descriptors.Float1UInt)
    testArrayCast(da, ReadDataArray[Vec2f, UInt](da), false, data)(Descriptors.Vec2fUInt)
    testArrayCast(da, ReadDataArray[Vec3f, UInt](da), false, data)(Descriptors.Vec3fUInt)
    testArrayCast(da, ReadDataArray[Vec4f, UInt](da), false, data)(Descriptors.Vec4fUInt)
    
    testArrayCast(da, ReadDataArray[Double1, UInt](da), false, data)(Descriptors.Double1UInt)
    testArrayCast(da, ReadDataArray[Vec2d, UInt](da), false, data)(Descriptors.Vec2dUInt)
    testArrayCast(da, ReadDataArray[Vec3d, UInt](da), false, data)(Descriptors.Vec3dUInt)
    testArrayCast(da, ReadDataArray[Vec4d, UInt](da), false, data)(Descriptors.Vec4dUInt)


    val ro = da.asReadOnlySeq()
    intercept[IllegalArgumentException] { DataArray[Int1, UInt](ro.asInstanceOf[DataArray[_, UInt]]) }
    intercept[IllegalArgumentException] { IndexArray[UInt](ro.asInstanceOf[DataArray[_, UInt]]) }
    
    testArrayCast(ro, ReadDataArray[Int1, UInt](ro), true, data)(Descriptors.Int1UInt)
    testArrayCast(ro, ReadDataArray[Vec2i, UInt](ro), true, data)(Descriptors.Vec2iUInt)
    testArrayCast(ro, ReadDataArray[Vec3i, UInt](ro), true, data)(Descriptors.Vec3iUInt)
    testArrayCast(ro, ReadDataArray[Vec4i, UInt](ro), true, data)(Descriptors.Vec4iUInt)

    testArrayCast(ro, ReadIndexArray[UInt](ro), true, data)(Descriptors.Int1UInt)

    testArrayCast(ro, ReadDataArray[Float1, UInt](ro), true, data)(Descriptors.Float1UInt)
    testArrayCast(ro, ReadDataArray[Vec2f, UInt](ro), true, data)(Descriptors.Vec2fUInt)
    testArrayCast(ro, ReadDataArray[Vec3f, UInt](ro), true, data)(Descriptors.Vec3fUInt)
    testArrayCast(ro, ReadDataArray[Vec4f, UInt](ro), true, data)(Descriptors.Vec4fUInt)
    
    testArrayCast(ro, ReadDataArray[Double1, UInt](ro), true, data)(Descriptors.Double1UInt)
    testArrayCast(ro, ReadDataArray[Vec2d, UInt](ro), true, data)(Descriptors.Vec2dUInt)
    testArrayCast(ro, ReadDataArray[Vec3d, UInt](ro), true, data)(Descriptors.Vec3dUInt)
    testArrayCast(ro, ReadDataArray[Vec4d, UInt](ro), true, data)(Descriptors.Vec4dUInt)
  }

  private def testHalfFloatArrayCast(da: DataArray[_, HalfFloat]) {
    val data = da.asBuffer()

    testArrayCast(da, DataArray[Float1, HalfFloat](da), false, data)(Descriptors.Float1HalfFloat)
    testArrayCast(da, DataArray[Vec2f, HalfFloat](da), false, data)(Descriptors.Vec2fHalfFloat)
    testArrayCast(da, DataArray[Vec3f, HalfFloat](da), false, data)(Descriptors.Vec3fHalfFloat)
    testArrayCast(da, DataArray[Vec4f, HalfFloat](da), false, data)(Descriptors.Vec4fHalfFloat)

    testArrayCast(da, DataArray[Double1, HalfFloat](da), false, data)(Descriptors.Double1HalfFloat)
    testArrayCast(da, DataArray[Vec2d, HalfFloat](da), false, data)(Descriptors.Vec2dHalfFloat)
    testArrayCast(da, DataArray[Vec3d, HalfFloat](da), false, data)(Descriptors.Vec3dHalfFloat)
    testArrayCast(da, DataArray[Vec4d, HalfFloat](da), false, data)(Descriptors.Vec4dHalfFloat)


    testArrayCast(da, ReadDataArray[Float1, HalfFloat](da), false, data)(Descriptors.Float1HalfFloat)
    testArrayCast(da, ReadDataArray[Vec2f, HalfFloat](da), false, data)(Descriptors.Vec2fHalfFloat)
    testArrayCast(da, ReadDataArray[Vec3f, HalfFloat](da), false, data)(Descriptors.Vec3fHalfFloat)
    testArrayCast(da, ReadDataArray[Vec4f, HalfFloat](da), false, data)(Descriptors.Vec4fHalfFloat)

    testArrayCast(da, ReadDataArray[Double1, HalfFloat](da), false, data)(Descriptors.Double1HalfFloat)
    testArrayCast(da, ReadDataArray[Vec2d, HalfFloat](da), false, data)(Descriptors.Vec2dHalfFloat)
    testArrayCast(da, ReadDataArray[Vec3d, HalfFloat](da), false, data)(Descriptors.Vec3dHalfFloat)
    testArrayCast(da, ReadDataArray[Vec4d, HalfFloat](da), false, data)(Descriptors.Vec4dHalfFloat)


    val ro = da.asReadOnlySeq()
    intercept[IllegalArgumentException] { DataArray[Float1, HalfFloat](ro.asInstanceOf[DataArray[_, HalfFloat]]) }

    testArrayCast(ro, ReadDataArray[Float1, HalfFloat](ro), true, data)(Descriptors.Float1HalfFloat)
    testArrayCast(ro, ReadDataArray[Vec2f, HalfFloat](ro), true, data)(Descriptors.Vec2fHalfFloat)
    testArrayCast(ro, ReadDataArray[Vec3f, HalfFloat](ro), true, data)(Descriptors.Vec3fHalfFloat)
    testArrayCast(ro, ReadDataArray[Vec4f, HalfFloat](ro), true, data)(Descriptors.Vec4fHalfFloat)

    testArrayCast(ro, ReadDataArray[Double1, HalfFloat](ro), true, data)(Descriptors.Double1HalfFloat)
    testArrayCast(ro, ReadDataArray[Vec2d, HalfFloat](ro), true, data)(Descriptors.Vec2dHalfFloat)
    testArrayCast(ro, ReadDataArray[Vec3d, HalfFloat](ro), true, data)(Descriptors.Vec3dHalfFloat)
    testArrayCast(ro, ReadDataArray[Vec4d, HalfFloat](ro), true, data)(Descriptors.Vec4dHalfFloat)
  }
  
  private def testRawFloatArrayCast(da: DataArray[_, RawFloat]) {
    val data = da.asBuffer()

    testArrayCast(da, DataArray[Float1, RawFloat](da), false, data)(Descriptors.Float1RawFloat)
    testArrayCast(da, DataArray[Vec2f, RawFloat](da), false, data)(Descriptors.Vec2fRawFloat)
    testArrayCast(da, DataArray[Vec3f, RawFloat](da), false, data)(Descriptors.Vec3fRawFloat)
    testArrayCast(da, DataArray[Vec4f, RawFloat](da), false, data)(Descriptors.Vec4fRawFloat)

    testArrayCast(da, DataArray[Double1, RawFloat](da), false, data)(Descriptors.Double1RawFloat)
    testArrayCast(da, DataArray[Vec2d, RawFloat](da), false, data)(Descriptors.Vec2dRawFloat)
    testArrayCast(da, DataArray[Vec3d, RawFloat](da), false, data)(Descriptors.Vec3dRawFloat)
    testArrayCast(da, DataArray[Vec4d, RawFloat](da), false, data)(Descriptors.Vec4dRawFloat)


    testArrayCast(da, ReadDataArray[Float1, RawFloat](da), false, data)(Descriptors.Float1RawFloat)
    testArrayCast(da, ReadDataArray[Vec2f, RawFloat](da), false, data)(Descriptors.Vec2fRawFloat)
    testArrayCast(da, ReadDataArray[Vec3f, RawFloat](da), false, data)(Descriptors.Vec3fRawFloat)
    testArrayCast(da, ReadDataArray[Vec4f, RawFloat](da), false, data)(Descriptors.Vec4fRawFloat)

    testArrayCast(da, ReadDataArray[Double1, RawFloat](da), false, data)(Descriptors.Double1RawFloat)
    testArrayCast(da, ReadDataArray[Vec2d, RawFloat](da), false, data)(Descriptors.Vec2dRawFloat)
    testArrayCast(da, ReadDataArray[Vec3d, RawFloat](da), false, data)(Descriptors.Vec3dRawFloat)
    testArrayCast(da, ReadDataArray[Vec4d, RawFloat](da), false, data)(Descriptors.Vec4dRawFloat)


    val ro = da.asReadOnlySeq()
    intercept[IllegalArgumentException] { DataArray[Float1, RawFloat](ro.asInstanceOf[DataArray[_, RawFloat]]) }

    testArrayCast(ro, ReadDataArray[Float1, RawFloat](ro), true, data)(Descriptors.Float1RawFloat)
    testArrayCast(ro, ReadDataArray[Vec2f, RawFloat](ro), true, data)(Descriptors.Vec2fRawFloat)
    testArrayCast(ro, ReadDataArray[Vec3f, RawFloat](ro), true, data)(Descriptors.Vec3fRawFloat)
    testArrayCast(ro, ReadDataArray[Vec4f, RawFloat](ro), true, data)(Descriptors.Vec4fRawFloat)

    testArrayCast(ro, ReadDataArray[Double1, RawFloat](ro), true, data)(Descriptors.Double1RawFloat)
    testArrayCast(ro, ReadDataArray[Vec2d, RawFloat](ro), true, data)(Descriptors.Vec2dRawFloat)
    testArrayCast(ro, ReadDataArray[Vec3d, RawFloat](ro), true, data)(Descriptors.Vec3dRawFloat)
    testArrayCast(ro, ReadDataArray[Vec4d, RawFloat](ro), true, data)(Descriptors.Vec4dRawFloat)
  }
  
  private def testRawDoubleArrayCast(da: DataArray[_, RawDouble]) {
    val data = da.asBuffer()

    testArrayCast(da, DataArray[Double1, RawDouble](da), false, data)(Descriptors.Double1RawDouble)
    testArrayCast(da, DataArray[Vec2d, RawDouble](da), false, data)(Descriptors.Vec2dRawDouble)
    testArrayCast(da, DataArray[Vec3d, RawDouble](da), false, data)(Descriptors.Vec3dRawDouble)
    testArrayCast(da, DataArray[Vec4d, RawDouble](da), false, data)(Descriptors.Vec4dRawDouble)


    testArrayCast(da, ReadDataArray[Double1, RawDouble](da), false, data)(Descriptors.Double1RawDouble)
    testArrayCast(da, ReadDataArray[Vec2d, RawDouble](da), false, data)(Descriptors.Vec2dRawDouble)
    testArrayCast(da, ReadDataArray[Vec3d, RawDouble](da), false, data)(Descriptors.Vec3dRawDouble)
    testArrayCast(da, ReadDataArray[Vec4d, RawDouble](da), false, data)(Descriptors.Vec4dRawDouble)


    val ro = da.asReadOnlySeq()
    intercept[IllegalArgumentException] { DataArray[Double1, RawDouble](ro.asInstanceOf[DataArray[_, RawDouble]]) }

    testArrayCast(ro, ReadDataArray[Double1, RawDouble](ro), true, data)(Descriptors.Double1RawDouble)
    testArrayCast(ro, ReadDataArray[Vec2d, RawDouble](ro), true, data)(Descriptors.Vec2dRawDouble)
    testArrayCast(ro, ReadDataArray[Vec3d, RawDouble](ro), true, data)(Descriptors.Vec3dRawDouble)
    testArrayCast(ro, ReadDataArray[Vec4d, RawDouble](ro), true, data)(Descriptors.Vec4dRawDouble)
  }


  def testBufferCast[E <: MetaElement, R <: RawData](
    factory: (ByteBuffer) => DataBuffer[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    //
  }
}
