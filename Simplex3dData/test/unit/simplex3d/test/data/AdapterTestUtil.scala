/*
 * Simplex3dData - Test Package
 * Copyright (C) 2011, Aleksey Nikiforov
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

package simplex3d.test.data

import java.nio._
import scala.language.existentials
import scala.reflect._
import scala.reflect.runtime.universe._
import org.scalatest._
import simplex3d.math.floatx._
import simplex3d.math.doublex._
import simplex3d.data._
import simplex3d.data.extension._
import TestUtil._
import AttributeTestUtil._


/**
 * @author Aleksey Nikiforov (lex)
 */
object AdapterTestUtil extends FunSuite {

  def testAdapter[F <: CompositeFormat, B <: Raw with Tangible](adapter: DataAdapter[F, B])(
    sample: F#Accessor#Read, sampleData: DataArray[F#Component, Raw]
  )(implicit attribs: AdapterAttrib[F, B])
  {
    // Test attributes.
    assert(adapter.components == attribs.components)
    assert(adapter.formatTag == attribs.formatTag)
    assert(adapter.accessorTag == attribs.accessorTag)

    // Test make.
    val primSize = 10*adapter.components
    for (readOnly <- List(true, false); rawTag <- attribs.allowed) {

      type R = T forSome { type T <: B }
      val original = genRandomSeq(
        attribs.componentTag, RawEnum.TypeTags.toRawEnum(rawTag), primSize
      ).asInstanceOf[ReadDataArray[F#Component, R]]

      val descriptor = Descriptor[F, R](
        attribs.formatTag, attribs.componentTag, attribs.accessorTag,
        attribs.components, original.rawEnum, original.isNormalized
      )

      val primitiveArray =
        if (readOnly) original.copyAsDataArray().asReadOnly().asInstanceOf[DataArray[F#Component, R]]
        else original.copyAsDataArray()

      val primitiveBuffer =
        if (readOnly) original.copyAsDataBuffer().asReadOnly().asInstanceOf[DataBuffer[F#Component, R]]
        else original.copyAsDataBuffer()

      val offset = 1; val stride = adapter.components + 1

      // Test mkReadData.
      {
        testArray(adapter.mkReadDataArray(primitiveArray), readOnly, original.readOnlyBuffer)(descriptor)
        testBuffer(adapter.mkReadDataBuffer(primitiveBuffer), readOnly, original.readOnlyBuffer)(descriptor)

        testView(
          adapter.mkReadDataView(primitiveBuffer, offset, stride),
          offset, stride, readOnly, original.readOnlyBuffer
        )(descriptor)
      }

      // Test mkData.
      if (readOnly) {
        intercept[IllegalArgumentException] {
          testArray(adapter.mkDataArray(primitiveArray), true, original.readOnlyBuffer)(descriptor)
        }

        intercept[IllegalArgumentException] {
          testBuffer(adapter.mkDataBuffer(primitiveBuffer), true, original.readOnlyBuffer)(descriptor)
        }

        intercept[IllegalArgumentException] {
          testView(
            adapter.mkDataView(primitiveBuffer, offset, stride),
            offset, stride, true, original.readOnlyBuffer
          )(descriptor)
        }
      }
      else {
        testArray(adapter.mkDataArray(primitiveArray), false, original.readOnlyBuffer)(descriptor)
        testBuffer(adapter.mkDataBuffer(primitiveBuffer), false, original.readOnlyBuffer)(descriptor)

        testView(
          adapter.mkDataView(primitiveBuffer, offset, stride),
          offset, stride, false, original.readOnlyBuffer
        )(descriptor)
      }
    }

    val factory = {
      type R = T forSome { type T <: B }
      val primitives = genRandomSeq(
        attribs.componentTag, RawEnum.ClassTags.toRawEnum(attribs.componentTag), 0
      ).asInstanceOf[ReadDataArray[F#Component, R]]
      adapter.mkReadDataArray(primitives).asInstanceOf[CompositionFactory[F, B]]
    }

    // Test raw types that are not allowed.
    for (
      raw <- RawEnum.TypeTags.AllTangible
      if !attribs.allowed.contains(raw) && supportsRawEnum(attribs.componentTag, raw)
    ) {
      type R = T forSome { type T <: B }

      val array = genRandomSeq(
        attribs.componentTag, RawEnum.TypeTags.toRawEnum(raw), primSize
      ).asInstanceOf[DataArray[F#Component, R]]

      val buffer = array.copyAsDataBuffer()
      val offset = 1; val stride = adapter.components + 1

      intercept[IllegalArgumentException] {
        adapter.mkReadDataArray(array)
      }
      intercept[IllegalArgumentException] {
        adapter.mkReadDataBuffer(buffer)
      }
      intercept[IllegalArgumentException] {
        adapter.mkReadDataView(buffer, offset, stride)
      }

      intercept[IllegalArgumentException] {
        adapter.mkDataArray(array)
      }
      intercept[IllegalArgumentException] {
        adapter.mkDataBuffer(buffer)
      }
      intercept[IllegalArgumentException] {
        adapter.mkDataView(buffer, offset, stride)
      }

      intercept[IllegalArgumentException] {
        factory.mkReadDataArray(array)
      }
      intercept[IllegalArgumentException] {
        factory.mkReadDataBuffer(buffer)
      }
      intercept[IllegalArgumentException] {
        factory.mkReadDataView(buffer, offset, stride)
      }

      intercept[IllegalArgumentException] {
        factory.mkDataArray(array)
      }
      intercept[IllegalArgumentException] {
        factory.mkDataBuffer(buffer)
      }
      intercept[IllegalArgumentException] {
        factory.mkDataView(buffer, offset, stride)
      }
    }

    // Test apply/update.
    assert(sampleData.formatTag == attribs.componentTag)

    val j = 1
    val writeBuffer = genRandomSeq(
      attribs.componentTag, sampleData.rawEnum, sampleData.size + j
    ).asInstanceOf[Contiguous[F#Component, Raw]]

    assert(adapter.apply(sampleData, 0) == sample)
    assert(adapter.apply(writeBuffer, j) != sample)
    adapter.update(writeBuffer, j, sample)
    assert(adapter.apply(writeBuffer, j) == sample)
  }
}

case class AdapterAttrib[F <: Format, B <: Raw with Tangible]
(components: Int, allowed: Seq[TypeTag[_ <: Raw with Tangible]])(
  implicit
  val formatTag: ClassTag[F],
  val accessorTag: ClassTag[F#Accessor],
  val componentTag: ClassTag[F#Component]
)

object AdapterAttribs {
  import RawEnum.TypeTags._

  private val allowedFloat: Seq[TypeTag[_ <: Raw with Tangible]] =
    Seq(SByte, UByte, SShort, UShort, SInt, UInt, HFloat, RFloat)
    
  private val allowedDouble = allowedFloat :+ RDouble
  
  
  implicit val Quat4fAttribs = AdapterAttrib[Quat4f, TangibleFloat](4, allowedFloat)
  implicit val Mat2x2fAttribs = AdapterAttrib[Mat2x2f, TangibleFloat](4, allowedFloat)
  implicit val Mat2x3fAttribs = AdapterAttrib[Mat2x3f, TangibleFloat](6, allowedFloat)
  implicit val Mat2x4fAttribs = AdapterAttrib[Mat2x4f, TangibleFloat](8, allowedFloat)
  implicit val Mat3x2fAttribs = AdapterAttrib[Mat3x2f, TangibleFloat](6, allowedFloat)
  implicit val Mat3x3fAttribs = AdapterAttrib[Mat3x3f, TangibleFloat](9, allowedFloat)
  implicit val Mat3x4fAttribs = AdapterAttrib[Mat3x4f, TangibleFloat](12, allowedFloat)
  implicit val Mat4x2fAttribs = AdapterAttrib[Mat4x2f, TangibleFloat](8, allowedFloat)
  implicit val Mat4x3fAttribs = AdapterAttrib[Mat4x3f, TangibleFloat](12, allowedFloat)
  implicit val Mat4x4fAttribs = AdapterAttrib[Mat4x4f, TangibleFloat](16, allowedFloat)

  implicit val Quat4dAttribs = AdapterAttrib[Quat4d, TangibleDouble](4, allowedDouble)
  implicit val Mat2x2dAttribs = AdapterAttrib[Mat2x2d, TangibleDouble](4, allowedDouble)
  implicit val Mat2x3dAttribs = AdapterAttrib[Mat2x3d, TangibleDouble](6, allowedDouble)
  implicit val Mat2x4dAttribs = AdapterAttrib[Mat2x4d, TangibleDouble](8, allowedDouble)
  implicit val Mat3x2dAttribs = AdapterAttrib[Mat3x2d, TangibleDouble](6, allowedDouble)
  implicit val Mat3x3dAttribs = AdapterAttrib[Mat3x3d, TangibleDouble](9, allowedDouble)
  implicit val Mat3x4dAttribs = AdapterAttrib[Mat3x4d, TangibleDouble](12, allowedDouble)
  implicit val Mat4x2dAttribs = AdapterAttrib[Mat4x2d, TangibleDouble](8, allowedDouble)
  implicit val Mat4x3dAttribs = AdapterAttrib[Mat4x3d, TangibleDouble](12, allowedDouble)
  implicit val Mat4x4dAttribs = AdapterAttrib[Mat4x4d, TangibleDouble](16, allowedDouble)
}
