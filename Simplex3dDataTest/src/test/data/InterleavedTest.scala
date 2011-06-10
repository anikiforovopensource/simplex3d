/*
 * Simplex3d, DataTest package
 * Copyright (C) 2010-2011, Aleksey Nikiforov
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
import java.nio._
import org.scalatest._
import simplex3d.math._
import simplex3d.math.floatx._
import simplex3d.math.doublex._
import simplex3d.data._
import simplex3d.data.float._
import simplex3d.data.double._

import Descriptors._
import TestUtil._
import AttributeTestUtil._


/**
 * @author Aleksey Nikiforov (lex)
 */
class InterleavedTest extends FunSuite {

  test("Verify") {
    val buff = ByteBuffer.allocateDirect(64)

    // Exception: non-matching stride.
    intercept[DataFormatException] {
      InterleavedData.verifyFormat(
        Array(
          DataView[RFloat, RFloat](buff, 0, 2),
          DataView[RFloat, RFloat](buff, 1, 3)
        )
      )
    }

    // Exception: non-matching size
    val wrongSizeBuff = ByteBuffer.allocateDirect(68)
    intercept[DataFormatException] {
      InterleavedData.verifyFormat(
        Array(
          DataView[RFloat, RFloat](wrongSizeBuff, 0, 2),
          DataView[RFloat, SByte](wrongSizeBuff, 4, 8)
        )
      )
    }

    // Exception: different storeObject.
    intercept[DataFormatException] {
      InterleavedData.verifyFormat(
        Array(
          DataView[RFloat, RFloat](buff, 0, 2),
          DataView[RFloat, RFloat](ByteBuffer.allocateDirect(64), 1, 2)
        )
      )
    }

    // Exception: data overlap.
    intercept[DataFormatException] {
      InterleavedData.verifyFormat(
        Array(
          DataView[RFloat, RFloat](buff, 0, 2),
          DataView[RFloat, RFloat](buff, 0, 2)
        )
      )
    }

    // Working
    InterleavedData.verifyFormat(
      Array(
        DataView[RFloat, RFloat](buff, 0, 2),
        DataView[RFloat, RFloat](buff, 1, 2)
      )
    )
  }

  test("Constructors") {
    val buff = ByteBuffer.allocateDirect(64)

    // Exception: non-matching stride.
    intercept[DataFormatException] {
      new InterleavedData(
        DataView[RFloat, RFloat](buff, 0, 2),
        DataView[RFloat, RFloat](buff, 1, 3)
      )
    }

    // Exception: non-matching size
    val wrongSizeBuff = ByteBuffer.allocateDirect(68)
    intercept[DataFormatException] {
      new InterleavedData(
        DataView[RFloat, RFloat](wrongSizeBuff, 0, 2),
        DataView[RFloat, SByte](wrongSizeBuff, 4, 8)
      )
    }

    // Exception: different storeObject.
    intercept[DataFormatException] {
      new InterleavedData(
        DataView[RFloat, RFloat](buff, 0, 2),
        DataView[RFloat, RFloat](ByteBuffer.allocateDirect(64), 1, 2)
      )
    }

    // Exception: data overlap.
    intercept[DataFormatException] {
      new InterleavedData(
        DataView[RFloat, RFloat](buff, 0, 2),
        DataView[RFloat, RFloat](buff, 0, 2)
      )
    }

    // Working
    val views = Array(
      DataView[RFloat, RFloat](buff, 0, 2),
      DataView[RFloat, RFloat](buff, 1, 2)
    )

    {
      val id = new InterleavedData(views(0), views(1))
      assert(id(0) eq views(0))
      assert(id(1) eq views(1))
    }

    {
      val id = new InterleavedData(views)
      assert(id(0) eq views(0))
      assert(id(1) eq views(1))
    }
  }

  test("interleaveAll") {
    val size = 10

    for (i <- 0 until 10000) {
      val src = (for (c <- 0 until 10) yield mkRandOrEmpty(size))
      val res = interleaveAll(src: _*)(size)
      testInterleaved(src, res)

      // Test exception when applicable.
      if (src.exists(_.size != 0)) {
        intercept[IllegalArgumentException] {
          interleaveAll(src: _*)(size + 1)
        }
      }
    }
  }

  test("interleave") {
    val size = 10

    for (i <- 0 until 1000) {
      {
        val src = Array[DataSeq[_ <: Meta, Raw]](mkRandOrEmpty(size), mkRandOrEmpty(size))
        val (i1, i2) = interleave(src(0), src(1))(size)
        testInterleaved(src, Array[RawView](i1, i2))

        if (src.exists(_.size != 0)) {
          intercept[IllegalArgumentException] {
            interleave(src(0), src(1))(size + 1)
          }
        }
      }
      
      {
        val src = Array[DataSeq[_ <: Meta, Raw]](mkRandOrEmpty(size), mkRandOrEmpty(size), mkRandOrEmpty(size))
        val (i1, i2, i3) = interleave(src(0), src(1), src(2))(size)
        testInterleaved(src, Array[RawView](i1, i2, i3))

        if (src.exists(_.size != 0)) {
          intercept[IllegalArgumentException] {
            interleave(src(0), src(1), src(2))(size + 1)
          }
        }
      }
      
      {
        val src = Array[DataSeq[_ <: Meta, Raw]](
          mkRandOrEmpty(size), mkRandOrEmpty(size), mkRandOrEmpty(size), mkRandOrEmpty(size)
        )
        val (i1, i2, i3, i4) = interleave(src(0), src(1), src(2), src(3))(size)
        testInterleaved(src, Array[RawView](i1, i2, i3, i4))

        if (src.exists(_.size != 0)) {
          intercept[IllegalArgumentException] {
            interleave(src(0), src(1), src(2), src(3))(size + 1)
          }
        }
      }
      
      {
        val src = Array[DataSeq[_ <: Meta, Raw]](
          mkRandOrEmpty(size), mkRandOrEmpty(size), mkRandOrEmpty(size), mkRandOrEmpty(size),
          mkRandOrEmpty(size)
        )
        val (i1, i2, i3, i4, i5) = interleave(src(0), src(1), src(2), src(3), src(4))(size)
        testInterleaved(src, Array[RawView](i1, i2, i3, i4, i5))

        if (src.exists(_.size != 0)) {
          intercept[IllegalArgumentException] {
            interleave(src(0), src(1), src(2), src(3), src(4))(size + 1)
          }
        }
      }

      {
        val src = Array[DataSeq[_ <: Meta, Raw]](
          mkRandOrEmpty(size), mkRandOrEmpty(size), mkRandOrEmpty(size), mkRandOrEmpty(size),
          mkRandOrEmpty(size), mkRandOrEmpty(size)
        )
        val (i1, i2, i3, i4, i5, i6) = interleave(src(0), src(1), src(2), src(3), src(4), src(5))(size)
        testInterleaved(src, Array[RawView](i1, i2, i3, i4, i5, i6))

        if (src.exists(_.size != 0)) {
          intercept[IllegalArgumentException] {
            interleave(src(0), src(1), src(2), src(3), src(4), src(5))(size + 1)
          }
        }
      }

      {
        val src = Array[DataSeq[_ <: Meta, Raw]](
          mkRandOrEmpty(size), mkRandOrEmpty(size), mkRandOrEmpty(size), mkRandOrEmpty(size),
          mkRandOrEmpty(size), mkRandOrEmpty(size), mkRandOrEmpty(size)
        )
        val (i1, i2, i3, i4, i5, i6, i7) = interleave(
          src(0), src(1), src(2), src(3), src(4), src(5), src(6)
        )(size)
        
        testInterleaved(src, Array[RawView](i1, i2, i3, i4, i5, i6, i7))

        if (src.exists(_.size != 0)) {
          intercept[IllegalArgumentException] {
            interleave(
              src(0), src(1), src(2), src(3), src(4), src(5), src(6)
            )(size + 1)
          }
        }
      }

      {
        val src = Array[DataSeq[_ <: Meta, Raw]](
          mkRandOrEmpty(size), mkRandOrEmpty(size), mkRandOrEmpty(size), mkRandOrEmpty(size),
          mkRandOrEmpty(size), mkRandOrEmpty(size), mkRandOrEmpty(size), mkRandOrEmpty(size)
        )
        val (i1, i2, i3, i4, i5, i6, i7, i8) = interleave(
          src(0), src(1), src(2), src(3), src(4), src(5), src(6), src(7)
        )(size)

        testInterleaved(src, Array[RawView](i1, i2, i3, i4, i5, i6, i7, i8))

        if (src.exists(_.size != 0)) {
          intercept[IllegalArgumentException] {
            interleave(
              src(0), src(1), src(2), src(3), src(4), src(5), src(6), src(7)
            )(size + 1)
          }
        }
      }

      {
        val src = Array[DataSeq[_ <: Meta, Raw]](
          mkRandOrEmpty(size), mkRandOrEmpty(size), mkRandOrEmpty(size), mkRandOrEmpty(size),
          mkRandOrEmpty(size), mkRandOrEmpty(size), mkRandOrEmpty(size), mkRandOrEmpty(size),
          mkRandOrEmpty(size)
        )
        val (i1, i2, i3, i4, i5, i6, i7, i8, i9) = interleave(
          src(0), src(1), src(2), src(3), src(4), src(5), src(6), src(7), src(8)
        )(size)

        testInterleaved(src, Array[RawView](i1, i2, i3, i4, i5, i6, i7, i8, i9))

        if (src.exists(_.size != 0)) {
          intercept[IllegalArgumentException] {
            interleave(
              src(0), src(1), src(2), src(3), src(4), src(5), src(6), src(7), src(8)
            )(size + 1)
          }
        }
      }

      {
        val src = Array[DataSeq[_ <: Meta, Raw]](
          mkRandOrEmpty(size), mkRandOrEmpty(size), mkRandOrEmpty(size), mkRandOrEmpty(size),
          mkRandOrEmpty(size), mkRandOrEmpty(size), mkRandOrEmpty(size), mkRandOrEmpty(size),
          mkRandOrEmpty(size), mkRandOrEmpty(size)
        )
        val (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10) = interleave(
          src(0), src(1), src(2), src(3), src(4), src(5), src(6), src(7), src(8), src(9)
        )(size)

        testInterleaved(src, Array[RawView](i1, i2, i3, i4, i5, i6, i7, i8, i9, i10))

        if (src.exists(_.size != 0)) {
          intercept[IllegalArgumentException] {
            interleave(
              src(0), src(1), src(2), src(3), src(4), src(5), src(6), src(7), src(8), src(9)
            )(size + 1)
          }
        }
      }

      {
        val src = Array[DataSeq[_ <: Meta, Raw]](
          mkRandOrEmpty(size), mkRandOrEmpty(size), mkRandOrEmpty(size), mkRandOrEmpty(size),
          mkRandOrEmpty(size), mkRandOrEmpty(size), mkRandOrEmpty(size), mkRandOrEmpty(size),
          mkRandOrEmpty(size), mkRandOrEmpty(size), mkRandOrEmpty(size)
        )
        val (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11) = interleave(
          src(0), src(1), src(2), src(3), src(4), src(5), src(6), src(7), src(8), src(9), src(10)
        )(size)

        testInterleaved(src, Array[RawView](i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11))

        if (src.exists(_.size != 0)) {
          intercept[IllegalArgumentException] {
            interleave(
              src(0), src(1), src(2), src(3), src(4), src(5), src(6), src(7), src(8), src(9), src(10)
            )(size + 1)
          }
        }
      }

      {
        val src = Array[DataSeq[_ <: Meta, Raw]](
          mkRandOrEmpty(size), mkRandOrEmpty(size), mkRandOrEmpty(size), mkRandOrEmpty(size),
          mkRandOrEmpty(size), mkRandOrEmpty(size), mkRandOrEmpty(size), mkRandOrEmpty(size),
          mkRandOrEmpty(size), mkRandOrEmpty(size), mkRandOrEmpty(size), mkRandOrEmpty(size)
        )
        val (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12) = interleave(
          src(0), src(1), src(2), src(3), src(4), src(5), src(6), src(7), src(8), src(9), src(10), src(11)
        )(size)

        testInterleaved(src, Array[RawView](i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12))

        if (src.exists(_.size != 0)) {
          intercept[IllegalArgumentException] {
            interleave(
              src(0), src(1), src(2), src(3), src(4), src(5), src(6), src(7), src(8), src(9), src(10), src(11)
            )(size + 1)
          }
        }
      }
    }
  }

  test("Serialization") {
    val count = 10
    val size = 10

    for (i <- 0 until 10000) {
      val src = (for (c <- 0 until count) yield genRandomSeq(size))

      val rwdata = new InterleavedData(interleaveAll(src: _*)(size))
      val rodata = new InterleavedData(rwdata.map(_.asReadOnly()))

      val bytes = new ByteArrayOutputStream()
      val out = new ObjectOutputStream(bytes)
      out.writeObject(rwdata)
      out.writeObject(rodata)
      out.close()

      val in = new ObjectInputStream(new ByteArrayInputStream(bytes.toByteArray()))

      val rwrestored = in.readObject().asInstanceOf[InterleavedData]
      testInterleaved(src, rwrestored)
      for ((a, b) <- rwdata zip rwrestored) {
        assert(a.offset == b.offset)
        assert(a.stride == b.stride)
        assert(a.isReadOnly == b.isReadOnly)
      }

      val rorestored = in.readObject().asInstanceOf[InterleavedData]
      testInterleaved(src, rorestored)
      for ((a, b) <- rodata zip rorestored) {
        assert(a.offset == b.offset)
        assert(a.stride == b.stride)
        assert(a.isReadOnly == b.isReadOnly)
      }

      in.close()
    }
  }

  private def testInterleaved(src: Seq[DataSeq[_ <: Meta, Raw]], interleaved: Seq[RawView]) {
    // Test interleaved constraints.
    InterleavedData.verifyFormat(interleaved)

    
    for ((a, b) <- src.zip(interleaved); if (a.size != 0)) {
      // Test types

      assert(a.formatManifest == b.formatManifest)
      assert(a.readManifest == b.readManifest)
      assert(a.primitives.formatManifest == b.primitives.formatManifest)
      assert(a.primitives.readManifest == b.primitives.readManifest)
      assert(a.rawType == b.rawType)

      // Test content.
      assert(a.size == b.size)
      type T = F forSome { type F <: Meta }
      testContent(a.components, a.asInstanceOf[ReadDataSeq[T, Raw]], 0, b.asInstanceOf[ReadDataSeq[T, Raw]], 0, a.size)
    }
  }

  private val randomSrc = new java.util.Random(1)
  private def mkRandOrEmpty(sizeWhenSet: Int) :DataSeq[_ <: Meta, Raw] = {
    val size = if (randomSrc.nextBoolean) sizeWhenSet else 0
    genRandomSeq(size)
  }
}
