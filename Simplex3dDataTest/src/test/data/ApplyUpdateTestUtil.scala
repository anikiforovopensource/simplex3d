/*
 * Simplex3d, DataTest package
 * Copyright (C) 2010-2011, Simplex3d Team
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

import org.scalatest._

import java.nio._
import scala.reflect._
import simplex3d.data._
import simplex3d.data.float._
import simplex3d.data.double._
import simplex3d.math._
import simplex3d.math.floatx._
import simplex3d.math.doublex._

import RawType._
import TestUtil._


/**
 * @author Aleksey Nikiforov (lex)
 */
object ApplyUpdateTestUtil extends FunSuite {

  private val randomSrc = new java.util.Random(0)
  private def ni = randomSrc.nextInt
  private def nf = randomSrc.nextFloat
  private def nd = randomSrc.nextDouble

  def intFromBits(bits: String) :Int = {
    var x = 0L
    var i = 0; while (i < bits.length) {
      if (bits(i) != ' ') {
        x <<= 1
        if (bits(i) == '1') x |= 1
      }

      i += 1
    }

    x.toInt
  }
  def intToBits(x: Int) :String = {
    var s = ""
    var check = 0

    var i = 31; while (i >= 0) {
      if (i != 31 && (i + 1) % 8 == 0) s += " "

      val digit = (x >>> i) & 1
      s += digit
      if (i > 15) check |= digit

      i -= 1
    }

    if (check != 0) s else s.substring(18)
  }
  def floatFromBits(bits: String) = java.lang.Float.intBitsToFloat(intFromBits(bits))
  def floatToBits(x: Float) = intToBits(java.lang.Float.floatToRawIntBits(x))

  def testIndex[E <: Meta](seq: DataSeq[E, Raw]) {
    assert(seq.size > 0)

    intercept[Exception] { seq(-1) }
    seq(0)
    seq(seq.size - 1)
    intercept[Exception] { seq(seq.size) }

    val ro = seq.asReadOnly().asInstanceOf[DataSeq[E, Raw]]
    intercept[Exception] { ro(0) = seq(0) }
    intercept[Exception] { ro(seq.size - 1) = seq(0) }
  }

  def testApplyUpdate(seq: DataSeq[SInt, _], value: Int, expected: Int, store: AnyVal) {
    assert(seq.size > 0)
    val i = randomSrc.nextInt(seq.size)

    seq(i) = value
    assert(seq(i) == expected)
    verifyValue(seq.buffer(), seq.offset + i*seq.stride, store)
  }

  def testApplyUpdate(seq: DataSeq[RFloat, _], value: Float, expected: Float, store: AnyVal) {
    assert(seq.size > 0)
    val i = randomSrc.nextInt(seq.size)

    seq(i) = value

    if (floatx.functions.isnan(expected)) assert(floatx.functions.isnan(seq(i)))
    else assert(seq(i) == expected)

    verifyValue(seq.buffer(), seq.offset + i*seq.stride, store)
  }

  def testApplyUpdate(seq: DataSeq[RDouble, _], value: Double, expected: Double, store: AnyVal) {
    assert(seq.size > 0)
    val i = randomSrc.nextInt(seq.size)

    seq(i) = value

    if (doublex.functions.isnan(expected)) assert(doublex.functions.isnan(seq(i)))
    else assert(seq(i) == expected)

    verifyValue(seq.buffer(), seq.offset + i*seq.stride, store)
  }

  private[this] final def toByte(value: AnyVal) :Byte = {
    (value: @unchecked) match {
      case x: Int => x.toByte
      case x: Byte => x
    }
  }
  private[this] final def toShort(value: AnyVal) :Short = {
    (value: @unchecked) match {
      case x: Int => x.toShort
      case x: Short => x
    }
  }
  private[this] final def toChar(value: AnyVal) :Char = {
    (value: @unchecked) match {
      case x: Int => x.toChar
      case x: Char => x
    }
  }
  private[this] final def toFloat(value: AnyVal) :Float = {
    (value: @unchecked) match {
      case x: Int => x.toFloat
      case x: Float => x
    }
  }
  private[this] final def toDouble(value: AnyVal) :Double = {
    (value: @unchecked) match {
      case x: Int => x.toDouble
      case x: Double => x
    }
  }
  private def verifyValue(buff: Buffer, index: Int, value: AnyVal) {
    buff match {
      case b: ByteBuffer => assert(b.get(index) == toByte(value))
      case b: ShortBuffer => assert(b.get(index) == toShort(value))
      case b: CharBuffer => assert(b.get(index) == toChar(value))
      case b: IntBuffer => assert(b.get(index) == value.asInstanceOf[Int])
      case b: FloatBuffer =>
        val stored = b.get(index)
        if (floatx.functions.isnan(stored)) assert(floatx.functions.isnan(value.asInstanceOf[Float]))
        else assert(b.get(index) == toFloat(value))
      case b: DoubleBuffer =>
        val stored = b.get(index)
        if (doublex.functions.isnan(stored)) assert(doublex.functions.isnan(value.asInstanceOf[Double]))
        else assert(b.get(index) == toDouble(value))
    }
  }

  private def verifyComponents(seq: inData[_ <: Meta], i: Int) {
    def get(i: Int, j: Int) :Any = seq.backing(seq.offset + seq.stride*i + j)
    def cmp(x: Any, y: Any) {
      (x, y) match {
        case (a: Int, b: Int) => assert(a == b)
        case (a: Float, b: Float) =>
          if (floatx.functions.isnan(a)) assert(floatx.functions.isnan(b))
          else assert(a == b)
        case (a: Double, b: Double) =>
          if (doublex.functions.isnan(a)) assert(doublex.functions.isnan(b))
          else assert(a == b)
      }
    }

    seq(i) match {
      case Vec2i(x, y) =>       cmp(x, get(i, 0)); cmp(y, get(i, 1))
      case Vec3i(x, y, z) =>    cmp(x, get(i, 0)); cmp(y, get(i, 1)); cmp(z, get(i, 2))
      case Vec4i(x, y, z, w) => cmp(x, get(i, 0)); cmp(y, get(i, 1)); cmp(z, get(i, 2)); cmp(w, get(i, 3))
      case Vec2f(x, y) =>       cmp(x, get(i, 0)); cmp(y, get(i, 1))
      case Vec3f(x, y, z) =>    cmp(x, get(i, 0)); cmp(y, get(i, 1)); cmp(z, get(i, 2))
      case Vec4f(x, y, z, w) => cmp(x, get(i, 0)); cmp(y, get(i, 1)); cmp(z, get(i, 2)); cmp(w, get(i, 3))
      case Vec2d(x, y) =>       cmp(x, get(i, 0)); cmp(y, get(i, 1))
      case Vec3d(x, y, z) =>    cmp(x, get(i, 0)); cmp(y, get(i, 1)); cmp(z, get(i, 2))
      case Vec4d(x, y, z, w) => cmp(x, get(i, 0)); cmp(y, get(i, 1)); cmp(z, get(i, 2)); cmp(w, get(i, 3))
    }
  }

  private def updateValue[E <: Meta](seq: outData[E], i: Int, bcopy: outData[E#Component]) {
    def iput(i: Int, j: Int, u: Int) {
      val s = bcopy.asInstanceOf[Data[SInt]]
      s(seq.offset + seq.stride*i + j) = u
    }
    def fput(i: Int, j: Int, u: Float) {
      val s = bcopy.asInstanceOf[Data[RFloat]]
      s(seq.offset + seq.stride*i + j) = u
    }
    def dput(i: Int, j: Int, u: Double) {
      val s = bcopy.asInstanceOf[Data[RDouble]]
      s(seq.offset + seq.stride*i + j) = u
    }

    val e = seq.elemManifest match {
      case Vec2i.Manifest =>
        val u = Vec2i(ni, ni)
        iput(i, 0, u.x); iput(i, 1, u.y)
        u
      case Vec3i.Manifest =>
        val u = Vec3i(ni, ni, ni)
        iput(i, 0, u.x); iput(i, 1, u.y); iput(i, 2, u.z)
        u
      case Vec4i.Manifest =>
        val u = Vec4i(ni, ni, ni, ni)
        iput(i, 0, u.x); iput(i, 1, u.y); iput(i, 2, u.z); iput(i, 3, u.w)
        u
      case Vec2f.Manifest =>
        val u = Vec2f(nf, nf)
        fput(i, 0, u.x); fput(i, 1, u.y)
        u
      case Vec3f.Manifest =>
        val u = Vec3f(nf, nf, nf)
        fput(i, 0, u.x); fput(i, 1, u.y); fput(i, 2, u.z)
        u
      case Vec4f.Manifest =>
        val u = Vec4f(nf, nf, nf, nf)
        fput(i, 0, u.x); fput(i, 1, u.y); fput(i, 2, u.z); fput(i, 3, u.w)
        u
      case Vec2d.Manifest =>
        val u = Vec2d(nd, nd)
        dput(i, 0, u.x); dput(i, 1, u.y)
        u
      case Vec3d.Manifest =>
        val u = Vec3d(nd, nd, nd)
        dput(i, 0, u.x); dput(i, 1, u.y); dput(i, 2, u.z)
        u
      case Vec4d.Manifest =>
        val u = Vec4d(nd, nd, nd, nd)
        dput(i, 0, u.x); dput(i, 1, u.y); dput(i, 2, u.z); dput(i, 3, u.w)
        u
    }

    seq(i) = e.asInstanceOf[E#Read]
  }

  private def testApplyUpdate[E <: Meta](seq: outData[E]) {
    testIndex(seq)

    val bcopy = seq.backing.copyAsDataArray()

    var i = 0; while (i < seq.size) {
      verifyComponents(seq, i)
      i += 1
    }

    i = 0; while (i < seq.size) {
      updateValue(seq, i, bcopy)
      i += 1
    }

    testContent(1, bcopy, 0, seq.backing, 0, bcopy.size)
  }

  def testApplyUpdateArray[E <: Meta, R <: Raw](
    factory: (R#Array) => DataArray[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    testApplyUpdate(factory(genRandomArray(10, descriptor)))
  }

  def testApplyUpdateBuffer[E <: Meta, R <: Raw](
    factory: (ByteBuffer) => DataBuffer[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    val size = 10*descriptor.components*RawType.byteLength(descriptor.rawType)
    testApplyUpdate(factory(genRandomBuffer(size, descriptor)._1))
  }

  def testApplyUpdateView[E <: Meta, R <: Raw](
    factory: (ByteBuffer, Int, Int) => DataView[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    val c = descriptor.components
    val size = 10*c*RawType.byteLength(descriptor.rawType)
    testApplyUpdate(factory(genRandomBuffer(size, descriptor)._1, 0, c))
    testApplyUpdate(factory(genRandomBuffer(size, descriptor)._1, 0, c + 1))
    testApplyUpdate(factory(genRandomBuffer(size, descriptor)._1, 0, c + 2))
    testApplyUpdate(factory(genRandomBuffer(size, descriptor)._1, 0, c + 3))
    testApplyUpdate(factory(genRandomBuffer(size, descriptor)._1, 0, c + 4))
    testApplyUpdate(factory(genRandomBuffer(size, descriptor)._1, 1, c + 1))
    testApplyUpdate(factory(genRandomBuffer(size, descriptor)._1, 1, c + 2))
    testApplyUpdate(factory(genRandomBuffer(size, descriptor)._1, 1, c + 3))
    testApplyUpdate(factory(genRandomBuffer(size, descriptor)._1, 1, c + 4))
    testApplyUpdate(factory(genRandomBuffer(size, descriptor)._1, 2, c + 2))
    testApplyUpdate(factory(genRandomBuffer(size, descriptor)._1, 2, c + 3))
    testApplyUpdate(factory(genRandomBuffer(size, descriptor)._1, 2, c + 4))
    testApplyUpdate(factory(genRandomBuffer(size, descriptor)._1, 3, c + 3))
    testApplyUpdate(factory(genRandomBuffer(size, descriptor)._1, 3, c + 4))
    testApplyUpdate(factory(genRandomBuffer(size, descriptor)._1, 4, c + 4))
  }
}
