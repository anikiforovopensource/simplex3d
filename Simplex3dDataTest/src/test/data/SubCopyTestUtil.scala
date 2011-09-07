/*
 * Simplex3d, DataTest package
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

package test.data

import org.scalatest._
import simplex3d.data._
import simplex3d.math._
import simplex3d.math.double.functions._
import TestUtil._
import CopyTestUtil._


/**
 * @author Aleksey Nikiforov (lex)
 */
object SubCopyTestUtil extends FunSuite {
  
  def testSubCopy[F <: Format, R <: Raw](
    factory: DataFactory[F, R]
  )(implicit descriptor: Descriptor[F, R]) {
    test2d(factory)
    test3d(factory)
  }
  
  private def test2d[F <: Format, R <: Raw](
    factory: DataFactory[F, R]
  )(implicit descriptor: Descriptor[F, R]) {
    val dims = ConstVec2i(6)
    val size = dims.x*dims.y

    val original = factory.mkDataArray(genRandomArray(size*factory.components, descriptor)).asReadOnly()
    val matchingSrc = factory.mkDataArray(genRandomArray(size*factory.components, descriptor)).asReadOnly()
    val seqSrc: IndexedSeq[F#Accessor#Read] = matchingSrc.toArray(readManifest(matchingSrc.accessorManifest))
    val convertionSrc = genRandomSeq(
      original.formatManifest,
      conversionType(original.formatManifest, original.rawType),
      size
    ).asInstanceOf[DataArray[F, R]].asReadOnly()
    
    
    val dest = original.copyAsDataArray()
    def testExceptions(src: IndexedSeq[F#Accessor#Read]) {
      intercept[IllegalArgumentException] { dest.put2d(dims, Vec2i(-1, 0), src, dims, Vec2i(0), Vec2i(0)) }
      intercept[IllegalArgumentException] { dest.put2d(dims, Vec2i(0, -1), src, dims, Vec2i(0), Vec2i(0)) }
      intercept[IllegalArgumentException] { dest.put2d(dims, Vec2i(0), src, dims, Vec2i(-1, 0), Vec2i(0)) }
      intercept[IllegalArgumentException] { dest.put2d(dims, Vec2i(0), src, dims, Vec2i(0, -1), Vec2i(0)) }
      intercept[IllegalArgumentException] { dest.put2d(dims, Vec2i(0), src, dims, Vec2i(0), Vec2i(-1, 0)) }
      intercept[IllegalArgumentException] { dest.put2d(dims, Vec2i(0), src, dims, Vec2i(0), Vec2i(0, -1)) }
      
      intercept[IllegalArgumentException] { dest.put2d(dims, Vec2i(dims.x + 1, 0), src, dims, Vec2i(0), Vec2i(0)) }
      intercept[IllegalArgumentException] { dest.put2d(dims, Vec2i(0, dims.y + 1), src, dims, Vec2i(0), Vec2i(0)) }
      intercept[IllegalArgumentException] { dest.put2d(dims, Vec2i(0), src, dims, Vec2i(dims.x + 1, 0), Vec2i(0)) }
      intercept[IllegalArgumentException] { dest.put2d(dims, Vec2i(0), src, dims, Vec2i(0, dims.y + 1), Vec2i(0)) }
      intercept[IllegalArgumentException] { dest.put2d(dims, Vec2i(0), src, dims, Vec2i(0), Vec2i(dims.x + 1, 0)) }
      intercept[IllegalArgumentException] { dest.put2d(dims, Vec2i(0), src, dims, Vec2i(0), Vec2i(0, dims.y + 1)) }
      
      intercept[IllegalArgumentException] { dest.put2d(dims, Vec2i(1), src, dims, Vec2i(1), Vec2i(dims.x, 0)) }
      intercept[IllegalArgumentException] { dest.put2d(dims, Vec2i(1), src, dims, Vec2i(1), Vec2i(0, dims.y)) }
    }
    
    testExceptions(matchingSrc)
    testExceptions(convertionSrc)
    testExceptions(seqSrc)
    

    def verify(
      dest: inDataArray[F, R], destOffset: inVec2i,
      src: inDataArray[F, Raw], srcOffset: inVec2i,
      copyDims: inVec2i
    ) {
      val copy = original.copyAsDataArray()
      
      for (y <- 0 until copyDims.y; x <- 0 until copyDims.x) {
        copy(x + destOffset.x + (y + destOffset.y)*dims.x) = src(x + srcOffset.x + (y + srcOffset.y)*dims.x)
      }
      
      testContent(
        dest.components,
        dest, 0,
        copy, 0,
        dest.size
      )
    }
    
    def test(
      destOffset: inVec2i,
      src: IndexedSeq[F#Accessor#Read], verifySrc: inDataArray[F, Raw], srcOffset: inVec2i,
      copyDims: inVec2i
    ) {
      val dest = original.copyAsDataArray()
      dest.put2d(
        dims, destOffset,
        src, dims, srcOffset,
        copyDims
      )
      
      verify(dest, destOffset, verifySrc, srcOffset, copyDims)
    }
    
    for (
      srcType <- 0 to 2;
      dx <- 0 to 2 by 2; dy <- 0 to 2 by 2;
      sx <- 0 to 2 by 2; sy <- 0 to 2 by 2;
      cx <- 0 to min(dims.x - max(dx, sx), 6) by 2; cy <- 0 to min(dims.y - max(dy, sy), 6) by 2
    ) {
      val (src, verifySrc) = srcType match {
        case 0 => (matchingSrc, matchingSrc)
        case 1 => (convertionSrc, convertionSrc)
        case 2 => (seqSrc, matchingSrc)
      }
      test(ConstVec2i(dx, dy), src, verifySrc, ConstVec2i(sx, sy), ConstVec2i(cx, cy))
    }
  }
  
  private def test3d[F <: Format, R <: Raw](
    factory: DataFactory[F, R]
  )(implicit descriptor: Descriptor[F, R]) {
    val dims = ConstVec3i(6)
    val size = dims.x*dims.y*dims.z

    val original = factory.mkDataArray(genRandomArray(size*factory.components, descriptor)).asReadOnly()
    val matchingSrc = factory.mkDataArray(genRandomArray(size*factory.components, descriptor)).asReadOnly()
    val seqSrc: IndexedSeq[F#Accessor#Read] = matchingSrc.toArray(readManifest(matchingSrc.accessorManifest))
    val convertionSrc = genRandomSeq(
      original.formatManifest,
      conversionType(original.formatManifest, original.rawType),
      size
    ).asInstanceOf[DataArray[F, R]].asReadOnly()
    

    val dest = original.copyAsDataArray()
    def testExceptions(src: IndexedSeq[F#Accessor#Read]) {
      intercept[IllegalArgumentException] { dest.put3d(dims, Vec3i(-1, 0, 0), src, dims, Vec3i(0), Vec3i(0)) }
      intercept[IllegalArgumentException] { dest.put3d(dims, Vec3i(0, -1, 0), src, dims, Vec3i(0), Vec3i(0)) }
      intercept[IllegalArgumentException] { dest.put3d(dims, Vec3i(0, 0, -1), src, dims, Vec3i(0), Vec3i(0)) }
      intercept[IllegalArgumentException] { dest.put3d(dims, Vec3i(0), src, dims, Vec3i(-1, 0, 0), Vec3i(0)) }
      intercept[IllegalArgumentException] { dest.put3d(dims, Vec3i(0), src, dims, Vec3i(0, -1, 0), Vec3i(0)) }
      intercept[IllegalArgumentException] { dest.put3d(dims, Vec3i(0), src, dims, Vec3i(0, 0, -1), Vec3i(0)) }
      intercept[IllegalArgumentException] { dest.put3d(dims, Vec3i(0), src, dims, Vec3i(0), Vec3i(-1, 0, 0)) }
      intercept[IllegalArgumentException] { dest.put3d(dims, Vec3i(0), src, dims, Vec3i(0), Vec3i(0, -1, 0)) }
      intercept[IllegalArgumentException] { dest.put3d(dims, Vec3i(0), src, dims, Vec3i(0), Vec3i(0, 0, -1)) }
      
      intercept[IllegalArgumentException] { dest.put3d(dims, Vec3i(dims.x + 1, 0, 0), src, dims, Vec3i(0), Vec3i(0)) }
      intercept[IllegalArgumentException] { dest.put3d(dims, Vec3i(0, dims.y + 1, 0), src, dims, Vec3i(0), Vec3i(0)) }
      intercept[IllegalArgumentException] { dest.put3d(dims, Vec3i(0, 0, dims.z + 1), src, dims, Vec3i(0), Vec3i(0)) }
      intercept[IllegalArgumentException] { dest.put3d(dims, Vec3i(0), src, dims, Vec3i(dims.x + 1, 0, 0), Vec3i(0)) }
      intercept[IllegalArgumentException] { dest.put3d(dims, Vec3i(0), src, dims, Vec3i(0, dims.y + 1, 0), Vec3i(0)) }
      intercept[IllegalArgumentException] { dest.put3d(dims, Vec3i(0), src, dims, Vec3i(0, 0, dims.z + 1), Vec3i(0)) }
      intercept[IllegalArgumentException] { dest.put3d(dims, Vec3i(0), src, dims, Vec3i(0), Vec3i(dims.x + 1, 0, 0)) }
      intercept[IllegalArgumentException] { dest.put3d(dims, Vec3i(0), src, dims, Vec3i(0), Vec3i(0, dims.y + 1, 0)) }
      intercept[IllegalArgumentException] { dest.put3d(dims, Vec3i(0), src, dims, Vec3i(0), Vec3i(0, 0, dims.z + 1)) }
      
      intercept[IllegalArgumentException] { dest.put3d(dims, Vec3i(1), src, dims, Vec3i(1), Vec3i(dims.x, 0, 0)) }
      intercept[IllegalArgumentException] { dest.put3d(dims, Vec3i(1), src, dims, Vec3i(1), Vec3i(0, dims.y, 0)) }
      intercept[IllegalArgumentException] { dest.put3d(dims, Vec3i(1), src, dims, Vec3i(1), Vec3i(0, 0, dims.z)) }
    }
    
    testExceptions(matchingSrc)
    testExceptions(convertionSrc)
    testExceptions(seqSrc)
    
    
    def verify(
      dest: inDataArray[F, R], destOffset: inVec3i,
      src: inDataArray[F, Raw], srcOffset: inVec3i,
      copyDims: inVec3i
    ) {
      val copy = original.copyAsDataArray()
      
      for (z <- 0 until copyDims.z; y <- 0 until copyDims.y; x <- 0 until copyDims.x) {
        val di = x + destOffset.x + (y + destOffset.y)*dims.x + (z + destOffset.z)*dims.x*dims.y
        val si = x + srcOffset.x + (y + srcOffset.y)*dims.x + (z + srcOffset.z)*dims.x*dims.y
        copy(di) = src(si)
      }
      
      testContent(
        dest.components,
        dest, 0,
        copy, 0,
        dest.size
      )
    }
    
    def test(
      destOffset: inVec3i,
      src: IndexedSeq[F#Accessor#Read], verifySrc: inDataArray[F, Raw], srcOffset: inVec3i,
      copyDims: inVec3i
    ) {
      val dest = original.copyAsDataArray()
      dest.put3d(
        dims, destOffset,
        src, dims, srcOffset,
        copyDims
      )
      
      verify(dest, destOffset, verifySrc, srcOffset, copyDims)
    }
    
    for (
      srcType <- 0 to 2;
      dx <- 0 to 2 by 2; dy <- 0 to 2 by 2; dz <- 0 to 2 by 2;
      sx <- 0 to 2 by 2; sy <- 0 to 2 by 2; sz <- 0 to 2 by 2;
      cx <- 0 to min(dims.x - max(dx, sx), 6) by 2;
      cy <- 0 to min(dims.y - max(dy, sy), 6) by 2;
      cz <- 0 to min(dims.z - max(dz, sz), 6) by 2
    ) {
      val (src, verifySrc) = srcType match {
        case 0 => (matchingSrc, matchingSrc)
        case 1 => (convertionSrc, convertionSrc)
        case 2 => (seqSrc, matchingSrc)
      }
      test(ConstVec3i(dx, dy, dz), src, verifySrc, ConstVec3i(sx, sy, sz), ConstVec3i(cx, cy, cz))
    }
  }
}
