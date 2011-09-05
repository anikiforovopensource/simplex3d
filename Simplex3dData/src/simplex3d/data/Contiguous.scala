/*
 * Simplex3d, CoreData module
 * Copyright (C) 2010-2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dData.
 *
 * Simplex3dData is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dData is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.data

import java.nio._
import scala.annotation._
import simplex3d.math._
import StoreType._


/**
 * @author Aleksey Nikiforov (lex)
 */
trait ReadContiguous[F <: Format, +R <: Raw]
extends ReadDataSeq[F, R] with ContiguousSrc {
  type Read <: ReadContiguous[F, R]
}

trait Contiguous[F <: Format, +R <: Raw]
extends DataSeq[F, R] with ReadContiguous[F, R] {
  
  /** This will copy a 2d sub image from the source Data object into this object.
   */
  def put[A <: F#Accessor](
    dimensions: inVec2i, offset: inVec2i,
    src: inData[A], srcDimensions: inVec2i
  ) {
    put(
      dimensions, offset,
      src, srcDimensions, Vec2i.Zero,
      srcDimensions
    )
  }
  
  /** This will copy a 2d sub image from the source Data object into this object.
   */
  def put[A <: F#Accessor](
    dimensions: inVec2i, offset: inVec2i,
    src: inData[A], srcDimensions: inVec2i, srcOffset: inVec2i,
    copyDimensions: inVec2i
  ) {
    if ((accessorManifest ne src.accessorManifest) && (accessorManifest != src.accessorManifest)) {
      throw new ClassCastException(
        "ReadData[" + src.accessorManifest + "] cannot be cast to ReadData[" + accessorManifest + "]."
      )
    }
    
    if (dimensions.x*dimensions.y > size) throw new IllegalArgumentException(
      "Specified dimensions exceed data size."
    )
    if (offset.x >= dimensions.x || offset.y >= dimensions.y) throw new IllegalArgumentException(
      "Specified offset exceeds dimensions."
    )
    if (offset.x + copyDimensions.x > dimensions.x || offset.y + copyDimensions.y > dimensions.y)
      throw new IllegalArgumentException(
        "Specified destination regioun lies outside specified dimensions."
      )
    
    if (srcDimensions.x*srcDimensions.y > src.size) throw new IllegalArgumentException(
      "Specified source dimensions exceed source size."
    )
    if (srcOffset.x >= srcDimensions.x || srcOffset.y >= srcDimensions.y) throw new IllegalArgumentException(
      "Specified source offset exceeds source dimensions."
    )
    if (srcOffset.x + copyDimensions.x > srcDimensions.x || srcOffset.y + copyDimensions.y > srcDimensions.y)
      throw new IllegalArgumentException(
        "Specified source regioun lies outside specified source dimensions."
      )
    
    
    if (
      offset.x == 0 && srcOffset.x == 0 &&
      dimensions.x == copyDimensions.x && srcDimensions.x == copyDimensions.x
    ) {
      put(offset.y*dimensions.x, src, srcOffset.y*srcDimensions.x, copyDimensions.x*copyDimensions.y)
    }
    else if (
      (formatManifest eq src.formatManifest) || (formatManifest == src.formatManifest)
    ) {
      
      val noConversion = (
        (rawType == src.rawType) ||
        (!isNormalized && copyGroup(rawType) == copyGroup(src.rawType))
      )

      if (noConversion) {
        def copyBuff() {
          val destBuff = buffer()
          val srcBuff = src.readOnlyBuffer()

          (storeType: @switch) match {
            case ByteStore => Util.copyBuffer2d(
              components,
              destBuff.asInstanceOf[ByteBuffer], dimensions.x, offset.x, offset.y,
              srcBuff.asInstanceOf[ByteBuffer], srcDimensions.x, srcOffset.x, srcOffset.y,
              copyDimensions.x, copyDimensions.y
            )
            case ShortStore => Util.copyBuffer2d(
              components,
              destBuff.asInstanceOf[ShortBuffer], dimensions.x, offset.x, offset.y,
              srcBuff.asInstanceOf[ShortBuffer], srcDimensions.x, srcOffset.x, srcOffset.y,
              copyDimensions.x: Int, copyDimensions.y: Int
            )
            case CharStore => Util.copyBuffer2d(
              components,
              destBuff.asInstanceOf[CharBuffer], dimensions.x, offset.x, offset.y,
              srcBuff.asInstanceOf[CharBuffer], srcDimensions.x, srcOffset.x, srcOffset.y,
              copyDimensions.x: Int, copyDimensions.y: Int
            )
            case IntStore => Util.copyBuffer2d(
              components,
              destBuff.asInstanceOf[IntBuffer], dimensions.x, offset.x, offset.y,
              srcBuff.asInstanceOf[IntBuffer], srcDimensions.x, srcOffset.x, srcOffset.y,
              copyDimensions.x: Int, copyDimensions.y: Int
            )
            case FloatStore => Util.copyBuffer2d(
              components,
              destBuff.asInstanceOf[FloatBuffer], dimensions.x, offset.x, offset.y,
              srcBuff.asInstanceOf[FloatBuffer], srcDimensions.x, srcOffset.x, srcOffset.y,
              copyDimensions.x: Int, copyDimensions.y: Int
            )
            case DoubleStore => Util.copyBuffer2d(
              components,
              destBuff.asInstanceOf[DoubleBuffer], dimensions.x, offset.x, offset.y,
              srcBuff.asInstanceOf[DoubleBuffer], srcDimensions.x, srcOffset.x, srcOffset.y,
              copyDimensions.x: Int, copyDimensions.y: Int
            )
          }
        }
        copyBuff()
      }
      else {
        def copyPrimSeq() {
          primitives.formatManifest match {
            case PrimitiveFormat.SInt => Util.copySeqInt2d(
                components,
                this.primitives.asInstanceOf[Contiguous[SInt, _]], dimensions.x, offset.x, offset.y,
                src.primitives.asInstanceOf[inContiguous[SInt, _]], srcDimensions.x, srcOffset.x, srcOffset.y,
                copyDimensions.x: Int, copyDimensions.y: Int
              )
            case PrimitiveFormat.RFloat => Util.copySeqFloat2d(
                components,
                this.primitives.asInstanceOf[Contiguous[RFloat, _]], dimensions.x, offset.x, offset.y,
                src.primitives.asInstanceOf[inContiguous[RFloat, _]], srcDimensions.x, srcOffset.x, srcOffset.y,
                copyDimensions.x: Int, copyDimensions.y: Int
              )
            case PrimitiveFormat.RDouble => Util.copySeqDouble2d(
                components,
                this.primitives.asInstanceOf[Contiguous[RDouble, _]], dimensions.x, offset.x, offset.y,
                src.primitives.asInstanceOf[inContiguous[RDouble, _]], srcDimensions.x, srcOffset.x, srcOffset.y,
                copyDimensions.x: Int, copyDimensions.y: Int
              )
          }
        }
        copyPrimSeq()
      }
    }
    else {
      def convertCopy() {
        var y = 0; while (y < copyDimensions.y) {
          val td = offset.x + (y + offset.y)*dimensions.x
          val ts = srcOffset.x + (y + srcOffset.y)*srcDimensions.x
          
          var x = 0; while (x < copyDimensions.x) {
            this(x + td) = src(x + ts)
            
            x += 1
          }
          y += 1
        }
      }
      convertCopy()
    }
  }
  
  
  /** This will copy a 3d sub image from the source Data object into this object.
   */
  def put[A <: F#Accessor](
    dimensions: inVec3i, offset: inVec3i,
    src: inData[A], srcDimensions: inVec3i
  ) {
    put(
      dimensions, offset,
      src, srcDimensions, Vec3i.Zero,
      srcDimensions
    )
  }
  
  /** This will copy a 3d sub image from the source Data object into this object.
   */
  def put[A <: F#Accessor](
    dimensions: inVec3i, offset: inVec3i,
    src: inData[A], srcDimensions: inVec3i, srcOffset: inVec3i,
    copyDimensions: inVec3i
  ) {
    if ((accessorManifest ne src.accessorManifest) && (accessorManifest != src.accessorManifest)) {
      throw new ClassCastException(
        "ReadData[" + src.accessorManifest + "] cannot be cast to ReadData[" + accessorManifest + "]."
      )
    }
    
    if (dimensions.x*dimensions.y*dimensions.z > size) throw new IllegalArgumentException(
      "Specified dimensions exceed data size."
    )
    if (offset.x >= dimensions.x || offset.y >= dimensions.y || offset.z >= dimensions.z)
      throw new IllegalArgumentException(
        "Specified offset exceeds dimensions."
      )
    if (
      offset.x + copyDimensions.x > dimensions.x ||
      offset.y + copyDimensions.y > dimensions.y ||
      offset.z + copyDimensions.z > dimensions.z
    )
      throw new IllegalArgumentException(
        "Specified destination regioun lies outside specified dimensions."
      )
    
    if (srcDimensions.x*srcDimensions.y*srcDimensions.z > src.size) throw new IllegalArgumentException(
      "Specified source dimensions exceed source size."
    )
    if (srcOffset.x >= srcDimensions.x || srcOffset.y >= srcDimensions.y || srcOffset.z >= srcDimensions.z)
      throw new IllegalArgumentException(
        "Specified source offset exceeds source dimensions."
      )
    if (
      srcOffset.x + copyDimensions.x > srcDimensions.x ||
      srcOffset.y + copyDimensions.y > srcDimensions.y ||
      srcOffset.z + copyDimensions.z > srcDimensions.z
    )
      throw new IllegalArgumentException(
        "Specified source regioun lies outside specified source dimensions."
      )
    
    
    if (
      offset.x == 0 && offset.y == 0 &&
      srcOffset.x == 0 && srcOffset.y == 0 &&
      dimensions.x == copyDimensions.x && dimensions.y == copyDimensions.y &&
      srcDimensions.x == copyDimensions.x && srcDimensions.y == copyDimensions.y
    ) {
      put(
        offset.z*dimensions.x*dimensions.y,
        src,
        srcOffset.z*srcDimensions.x*srcDimensions.y,
        copyDimensions.x*copyDimensions.y*copyDimensions.z
      )
    }
    else if (
      (formatManifest eq src.formatManifest) || (formatManifest == src.formatManifest)
    ) {
      
      val noConversion = (
        (rawType == src.rawType) ||
        (!isNormalized && copyGroup(rawType) == copyGroup(src.rawType))
      )

      if (noConversion) {
        def copyBuff() {
          val destBuff = buffer()
          val srcBuff = src.readOnlyBuffer()

          (storeType: @switch) match {
            case ByteStore => Util.copyBuffer3d(
              components,
              destBuff.asInstanceOf[ByteBuffer], dimensions.x, dimensions.y,
              offset.x, offset.y, offset.z,
              srcBuff.asInstanceOf[ByteBuffer], srcDimensions.x, srcDimensions.y,
              srcOffset.x, srcOffset.y, srcOffset.z,
              copyDimensions.x, copyDimensions.y, copyDimensions.z
            )
            case ShortStore => Util.copyBuffer3d(
              components,
              destBuff.asInstanceOf[ShortBuffer], dimensions.x, dimensions.y,
              offset.x, offset.y, offset.z,
              srcBuff.asInstanceOf[ShortBuffer], srcDimensions.x, srcDimensions.y,
              srcOffset.x, srcOffset.y, srcOffset.z,
              copyDimensions.x, copyDimensions.y, copyDimensions.z
            )
            case CharStore => Util.copyBuffer3d(
              components,
              destBuff.asInstanceOf[CharBuffer], dimensions.x, dimensions.y,
              offset.x, offset.y, offset.z,
              srcBuff.asInstanceOf[CharBuffer], srcDimensions.x, srcDimensions.y,
              srcOffset.x, srcOffset.y, srcOffset.z,
              copyDimensions.x, copyDimensions.y, copyDimensions.z
            )
            case IntStore => Util.copyBuffer3d(
              components,
              destBuff.asInstanceOf[IntBuffer], dimensions.x, dimensions.y,
              offset.x, offset.y, offset.z,
              srcBuff.asInstanceOf[IntBuffer], srcDimensions.x, srcDimensions.y,
              srcOffset.x, srcOffset.y, srcOffset.z,
              copyDimensions.x, copyDimensions.y, copyDimensions.z
            )
            case FloatStore => Util.copyBuffer3d(
              components,
              destBuff.asInstanceOf[FloatBuffer], dimensions.x, dimensions.y,
              offset.x, offset.y, offset.z,
              srcBuff.asInstanceOf[FloatBuffer], srcDimensions.x, srcDimensions.y,
              srcOffset.x, srcOffset.y, srcOffset.z,
              copyDimensions.x, copyDimensions.y, copyDimensions.z
            )
            case DoubleStore => Util.copyBuffer3d(
              components,
              destBuff.asInstanceOf[DoubleBuffer], dimensions.x, dimensions.y,
              offset.x, offset.y, offset.z,
              srcBuff.asInstanceOf[DoubleBuffer], srcDimensions.x, srcDimensions.y,
              srcOffset.x, srcOffset.y, srcOffset.z,
              copyDimensions.x, copyDimensions.y, copyDimensions.z
            )
          }
        }
        copyBuff()
      }
      else {
        def copyPrimSeq() {
          primitives.formatManifest match {
            case PrimitiveFormat.SInt => Util.copySeqInt3d(
                components,
                this.primitives.asInstanceOf[Contiguous[SInt, _]], dimensions.x, dimensions.y,
                offset.x, offset.y, offset.z,
                src.primitives.asInstanceOf[inContiguous[SInt, _]], srcDimensions.x, srcDimensions.y,
                srcOffset.x, srcOffset.y, srcOffset.z,
                copyDimensions.x, copyDimensions.y, copyDimensions.z
              )
            case PrimitiveFormat.RFloat => Util.copySeqFloat3d(
                components,
                this.primitives.asInstanceOf[Contiguous[RFloat, _]], dimensions.x, dimensions.y,
                offset.x, offset.y, offset.z,
                src.primitives.asInstanceOf[inContiguous[RFloat, _]], srcDimensions.x, srcDimensions.y,
                srcOffset.x, srcOffset.y, srcOffset.z,
                copyDimensions.x, copyDimensions.y, copyDimensions.z
              )
            case PrimitiveFormat.RDouble => Util.copySeqDouble3d(
                components,
                this.primitives.asInstanceOf[Contiguous[RDouble, _]], dimensions.x, dimensions.y,
                offset.x, offset.y, offset.z,
                src.primitives.asInstanceOf[inContiguous[RDouble, _]], srcDimensions.x, srcDimensions.y,
                srcOffset.x, srcOffset.y, srcOffset.z,
                copyDimensions.x, copyDimensions.y, copyDimensions.z
              )
          }
        }
        copyPrimSeq()
      }
    }
    else {
      def convertCopy() {
        val dmz = dimensions.x*dimensions.y
        val smz = srcDimensions.x*srcDimensions.y
        
        var z = 0; while (z < copyDimensions.z) {
          val dtz = offset.x + (z + offset.z)*dmz
          val stz = srcOffset.x + (z + srcOffset.z)*smz
          
          var y = 0; while (y < copyDimensions.y) {
            val dty = (y + offset.y)*dimensions.x + dtz
            val sty = (y + srcOffset.y)*srcDimensions.x + stz

            var x = 0; while (x < copyDimensions.x) {
              this(x + dty) = src(x + sty)

              x += 1
            }
            y += 1
          }
          z += 1
        }
      }
      convertCopy()
    }
  }
}


object ReadContiguous {
  def apply[F <: Format, R <: Defined](dc: ReadContiguous[_, R])(
    implicit composition: CompositionFactory[F, _ >: R], primitives: PrimitiveFactory[F#Component, R]
  ) :ReadContiguous[F, R] = {
    val res = dc match {
      case d: DataArray[_, _] => composition.mkDataArray(primitives.mkDataArray(dc.sharedStorage.asInstanceOf[R#Array]))
      case d: DataBuffer[_, _] => composition.mkDataBuffer(primitives.mkDataBuffer(dc.sharedBuffer))
    }
    if (dc.isReadOnly) res.asReadOnly() else res
  }
}

object Contiguous {
  def apply[F <: Format, R <: Defined](dc: Contiguous[_, R])(
    implicit composition: CompositionFactory[F, _ >: R], primitives: PrimitiveFactory[F#Component, R]
  ) :Contiguous[F, R] = {
    if (dc.isReadOnly) throw new IllegalArgumentException(
      "The Sequence must not be read-only."
    )
    dc match {
      case d: DataArray[_, _] => composition.mkDataArray(primitives.mkDataArray(dc.sharedStorage.asInstanceOf[R#Array]))
      case d: DataBuffer[_, _] => composition.mkDataBuffer(primitives.mkDataBuffer(dc.sharedBuffer))
    }
  }
}
