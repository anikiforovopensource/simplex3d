/*
 * Simplex3dData - Core Module
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
import scala.reflect._
import scala.collection._
import scala.collection.mutable.WrappedArray
import simplex3d.math.{Vec2i, inVec2i, Vec3i, inVec3i}
import simplex3d.data.extension._
import StoreType._
import RawType._


/**
 * @author Aleksey Nikiforov (lex)
 */
abstract class AbstractData[
  @specialized(Int, Float, Double) ReadAs <: WriteAs,
  @specialized(Int, Float, Double) WriteAs
] private[data] (
  shared: AnyRef, prim: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends ReadAbstractData[ReadAs](
  shared, prim, ro,
  off, str
) {

  type PrimitiveSeq <: Contiguous[Format#Component, Raw]

  
  final def buffer() :Raw#Buffer = Util.duplicateBuff(storeType, buff).asInstanceOf[Raw#Buffer]

  override def apply(i: Int) :ReadAs
  def update(i: Int, v: WriteAs)


  private[this] final def putArray(
    index: Int, array: Array[Int], first: Int, count: Int
  ) {
    if (stride == components && buff.isInstanceOf[IntBuffer]) {
      val b = buffer().asInstanceOf[IntBuffer]
      b.position(index + offset)
      b.put(array, first, count)
    }
    else {
      val t = this.asInstanceOf[AbstractData[Int, Int]]
      var i = 0; while (i < count) {
        t(i + index) = array(i + first)
        i += 1
      }
    }
  }
  private[this] final def putArray(
    index: Int, array: Array[Float], first: Int, count: Int
  ) {
    if (stride == components && buff.isInstanceOf[FloatBuffer]) {
      val b = buffer().asInstanceOf[FloatBuffer]
      b.position(index + offset)
      b.put(array, first, count)
    }
    else {
      val t = this.asInstanceOf[AbstractData[Float, Float]]
      var i = 0; while (i < count) {
        t(i + index) = array(i + first)
        i += 1
      }
    }
  }
  private[this] final def putArray(
    index: Int, array: Array[Double], first: Int, count: Int
  ) {
    if (stride == components && buff.isInstanceOf[DoubleBuffer]) {
      val b = buffer().asInstanceOf[DoubleBuffer]
      b.position(index + offset)
      b.put(array, first, count)
    }
    else {
      val t = this.asInstanceOf[AbstractData[Double, Double]]
      var i = 0; while (i < count) {
        t(i + index) = array(i + first)
        i += 1
      }
    }
  }
  private[this] final def putIndexedSeq(
    index: Int, seq: IndexedSeq[WriteAs], first: Int, count: Int
  ) {
    var i = 0; while (i < count) {
      this(index + i) = seq(first + i)
      i += 1
    }
  }
  private[this] final def putSeq(index: Int, seq: Seq[WriteAs], first: Int, count: Int) {
    val iter = seq.iterator
    iter.drop(first)
    val lim = index + count
    var i = index; while (i < lim) {
      this(i) = iter.next
      i += 1
    }
  }

  private[this] final def put(index: Int, src: Seq[WriteAs], srcSize: Int, first: Int, count: Int) {
    var dataCopy = false
    
    if (src.isInstanceOf[ReadDataSeq[_, _]]) {
      val ds = src.asInstanceOf[ReadDataSeq[Format, Raw]]
      
      if ((ds.formatManifest eq formatManifest) || (ds.formatManifest == formatManifest)) {
        putPrimitivesImpl(index, ds.primitives, ds.offset + first*ds.stride, ds.stride, count)
        dataCopy = true
      }
    }
    
    if (!dataCopy) { def seqCopy() {

      if (isReadOnly) throw new ReadOnlyBufferException()
      if (index < 0) throw new IndexOutOfBoundsException("Index = " + index + ", must be greater than or equal to 0.")
      if (first < 0) throw new IndexOutOfBoundsException("First = " + first + ", must be greater than or equal to 0.")
      if (count < 0) throw new IllegalArgumentException("Count = " + count + ", must be greater than or equal to 0.")

      if (index + count > size) {
        if (index > size) throw new IndexOutOfBoundsException("Index = " + index + " exceeds size = " + size + ".")
        else throw new BufferOverflowException()
      }
      if (first + count > srcSize) {
        if (first > srcSize) throw new IndexOutOfBoundsException(
          "First = " + first + " exceeds src size = " + srcSize + "."
        )
        else throw new BufferUnderflowException()
      }

      src match {
        case wrapped: WrappedArray[_] => def cpArray() {
          wrapped.elemManifest match {
            case Manifest.Int =>
              if (accessorManifest != PrimitiveFormat.SInt) throw new ClassCastException(
                "Seq[Int] cannot be cast to Seq[" + accessorManifest + "#Const]."
              )
              putArray(
                index, wrapped.array.asInstanceOf[Array[Int]], first, count
              )
            case Manifest.Float =>
              if (accessorManifest != PrimitiveFormat.RFloat) throw new ClassCastException(
                "Seq[Float] cannot be cast to Seq[" + accessorManifest + "#Const]."
              )
              putArray(
                index, wrapped.array.asInstanceOf[Array[Float]], first, count
              )
            case Manifest.Double =>
              if (accessorManifest != PrimitiveFormat.RDouble) throw new ClassCastException(
                "Seq[Double] cannot be cast to Seq[" + accessorManifest + "#Const]."
              )
              putArray(
                index, wrapped.array.asInstanceOf[Array[Double]], first, count
              )
            case _ =>
              putIndexedSeq(
                index, wrapped.asInstanceOf[IndexedSeq[WriteAs]], first, count
              )
          }
        }; cpArray()
        case is: IndexedSeq[_] => {
          putIndexedSeq(index, is.asInstanceOf[IndexedSeq[WriteAs]], first, count)
        }
        case _ => {
          putSeq(index, src.asInstanceOf[Seq[WriteAs]], first, count)
        }
      }
    }; seqCopy() }
  }

  final def put(index: Int, src: Seq[WriteAs], first: Int, count: Int) {
    put(index, src, src.size, first, count)
  }
  
  final def put(index: Int, src: Seq[WriteAs]) {
    put(index, src, src.size, 0, size)
  }

  final def put(src: Seq[WriteAs]) {
    put(0, src, src.size, 0, size)
  }


  private[data] final def copyGroup(rawType: Int) = {
    (rawType: @switch) match {
      case SByte | UByte => 0
      case SShort => 1
      case UShort => 2
      case SInt | UInt => 3
      case HFloat => 4
      case RFloat => 5
      case RDouble => 6
    }
  }
    
  private[data] final def putPrimitivesImpl(
    index: Int,
    src: inContiguous[Format#Component, simplex3d.data.Raw],
    srcOffset: Int, srcStride: Int, count: Int
  ) {

    if (isReadOnly) throw new ReadOnlyBufferException()
    if (index < 0) throw new IndexOutOfBoundsException("Index = " + index + ", must be greater than or equal to 0.")
    if (srcOffset < 0) throw new IndexOutOfBoundsException(
      "Src offset = " + srcOffset + ", must be greater than or equal to 0."
    )
    if (srcStride < 1) throw new IllegalArgumentException("Src stride = " + srcStride + ", must be greater than 0.")
    if (count < 0) throw new IllegalArgumentException("Count = " + count + ", must be greater than or equal to 0.")

    val destOffset = offset + index*stride
    val srcLim = srcOffset + (count - 1)*srcStride + components

    if (index + count > size) {
      if (index > size) throw new IndexOutOfBoundsException("Index = " + index + " exceeds size = " + size + ".")
      else throw new BufferOverflowException()
    }
    if (srcLim > src.buff.capacity) {
      if (srcOffset > src.size) throw new IndexOutOfBoundsException(
        "Src offset = " + srcOffset + " exceeds src size = " + src.size + "."
      )
      else throw new BufferUnderflowException()
    }

    val noConversion = (
      (rawType == src.rawType) ||
      (!isNormalized && copyGroup(rawType) == copyGroup(src.rawType))
    )

    if (stride == components && srcStride == components && noConversion) {
      def putBuff() {
        val destBuff = buffer()
        val srcBuff = src.readOnlyBuffer()

        destBuff.position(destOffset)
        srcBuff.position(srcOffset)
        srcBuff.limit(srcLim)

        (storeType: @switch) match {
          case ByteStore => destBuff.asInstanceOf[ByteBuffer].put(
            srcBuff.asInstanceOf[ByteBuffer]
          )
          case ShortStore => destBuff.asInstanceOf[ShortBuffer].put(
            srcBuff.asInstanceOf[ShortBuffer]
          )
          case CharStore => destBuff.asInstanceOf[CharBuffer].put(
            srcBuff.asInstanceOf[CharBuffer]
          )
          case IntStore => destBuff.asInstanceOf[IntBuffer].put(
            srcBuff.asInstanceOf[IntBuffer]
          )
          case FloatStore => destBuff.asInstanceOf[FloatBuffer].put(
            srcBuff.asInstanceOf[FloatBuffer]
          )
          case DoubleStore => destBuff.asInstanceOf[DoubleBuffer].put(
            srcBuff.asInstanceOf[DoubleBuffer]
          )
        }
      }
      putBuff()
    }
    else if (noConversion) {
      def copyBuff() {
        (storeType: @switch) match {
          case ByteStore => Util.copyBuffer(
              components,
              buff.asInstanceOf[ByteBuffer], destOffset, stride,
              src.buff.asInstanceOf[ByteBuffer], srcOffset, srcStride, srcLim
            )
          case ShortStore => Util.copyBuffer(
              components,
              buff.asInstanceOf[ShortBuffer], destOffset, stride,
              src.buff.asInstanceOf[ShortBuffer], srcOffset, srcStride, srcLim
            )
          case CharStore => Util.copyBuffer(
              components,
              buff.asInstanceOf[CharBuffer], destOffset, stride,
              src.buff.asInstanceOf[CharBuffer], srcOffset, srcStride, srcLim
            )
          case IntStore => Util.copyBuffer(
              components,
              buff.asInstanceOf[IntBuffer], destOffset, stride,
              src.buff.asInstanceOf[IntBuffer], srcOffset, srcStride, srcLim
            )
          case FloatStore => Util.copyBuffer(
              components,
              buff.asInstanceOf[FloatBuffer], destOffset, stride,
              src.buff.asInstanceOf[FloatBuffer], srcOffset, srcStride, srcLim
            )
          case DoubleStore => Util.copyBuffer(
              components,
              buff.asInstanceOf[DoubleBuffer], destOffset, stride,
              src.buff.asInstanceOf[DoubleBuffer], srcOffset, srcStride, srcLim
            )
        }
      }
      copyBuff()
    }
    else {
      def copyPrimSeq() {
        primitives.formatManifest match {
          case PrimitiveFormat.SInt => Util.copySeqInt(
              components,
              primitives.asInstanceOf[Contiguous[SInt, _]], destOffset, stride,
              src.asInstanceOf[inContiguous[SInt, _]], srcOffset, srcStride, srcLim
            )
          case PrimitiveFormat.RFloat => Util.copySeqFloat(
              components,
              primitives.asInstanceOf[Contiguous[RFloat, _]], destOffset, stride,
              src.asInstanceOf[inContiguous[RFloat, _]], srcOffset, srcStride, srcLim
            )
          case PrimitiveFormat.RDouble => Util.copySeqDouble(
              components,
              primitives.asInstanceOf[Contiguous[RDouble, _]], destOffset, stride,
              src.asInstanceOf[inContiguous[RDouble, _]], srcOffset, srcStride, srcLim
            )
        }
      }
      copyPrimSeq()
    }
  }
  
  
  /** This will copy a 2d sub image from the source sequence into this object.
   */
  final def put2d(
    dimensions: inVec2i, offset: inVec2i,
    src: IndexedSeq[WriteAs], srcDimensions: inVec2i
  ) {
    put2d(
      dimensions, offset,
      src, srcDimensions, Vec2i.Zero,
      srcDimensions
    )
  }
  
  /** This will copy a 2d sub image from the source sequence into this object.
   */
  final def put2d(
    dimensions: inVec2i, offset: inVec2i,
    src: IndexedSeq[WriteAs], srcDimensions: inVec2i, srcOffset: inVec2i,
    copyDimensions: inVec2i
  ) {
    var contiguousCopy = false
    
    if (this.isInstanceOf[ContiguousSrc] && src.isInstanceOf[ContiguousSrc]) {
      val srcFormatManifest = src.asInstanceOf[ReadAbstractData[_]].formatManifest
      
      if ((formatManifest eq srcFormatManifest) || (formatManifest == srcFormatManifest)) {
        put2dImpl(
          dimensions, offset,
          src.asInstanceOf[inContiguous[Format, simplex3d.data.Raw]], srcDimensions, srcOffset,
          copyDimensions
        )
        
        contiguousCopy = true
      }
    }
    
    if (!contiguousCopy) {
      def seqCopy() {

        checkArgs(
          size, dimensions, offset,
          src.size, srcDimensions, srcOffset,
          copyDimensions
        )

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
      seqCopy()
    }
  }
  
  private[this] final def checkArgs(
    destSize: Int, destDims: inVec2i, destOffset: inVec2i,
    srcSize: Int, srcDims: inVec2i, srcOffset: inVec2i,
    copyDims: inVec2i
  ) {
    if (destDims.x < 0 || destDims.y < 0) throw new IllegalArgumentException(
      "Dimensions = " + destDims + " contain negative components."
    )
    if (destOffset.x < 0 || destOffset.y < 0) throw new IllegalArgumentException(
      "Offset = " + destOffset + " has negative components."
    )
    if (destDims.x*destDims.y > destSize) throw new IllegalArgumentException(
      "Dimensions = " + destDims + " exceed data size = " + destSize + "."
    )
    if (destOffset.x + copyDims.x > destDims.x || destOffset.y + copyDims.y > destDims.y)
      throw new IllegalArgumentException(
        "Dest region from " + destOffset + " to " + (destOffset + copyDims) +
        " lies outside dimensions = " + destDims + "."
      )
    
    if (srcDims.x < 0 || srcDims.y < 0) throw new IllegalArgumentException(
      "Src dimensions = " + srcDims + " contain negative components."
    )
    if (srcOffset.x < 0 || srcOffset.y < 0) throw new IllegalArgumentException(
      "Src offset = " + srcOffset + " has negative components."
    )
    if (srcDims.x*srcDims.y > srcSize) throw new IllegalArgumentException(
      "Src dimensions = " + srcDims + " exceed src size = " + srcSize + "."
    )
    if (srcOffset.x + copyDims.x > srcDims.x || srcOffset.y + copyDims.y > srcDims.y)
      throw new IllegalArgumentException(
        "Src regioun from " + srcOffset + " to " + (srcOffset + copyDims) +
        " lies outside src dimensions = " + srcDims + "."
      )
    
    if (copyDims.x < 0 || copyDims.y < 0) throw new IllegalArgumentException(
      "Copy region dimensions = " + copyDims + " contain negative components."
    )
  }
  
  private[data] final def put2dImpl(
    dimensions: inVec2i, offset: inVec2i,
    src: inContiguous[Format, simplex3d.data.Raw], srcDimensions: inVec2i, srcOffset: inVec2i,
    copyDimensions: inVec2i
  ) {
    if (isReadOnly) throw new ReadOnlyBufferException()
    
    checkArgs(
      size, dimensions, offset,
      src.size, srcDimensions, srcOffset,
      copyDimensions
    )
    
    if (
      offset.x == 0 && srcOffset.x == 0 &&
      dimensions.x == copyDimensions.x && srcDimensions.x == copyDimensions.x
    ) {
      putPrimitivesImpl(
        offset.y*dimensions.x,
        src.primitives, src.offset + srcOffset.y*srcDimensions.x*src.stride, src.stride,
        copyDimensions.x*copyDimensions.y
      )
    }
    else {
      
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
  }
  
  
  /** This will copy a 3d sub image from the source sequence into this object.
   */
  final def put3d(
    dimensions: inVec3i, offset: inVec3i,
    src: IndexedSeq[WriteAs], srcDimensions: inVec3i
  ) {
    put3d(
      dimensions, offset,
      src, srcDimensions, Vec3i.Zero,
      srcDimensions
    )
  }
  
  /** This will copy a 3d sub image from the source sequence into this object.
   */
  final def put3d(
    dimensions: inVec3i, offset: inVec3i,
    src: IndexedSeq[WriteAs], srcDimensions: inVec3i, srcOffset: inVec3i,
    copyDimensions: inVec3i
  ) {
    var contiguousCopy = false
    
    if (this.isInstanceOf[ContiguousSrc] && src.isInstanceOf[ContiguousSrc]) {
      val srcFormatManifest = src.asInstanceOf[ReadAbstractData[_]].formatManifest
      
      if ((formatManifest eq srcFormatManifest) || (formatManifest == srcFormatManifest)) {
        put3dImpl(
          dimensions, offset,
          src.asInstanceOf[inContiguous[Format, simplex3d.data.Raw]], srcDimensions, srcOffset,
          copyDimensions
        )
        
        contiguousCopy = true
      }
    }
    
    if (!contiguousCopy) {
      def seqCopy() {
      
        checkArgs(
          size, dimensions, offset,
          src.size, srcDimensions, srcOffset,
          copyDimensions
        )

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
      seqCopy()
    }
  }
  
  private[this] final def checkArgs(
    destSize: Int, destDims: inVec3i, destOffset: inVec3i,
    srcSize: Int, srcDims: inVec3i, srcOffset: inVec3i,
    copyDims: inVec3i
  ) {
    if (destDims.x < 0 || destDims.y < 0 || destDims.z < 0) throw new IllegalArgumentException(
      "Dimensions = " + destDims + " contain negative components."
    )
    if (destOffset.x < 0 || destOffset.y < 0 || destOffset.z < 0) throw new IllegalArgumentException(
      "Offset = " + destOffset + " has negative components."
    )
    if (destDims.x*destDims.y*destDims.z > destSize) throw new IllegalArgumentException(
      "Dimensions = " + destDims + " exceed data size = " + destSize + "."
    )
    if (
      destOffset.x + copyDims.x > destDims.x ||
      destOffset.y + copyDims.y > destDims.y ||
      destOffset.z + copyDims.z > destDims.z
    )
      throw new IllegalArgumentException(
        "Dest region from " + destOffset + " to " + (destOffset + copyDims) +
        " lies outside dimensions = " + destDims + "."
      )
    
    if (srcDims.x < 0 || srcDims.y < 0 || srcDims.z < 0) throw new IllegalArgumentException(
      "Src dimensions = " + srcDims + " contain negative components."
    )
    if (srcOffset.x < 0 || srcOffset.y < 0 || srcOffset.z < 0) throw new IllegalArgumentException(
      "Src offset = " + srcOffset + " has negative components."
    )
    if (srcDims.x*srcDims.y*srcDims.z > srcSize) throw new IllegalArgumentException(
      "Src dimensions = " + srcDims + " exceed src size = " + srcSize + "."
    )
    if (
      srcOffset.x + copyDims.x > srcDims.x ||
      srcOffset.y + copyDims.y > srcDims.y ||
      srcOffset.z + copyDims.z > srcDims.z
    )
      throw new IllegalArgumentException(
        "Src regioun from " + srcOffset + " to " + (srcOffset + copyDims) +
        " lies outside src dimensions = " + srcDims + "."
      )
    
    if (copyDims.x < 0 || copyDims.y < 0 || copyDims.z < 0) throw new IllegalArgumentException(
      "Copy region dimensions = " + copyDims + " contain negative components."
    )
  }
  
  private[data] final def put3dImpl(
    dimensions: inVec3i, offset: inVec3i,
    src: inContiguous[Format, simplex3d.data.Raw], srcDimensions: inVec3i, srcOffset: inVec3i,
    copyDimensions: inVec3i
  ) {
    if (isReadOnly) throw new ReadOnlyBufferException()
    
    checkArgs(
      size, dimensions, offset,
      src.size, srcDimensions, srcOffset,
      copyDimensions
    )
    
    if (
      offset.x == 0 && offset.y == 0 &&
      srcOffset.x == 0 && srcOffset.y == 0 &&
      dimensions.x == copyDimensions.x && dimensions.y == copyDimensions.y &&
      srcDimensions.x == copyDimensions.x && srcDimensions.y == copyDimensions.y
    ) {
      putPrimitivesImpl(
        offset.z*dimensions.x*dimensions.y,
        src.primitives, src.offset + srcOffset.z*srcDimensions.x*srcDimensions.y*src.stride, src.stride,
        copyDimensions.x*copyDimensions.y*copyDimensions.z
      )
    }
    else {
      
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
  }
}
