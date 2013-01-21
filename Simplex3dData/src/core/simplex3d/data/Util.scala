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
import java.util.Arrays
import scala.annotation._
import StoreEnum._


// An empty class to make -Xno-forwarders work
private[data] class Util


/**
 * @author Aleksey Nikiforov (lex)
 */
private[data] object Util {

  final val emptyByte = new Array[Byte](0)
  final val emptyChar = new Array[Char](0)
  final val emptyShort = new Array[Short](0)
  final val emptyInt = new Array[Int](0)
  final val emptyFloat = new Array[Float](0)
  final val emptyDouble = new Array[Double](0)

  final def wrapBuffer(storeEnum: Int, buffer: ByteBuffer) :Buffer = {
    (storeEnum: @switch) match {
      case ByteStore => buffer
      case ShortStore => buffer.asShortBuffer()
      case CharStore => buffer.asCharBuffer()
      case IntStore => buffer.asIntBuffer()
      case FloatStore => buffer.asFloatBuffer()
      case DoubleStore => buffer.asDoubleBuffer()
    }
  }
  final def wrapArray(storeEnum: Int, array: AnyRef) :Buffer = {
    (storeEnum: @switch) match {
      case ByteStore => ByteBuffer.wrap(array.asInstanceOf[Array[Byte]]).order(ByteOrder.nativeOrder)
      case ShortStore => ShortBuffer.wrap(array.asInstanceOf[Array[Short]])
      case CharStore => CharBuffer.wrap(array.asInstanceOf[Array[Char]])
      case IntStore => IntBuffer.wrap(array.asInstanceOf[Array[Int]])
      case FloatStore => FloatBuffer.wrap(array.asInstanceOf[Array[Float]])
      case DoubleStore => DoubleBuffer.wrap(array.asInstanceOf[Array[Double]])
    }
  }

  final def duplicateBuff(storeEnum: Int, buffer: AnyRef) :Buffer = {
    (storeEnum: @switch) match {
      case ByteStore => buffer.asInstanceOf[ByteBuffer].duplicate().order(ByteOrder.nativeOrder)
      case ShortStore => buffer.asInstanceOf[ShortBuffer].duplicate()
      case CharStore => buffer.asInstanceOf[CharBuffer].duplicate()
      case IntStore => buffer.asInstanceOf[IntBuffer].duplicate()
      case FloatStore => buffer.asInstanceOf[FloatBuffer].duplicate()
      case DoubleStore => buffer.asInstanceOf[DoubleBuffer].duplicate()
    }
  }

  final def readOnlyBuff(storeEnum: Int, buffer: AnyRef) :Buffer = {
    (storeEnum: @switch) match {
      case ByteStore => buffer.asInstanceOf[ByteBuffer].asReadOnlyBuffer().order(ByteOrder.nativeOrder)
      case ShortStore => buffer.asInstanceOf[ShortBuffer].asReadOnlyBuffer()
      case CharStore => buffer.asInstanceOf[CharBuffer].asReadOnlyBuffer()
      case IntStore => buffer.asInstanceOf[IntBuffer].asReadOnlyBuffer()
      case FloatStore => buffer.asInstanceOf[FloatBuffer].asReadOnlyBuffer()
      case DoubleStore => buffer.asInstanceOf[DoubleBuffer].asReadOnlyBuffer()
    }
  }

  
  final def copyBuffer(
    components: Int,
    dest: ByteBuffer, destOffset: Int, destStride: Int,
    src: ByteBuffer, srcOffset: Int, srcStride: Int, srcLim: Int
  ) {
    (components: @switch) match {
      
      case 1 => def cp1() {
          var desti = destOffset
          var srci = srcOffset

          while (srci < srcLim) {
            dest.put(desti, src.get(srci))
            desti += destStride
            srci += srcStride
          }
        }; cp1()
      
      case 2 => def cp2() {
          var desti = destOffset
          var srci = srcOffset

          while (srci < srcLim - 1) {
            dest.put(desti, src.get(srci))
            dest.put(desti + 1, src.get(srci + 1))
            desti += destStride
            srci += srcStride
          }
        }; cp2()
      
      case 3 =>  def cp3() {
          var desti = destOffset
          var srci = srcOffset

          while (srci < srcLim - 2) {
            dest.put(desti, src.get(srci))
            dest.put(desti + 1, src.get(srci + 1))
            dest.put(desti + 2, src.get(srci + 2))
            desti += destStride
            srci += srcStride
          }
        }; cp3()
      
      case 4 =>  def cp4() {
          var desti = destOffset
          var srci = srcOffset

          while (srci < srcLim - 3) {
            dest.put(desti, src.get(srci))
            dest.put(desti + 1, src.get(srci + 1))
            dest.put(desti + 2, src.get(srci + 2))
            dest.put(desti + 3, src.get(srci + 3))
            desti += destStride
            srci += srcStride
          }
        }; cp4()
      
      case _ =>  def cpAny() {
          var desti = destOffset
          var srci = srcOffset

          while (srci <= srcLim - components) {

            var j = 0; while (j < components) {
              dest.put(desti + j, src.get(srci + j))
              j += 1
            }

            desti += destStride
            srci += srcStride
          }
        }; cpAny()
    }
  }
  
  
  final def copyBuffer(
    components: Int,
    dest: ShortBuffer, destOffset: Int, destStride: Int,
    src: ShortBuffer, srcOffset: Int, srcStride: Int, srcLim: Int
  ) {
    (components: @switch) match {
      
      case 1 => def cp1() {
          var desti = destOffset
          var srci = srcOffset

          while (srci < srcLim) {
            dest.put(desti, src.get(srci))
            desti += destStride
            srci += srcStride
          }
        }; cp1()
      
      case 2 => def cp2() {
          var desti = destOffset
          var srci = srcOffset

          while (srci < srcLim - 1) {
            dest.put(desti, src.get(srci))
            dest.put(desti + 1, src.get(srci + 1))
            desti += destStride
            srci += srcStride
          }
        }; cp2()
      
      case 3 =>  def cp3() {
          var desti = destOffset
          var srci = srcOffset

          while (srci < srcLim - 2) {
            dest.put(desti, src.get(srci))
            dest.put(desti + 1, src.get(srci + 1))
            dest.put(desti + 2, src.get(srci + 2))
            desti += destStride
            srci += srcStride
          }
        }; cp3()
      
      case 4 =>  def cp4() {
          var desti = destOffset
          var srci = srcOffset

          while (srci < srcLim - 3) {
            dest.put(desti, src.get(srci))
            dest.put(desti + 1, src.get(srci + 1))
            dest.put(desti + 2, src.get(srci + 2))
            dest.put(desti + 3, src.get(srci + 3))
            desti += destStride
            srci += srcStride
          }
        }; cp4()
      
      case _ =>  def cpAny() {
          var desti = destOffset
          var srci = srcOffset

          while (srci <= srcLim - components) {

            var j = 0; while (j < components) {
              dest.put(desti + j, src.get(srci + j))
              j += 1
            }

            desti += destStride
            srci += srcStride
          }
        }; cpAny()
    }
  }
  
  
  final def copyBuffer(
    components: Int,
    dest: CharBuffer, destOffset: Int, destStride: Int,
    src: CharBuffer, srcOffset: Int, srcStride: Int, srcLim: Int
  ) {
    (components: @switch) match {
      
      case 1 => def cp1() {
          var desti = destOffset
          var srci = srcOffset

          while (srci < srcLim) {
            dest.put(desti, src.get(srci))
            desti += destStride
            srci += srcStride
          }
        }; cp1()
      
      case 2 => def cp2() {
          var desti = destOffset
          var srci = srcOffset

          while (srci < srcLim - 1) {
            dest.put(desti, src.get(srci))
            dest.put(desti + 1, src.get(srci + 1))
            desti += destStride
            srci += srcStride
          }
        }; cp2()
      
      case 3 =>  def cp3() {
          var desti = destOffset
          var srci = srcOffset

          while (srci < srcLim - 2) {
            dest.put(desti, src.get(srci))
            dest.put(desti + 1, src.get(srci + 1))
            dest.put(desti + 2, src.get(srci + 2))
            desti += destStride
            srci += srcStride
          }
        }; cp3()
      
      case 4 =>  def cp4() {
          var desti = destOffset
          var srci = srcOffset

          while (srci < srcLim - 3) {
            dest.put(desti, src.get(srci))
            dest.put(desti + 1, src.get(srci + 1))
            dest.put(desti + 2, src.get(srci + 2))
            dest.put(desti + 3, src.get(srci + 3))
            desti += destStride
            srci += srcStride
          }
        }; cp4()
      
      case _ =>  def cpAny() {
          var desti = destOffset
          var srci = srcOffset

          while (srci <= srcLim - components) {

            var j = 0; while (j < components) {
              dest.put(desti + j, src.get(srci + j))
              j += 1
            }

            desti += destStride
            srci += srcStride
          }
        }; cpAny()
    }
  }
  
  
  final def copyBuffer(
    components: Int,
    dest: IntBuffer, destOffset: Int, destStride: Int,
    src: IntBuffer, srcOffset: Int, srcStride: Int, srcLim: Int
  ) {
    (components: @switch) match {
      
      case 1 => def cp1() {
          var desti = destOffset
          var srci = srcOffset

          while (srci < srcLim) {
            dest.put(desti, src.get(srci))
            desti += destStride
            srci += srcStride
          }
        }; cp1()
      
      case 2 => def cp2() {
          var desti = destOffset
          var srci = srcOffset

          while (srci < srcLim - 1) {
            dest.put(desti, src.get(srci))
            dest.put(desti + 1, src.get(srci + 1))
            desti += destStride
            srci += srcStride
          }
        }; cp2()
      
      case 3 =>  def cp3() {
          var desti = destOffset
          var srci = srcOffset

          while (srci < srcLim - 2) {
            dest.put(desti, src.get(srci))
            dest.put(desti + 1, src.get(srci + 1))
            dest.put(desti + 2, src.get(srci + 2))
            desti += destStride
            srci += srcStride
          }
        }; cp3()
      
      case 4 =>  def cp4() {
          var desti = destOffset
          var srci = srcOffset

          while (srci < srcLim - 3) {
            dest.put(desti, src.get(srci))
            dest.put(desti + 1, src.get(srci + 1))
            dest.put(desti + 2, src.get(srci + 2))
            dest.put(desti + 3, src.get(srci + 3))
            desti += destStride
            srci += srcStride
          }
        }; cp4()
      
      case _ =>  def cpAny() {
          var desti = destOffset
          var srci = srcOffset

          while (srci <= srcLim - components) {

            var j = 0; while (j < components) {
              dest.put(desti + j, src.get(srci + j))
              j += 1
            }

            desti += destStride
            srci += srcStride
          }
        }; cpAny()
    }
  }
  
  
  final def copyBuffer(
    components: Int,
    dest: FloatBuffer, destOffset: Int, destStride: Int,
    src: FloatBuffer, srcOffset: Int, srcStride: Int, srcLim: Int
  ) {
    (components: @switch) match {
      
      case 1 => def cp1() {
          var desti = destOffset
          var srci = srcOffset

          while (srci < srcLim) {
            dest.put(desti, src.get(srci))
            desti += destStride
            srci += srcStride
          }
        }; cp1()
      
      case 2 => def cp2() {
          var desti = destOffset
          var srci = srcOffset

          while (srci < srcLim - 1) {
            dest.put(desti, src.get(srci))
            dest.put(desti + 1, src.get(srci + 1))
            desti += destStride
            srci += srcStride
          }
        }; cp2()
      
      case 3 =>  def cp3() {
          var desti = destOffset
          var srci = srcOffset

          while (srci < srcLim - 2) {
            dest.put(desti, src.get(srci))
            dest.put(desti + 1, src.get(srci + 1))
            dest.put(desti + 2, src.get(srci + 2))
            desti += destStride
            srci += srcStride
          }
        }; cp3()
      
      case 4 =>  def cp4() {
          var desti = destOffset
          var srci = srcOffset

          while (srci < srcLim - 3) {
            dest.put(desti, src.get(srci))
            dest.put(desti + 1, src.get(srci + 1))
            dest.put(desti + 2, src.get(srci + 2))
            dest.put(desti + 3, src.get(srci + 3))
            desti += destStride
            srci += srcStride
          }
        }; cp4()
      
      case _ =>  def cpAny() {
          var desti = destOffset
          var srci = srcOffset

          while (srci <= srcLim - components) {

            var j = 0; while (j < components) {
              dest.put(desti + j, src.get(srci + j))
              j += 1
            }

            desti += destStride
            srci += srcStride
          }
        }; cpAny()
    }
  }
  
  
  final def copyBuffer(
    components: Int,
    dest: DoubleBuffer, destOffset: Int, destStride: Int,
    src: DoubleBuffer, srcOffset: Int, srcStride: Int, srcLim: Int
  ) {
    (components: @switch) match {
      
      case 1 => def cp1() {
          var desti = destOffset
          var srci = srcOffset

          while (srci < srcLim) {
            dest.put(desti, src.get(srci))
            desti += destStride
            srci += srcStride
          }
        }; cp1()
      
      case 2 => def cp2() {
          var desti = destOffset
          var srci = srcOffset

          while (srci < srcLim - 1) {
            dest.put(desti, src.get(srci))
            dest.put(desti + 1, src.get(srci + 1))
            desti += destStride
            srci += srcStride
          }
        }; cp2()
      
      case 3 =>  def cp3() {
          var desti = destOffset
          var srci = srcOffset

          while (srci < srcLim - 2) {
            dest.put(desti, src.get(srci))
            dest.put(desti + 1, src.get(srci + 1))
            dest.put(desti + 2, src.get(srci + 2))
            desti += destStride
            srci += srcStride
          }
        }; cp3()
      
      case 4 =>  def cp4() {
          var desti = destOffset
          var srci = srcOffset

          while (srci < srcLim - 3) {
            dest.put(desti, src.get(srci))
            dest.put(desti + 1, src.get(srci + 1))
            dest.put(desti + 2, src.get(srci + 2))
            dest.put(desti + 3, src.get(srci + 3))
            desti += destStride
            srci += srcStride
          }
        }; cp4()
      
      case _ =>  def cpAny() {
          var desti = destOffset
          var srci = srcOffset

          while (srci <= srcLim - components) {

            var j = 0; while (j < components) {
              dest.put(desti + j, src.get(srci + j))
              j += 1
            }

            desti += destStride
            srci += srcStride
          }
        }; cpAny()
    }
  }


  final def copySeqInt(
    components: Int,
    dest: Contiguous[SInt, _], destOffset: Int, destStride: Int,
    src: inContiguous[SInt, _], srcOffset: Int, srcStride: Int, srcLim: Int
  ) {
    (components: @switch) match {
      
      case 1 => def cp1() {
          var desti = destOffset
          var srci = srcOffset

          while (srci < srcLim) {
            dest(desti) = src(srci)
            desti += destStride
            srci += srcStride
          }
        }; cp1()
        
      case 2 => def cp2() {
          var desti = destOffset
          var srci = srcOffset

          while (srci < srcLim - 1) {
            dest(desti) = src(srci)
            dest(desti + 1) = src(srci + 1)
            desti += destStride
            srci += srcStride
          }
        }; cp2()
        
      case 3 => def cp3() {
          var desti = destOffset
          var srci = srcOffset

          while (srci < srcLim - 2) {
            dest(desti) = src(srci)
            dest(desti + 1) = src(srci + 1)
            dest(desti + 2) = src(srci + 2)
            desti += destStride
            srci += srcStride
          }
        }; cp3()
        
      case 4 => def cp4() {
          var desti = destOffset
          var srci = srcOffset

          while (srci < srcLim - 3) {
            dest(desti) = src(srci)
            dest(desti + 1) = src(srci + 1)
            dest(desti + 2) = src(srci + 2)
            dest(desti + 3) = src(srci + 3)
            desti += destStride
            srci += srcStride
          }
        }; cp4()
        
      case _ => def cpAny() {
          var desti = destOffset
          var srci = srcOffset

          while (srci <= srcLim - components) {

            var j = 0; while (j < components) {
              dest(desti + j) = src(srci + j)
              j += 1
            }

            desti += destStride
            srci += srcStride
          }
        }; cpAny()
    }
  }

  
  final def copySeqFloat(
    components: Int,
    dest: Contiguous[RFloat, _], destOffset: Int, destStride: Int,
    src: inContiguous[RFloat, _], srcOffset: Int, srcStride: Int, srcLim: Int
  ) {
    (components: @switch) match {
      
      case 1 => def cp1() {
          var desti = destOffset
          var srci = srcOffset

          while (srci < srcLim) {
            dest(desti) = src(srci)
            desti += destStride
            srci += srcStride
          }
        }; cp1()
        
      case 2 => def cp2() {
          var desti = destOffset
          var srci = srcOffset

          while (srci < srcLim - 1) {
            dest(desti) = src(srci)
            dest(desti + 1) = src(srci + 1)
            desti += destStride
            srci += srcStride
          }
        }; cp2()
        
      case 3 => def cp3() {
          var desti = destOffset
          var srci = srcOffset

          while (srci < srcLim - 2) {
            dest(desti) = src(srci)
            dest(desti + 1) = src(srci + 1)
            dest(desti + 2) = src(srci + 2)
            desti += destStride
            srci += srcStride
          }
        }; cp3()
        
      case 4 => def cp4() {
          var desti = destOffset
          var srci = srcOffset

          while (srci < srcLim - 3) {
            dest(desti) = src(srci)
            dest(desti + 1) = src(srci + 1)
            dest(desti + 2) = src(srci + 2)
            dest(desti + 3) = src(srci + 3)
            desti += destStride
            srci += srcStride
          }
        }; cp4()
        
      case _ => def cpAny() {
          var desti = destOffset
          var srci = srcOffset

          while (srci <= srcLim - components) {

            var j = 0; while (j < components) {
              dest(desti + j) = src(srci + j)
              j += 1
            }

            desti += destStride
            srci += srcStride
          }
        }; cpAny()
    }
  }
  
  
  final def copySeqDouble(
    components: Int,
    dest: Contiguous[RDouble, _], destOffset: Int, destStride: Int,
    src: inContiguous[RDouble, _], srcOffset: Int, srcStride: Int, srcLim: Int
  ) {
    (components: @switch) match {
      
      case 1 => def cp1() {
          var desti = destOffset
          var srci = srcOffset

          while (srci < srcLim) {
            dest(desti) = src(srci)
            desti += destStride
            srci += srcStride
          }
        }; cp1()
        
      case 2 => def cp2() {
          var desti = destOffset
          var srci = srcOffset

          while (srci < srcLim - 1) {
            dest(desti) = src(srci)
            dest(desti + 1) = src(srci + 1)
            desti += destStride
            srci += srcStride
          }
        }; cp2()
        
      case 3 => def cp3() {
          var desti = destOffset
          var srci = srcOffset

          while (srci < srcLim - 2) {
            dest(desti) = src(srci)
            dest(desti + 1) = src(srci + 1)
            dest(desti + 2) = src(srci + 2)
            desti += destStride
            srci += srcStride
          }
        }; cp3()
        
      case 4 => def cp4() {
          var desti = destOffset
          var srci = srcOffset

          while (srci < srcLim - 3) {
            dest(desti) = src(srci)
            dest(desti + 1) = src(srci + 1)
            dest(desti + 2) = src(srci + 2)
            dest(desti + 3) = src(srci + 3)
            desti += destStride
            srci += srcStride
          }
        }; cp4()
        
      case _ => def cpAny() {
          var desti = destOffset
          var srci = srcOffset

          while (srci <= srcLim - components) {

            var j = 0; while (j < components) {
              dest(desti + j) = src(srci + j)
              j += 1
            }

            desti += destStride
            srci += srcStride
          }
        }; cpAny()
    }
  }
  
  
  // Copy2d
  final def copyBuffer2d(
    components: Int,
    dest: ByteBuffer, destDimsX: Int, destOffsetX: Int, destOffsetY: Int,
    src: ByteBuffer, srcDimsX: Int, srcOffsetX: Int, srcOffsetY: Int,
    copyX: Int, copyY: Int
  ) {
    var y = 0; while (y < copyY) {
      val pd = (destOffsetX + (y + destOffsetY)*destDimsX)*components
      val ps = (srcOffsetX + (y + srcOffsetY)*srcDimsX)*components

      dest.position(pd)
      src.limit(ps + copyX*components)
      src.position(ps)
      dest.put(src)

      y += 1
    }
  }
  
  final def copyBuffer2d(
    components: Int,
    dest: ShortBuffer, destDimsX: Int, destOffsetX: Int, destOffsetY: Int,
    src: ShortBuffer, srcDimsX: Int, srcOffsetX: Int, srcOffsetY: Int,
    copyX: Int, copyY: Int
  ) {
    var y = 0; while (y < copyY) {
      val pd = (destOffsetX + (y + destOffsetY)*destDimsX)*components
      val ps = (srcOffsetX + (y + srcOffsetY)*srcDimsX)*components

      dest.position(pd)
      src.limit(ps + copyX*components)
      src.position(ps)
      dest.put(src)

      y += 1
    }
  }
  
  final def copyBuffer2d(
    components: Int,
    dest: CharBuffer, destDimsX: Int, destOffsetX: Int, destOffsetY: Int,
    src: CharBuffer, srcDimsX: Int, srcOffsetX: Int, srcOffsetY: Int,
    copyX: Int, copyY: Int
  ) {
    var y = 0; while (y < copyY) {
      val pd = (destOffsetX + (y + destOffsetY)*destDimsX)*components
      val ps = (srcOffsetX + (y + srcOffsetY)*srcDimsX)*components

      dest.position(pd)
      src.limit(ps + copyX*components)
      src.position(ps)
      dest.put(src)

      y += 1
    }
  }
  
  final def copyBuffer2d(
    components: Int,
    dest: IntBuffer, destDimsX: Int, destOffsetX: Int, destOffsetY: Int,
    src: IntBuffer, srcDimsX: Int, srcOffsetX: Int, srcOffsetY: Int,
    copyX: Int, copyY: Int
  ) {
    var y = 0; while (y < copyY) {
      val pd = (destOffsetX + (y + destOffsetY)*destDimsX)*components
      val ps = (srcOffsetX + (y + srcOffsetY)*srcDimsX)*components

      dest.position(pd)
      src.limit(ps + copyX*components)
      src.position(ps)
      dest.put(src)

      y += 1
    }
  }
  
  final def copyBuffer2d(
    components: Int,
    dest: FloatBuffer, destDimsX: Int, destOffsetX: Int, destOffsetY: Int,
    src: FloatBuffer, srcDimsX: Int, srcOffsetX: Int, srcOffsetY: Int,
    copyX: Int, copyY: Int
  ) {
    var y = 0; while (y < copyY) {
      val pd = (destOffsetX + (y + destOffsetY)*destDimsX)*components
      val ps = (srcOffsetX + (y + srcOffsetY)*srcDimsX)*components

      dest.position(pd)
      src.limit(ps + copyX*components)
      src.position(ps)
      dest.put(src)

      y += 1
    }
  }
  
  final def copyBuffer2d(
    components: Int,
    dest: DoubleBuffer, destDimsX: Int, destOffsetX: Int, destOffsetY: Int,
    src: DoubleBuffer, srcDimsX: Int, srcOffsetX: Int, srcOffsetY: Int,
    copyX: Int, copyY: Int
  ) {
    var y = 0; while (y < copyY) {
      val pd = (destOffsetX + (y + destOffsetY)*destDimsX)*components
      val ps = (srcOffsetX + (y + srcOffsetY)*srcDimsX)*components

      dest.position(pd)
      src.limit(ps + copyX*components)
      src.position(ps)
      dest.put(src)

      y += 1
    }
  }
  
  
  final def copySeqInt2d(
    components: Int,
    dest: Contiguous[SInt, _], destDimsX: Int, destOffsetX: Int, destOffsetY: Int,
    src: inContiguous[SInt, _], srcDimsX: Int, srcOffsetX: Int, srcOffsetY: Int,
    copyX: Int, copyY: Int
  ) {
    (components: @switch) match {
      
      case 1 => def cp1() {
          var y = 0; while (y < copyY) {
            val td = destOffsetX + (y + destOffsetY)*destDimsX
            val ts = srcOffsetX + (y + srcOffsetY)*srcDimsX
            
            var x = 0; while (x < copyX) {
              
              val destIndex = (x + td)
              val srcIndex = (x + ts)
              dest(destIndex) = src(srcIndex)

              x += 1
            }
            y += 1
          }
        }; cp1()
        
      case 2 => def cp2() {
          var y = 0; while (y < copyY) {
            val td = destOffsetX + (y + destOffsetY)*destDimsX
            val ts = srcOffsetX + (y + srcOffsetY)*srcDimsX
            
            var x = 0; while (x < copyX) {
              
              val destIndex = (x + td)*2
              val srcIndex = (x + ts)*2
              dest(destIndex) = src(srcIndex)
              dest(destIndex + 1) = src(srcIndex + 1)

              x += 1
            }
            y += 1
          }
        }; cp2()
        
      case 3 => def cp3() {
          var y = 0; while (y < copyY) {
            val td = destOffsetX + (y + destOffsetY)*destDimsX
            val ts = srcOffsetX + (y + srcOffsetY)*srcDimsX
            
            var x = 0; while (x < copyX) {
              
              val destIndex = (x + td)*3
              val srcIndex = (x + ts)*3
              dest(destIndex) = src(srcIndex)
              dest(destIndex + 1) = src(srcIndex + 1)
              dest(destIndex + 2) = src(srcIndex + 2)

              x += 1
            }
            y += 1
          }
        }; cp3()
        
      case 4 => def cp4() {
          var y = 0; while (y < copyY) {
            val td = destOffsetX + (y + destOffsetY)*destDimsX
            val ts = srcOffsetX + (y + srcOffsetY)*srcDimsX
            
            var x = 0; while (x < copyX) {
              
              val destIndex = (x + td)*4
              val srcIndex = (x + ts)*4
              dest(destIndex) = src(srcIndex)
              dest(destIndex + 1) = src(srcIndex + 1)
              dest(destIndex + 2) = src(srcIndex + 2)
              dest(destIndex + 3) = src(srcIndex + 3)

              x += 1
            }
            y += 1
          }
        }; cp4()
      
      case _ => def cpAny() {
          var y = 0; while (y < copyY) {
            val td = destOffsetX + (y + destOffsetY)*destDimsX
            val ts = srcOffsetX + (y + srcOffsetY)*srcDimsX
            
            var x = 0; while (x < copyX) {
              
              val destIndex = (x + td)*components
              val srcIndex = (x + ts)*components
              
              var j = 0; while (j < components) {
                dest(destIndex + j) = src(srcIndex + j)
                j += 1
              }

              x += 1
            }
            y += 1
          }
        }; cpAny()
    }
  }
  
  final def copySeqFloat2d(
    components: Int,
    dest: Contiguous[RFloat, _], destDimsX: Int, destOffsetX: Int, destOffsetY: Int,
    src: inContiguous[RFloat, _], srcDimsX: Int, srcOffsetX: Int, srcOffsetY: Int,
    copyX: Int, copyY: Int
  ) {
    (components: @switch) match {
      
      case 1 => def cp1() {
          var y = 0; while (y < copyY) {
            val td = destOffsetX + (y + destOffsetY)*destDimsX
            val ts = srcOffsetX + (y + srcOffsetY)*srcDimsX
            
            var x = 0; while (x < copyX) {
              
              val destIndex = (x + td)
              val srcIndex = (x + ts)
              dest(destIndex) = src(srcIndex)

              x += 1
            }
            y += 1
          }
        }; cp1()
        
      case 2 => def cp2() {
          var y = 0; while (y < copyY) {
            val td = destOffsetX + (y + destOffsetY)*destDimsX
            val ts = srcOffsetX + (y + srcOffsetY)*srcDimsX
            
            var x = 0; while (x < copyX) {
              
              val destIndex = (x + td)*2
              val srcIndex = (x + ts)*2
              dest(destIndex) = src(srcIndex)
              dest(destIndex + 1) = src(srcIndex + 1)

              x += 1
            }
            y += 1
          }
        }; cp2()
        
      case 3 => def cp3() {
          var y = 0; while (y < copyY) {
            val td = destOffsetX + (y + destOffsetY)*destDimsX
            val ts = srcOffsetX + (y + srcOffsetY)*srcDimsX
            
            var x = 0; while (x < copyX) {
              
              val destIndex = (x + td)*3
              val srcIndex = (x + ts)*3
              dest(destIndex) = src(srcIndex)
              dest(destIndex + 1) = src(srcIndex + 1)
              dest(destIndex + 2) = src(srcIndex + 2)

              x += 1
            }
            y += 1
          }
        }; cp3()
        
      case 4 => def cp4() {
          var y = 0; while (y < copyY) {
            val td = destOffsetX + (y + destOffsetY)*destDimsX
            val ts = srcOffsetX + (y + srcOffsetY)*srcDimsX
            
            var x = 0; while (x < copyX) {
              
              val destIndex = (x + td)*4
              val srcIndex = (x + ts)*4
              dest(destIndex) = src(srcIndex)
              dest(destIndex + 1) = src(srcIndex + 1)
              dest(destIndex + 2) = src(srcIndex + 2)
              dest(destIndex + 3) = src(srcIndex + 3)

              x += 1
            }
            y += 1
          }
        }; cp4()
      
      case _ => def cpAny() {
          var y = 0; while (y < copyY) {
            val td = destOffsetX + (y + destOffsetY)*destDimsX
            val ts = srcOffsetX + (y + srcOffsetY)*srcDimsX
            
            var x = 0; while (x < copyX) {
              
              val destIndex = (x + td)*components
              val srcIndex = (x + ts)*components
              
              var j = 0; while (j < components) {
                dest(destIndex + j) = src(srcIndex + j)
                j += 1
              }

              x += 1
            }
            y += 1
          }
        }; cpAny()
    }
  }
  
  final def copySeqDouble2d(
    components: Int,
    dest: Contiguous[RDouble, _], destDimsX: Int, destOffsetX: Int, destOffsetY: Int,
    src: inContiguous[RDouble, _], srcDimsX: Int, srcOffsetX: Int, srcOffsetY: Int,
    copyX: Int, copyY: Int
  ) {
    (components: @switch) match {
      
      case 1 => def cp1() {
          var y = 0; while (y < copyY) {
            val td = destOffsetX + (y + destOffsetY)*destDimsX
            val ts = srcOffsetX + (y + srcOffsetY)*srcDimsX
            
            var x = 0; while (x < copyX) {
              
              val destIndex = (x + td)
              val srcIndex = (x + ts)
              dest(destIndex) = src(srcIndex)

              x += 1
            }
            y += 1
          }
        }; cp1()
        
      case 2 => def cp2() {
          var y = 0; while (y < copyY) {
            val td = destOffsetX + (y + destOffsetY)*destDimsX
            val ts = srcOffsetX + (y + srcOffsetY)*srcDimsX
            
            var x = 0; while (x < copyX) {
              
              val destIndex = (x + td)*2
              val srcIndex = (x + ts)*2
              dest(destIndex) = src(srcIndex)
              dest(destIndex + 1) = src(srcIndex + 1)

              x += 1
            }
            y += 1
          }
        }; cp2()
        
      case 3 => def cp3() {
          var y = 0; while (y < copyY) {
            val td = destOffsetX + (y + destOffsetY)*destDimsX
            val ts = srcOffsetX + (y + srcOffsetY)*srcDimsX
            
            var x = 0; while (x < copyX) {
              
              val destIndex = (x + td)*3
              val srcIndex = (x + ts)*3
              dest(destIndex) = src(srcIndex)
              dest(destIndex + 1) = src(srcIndex + 1)
              dest(destIndex + 2) = src(srcIndex + 2)

              x += 1
            }
            y += 1
          }
        }; cp3()
        
      case 4 => def cp4() {
          var y = 0; while (y < copyY) {
            val td = destOffsetX + (y + destOffsetY)*destDimsX
            val ts = srcOffsetX + (y + srcOffsetY)*srcDimsX
            
            var x = 0; while (x < copyX) {
              
              val destIndex = (x + td)*4
              val srcIndex = (x + ts)*4
              dest(destIndex) = src(srcIndex)
              dest(destIndex + 1) = src(srcIndex + 1)
              dest(destIndex + 2) = src(srcIndex + 2)
              dest(destIndex + 3) = src(srcIndex + 3)

              x += 1
            }
            y += 1
          }
        }; cp4()
      
      case _ => def cpAny() {
          var y = 0; while (y < copyY) {
            val td = destOffsetX + (y + destOffsetY)*destDimsX
            val ts = srcOffsetX + (y + srcOffsetY)*srcDimsX
            
            var x = 0; while (x < copyX) {
              
              val destIndex = (x + td)*components
              val srcIndex = (x + ts)*components
              
              var j = 0; while (j < components) {
                dest(destIndex + j) = src(srcIndex + j)
                j += 1
              }

              x += 1
            }
            y += 1
          }
        }; cpAny()
    }
  }
  
  
  // Copy3d
  final def copyBuffer3d(
    components: Int,
    dest: ByteBuffer, destDimsX: Int, destDimsY: Int, destOffsetX: Int, destOffsetY: Int, destOffsetZ: Int,
    src: ByteBuffer, srcDimsX: Int, srcDimsY: Int, srcOffsetX: Int, srcOffsetY: Int, srcOffsetZ: Int,
    copyX: Int, copyY: Int, copyZ: Int
  ) {
    val dmz = destDimsX*destDimsY
    val smz = srcDimsX*srcDimsY
    
    var z = 0; while (z < copyZ) {
      val td = destOffsetX + (z + destOffsetZ)*dmz
      val ts = srcOffsetX + (z + srcOffsetZ)*smz
      
      var y = 0; while (y < copyY) {
        val pd = ((y + destOffsetY)*destDimsX + td)*components
        val ps = ((y + srcOffsetY)*srcDimsX + ts)*components

        dest.position(pd)
        src.limit(ps + copyX*components)
        src.position(ps)
        dest.put(src)

        y += 1
      }
      z += 1
    }
  }
  
  final def copyBuffer3d(
    components: Int,
    dest: ShortBuffer, destDimsX: Int, destDimsY: Int, destOffsetX: Int, destOffsetY: Int, destOffsetZ: Int,
    src: ShortBuffer, srcDimsX: Int, srcDimsY: Int, srcOffsetX: Int, srcOffsetY: Int, srcOffsetZ: Int,
    copyX: Int, copyY: Int, copyZ: Int
  ) {
    val dmz = destDimsX*destDimsY
    val smz = srcDimsX*srcDimsY
    
    var z = 0; while (z < copyZ) {
      val td = destOffsetX + (z + destOffsetZ)*dmz
      val ts = srcOffsetX + (z + srcOffsetZ)*smz
      
      var y = 0; while (y < copyY) {
        val pd = ((y + destOffsetY)*destDimsX + td)*components
        val ps = ((y + srcOffsetY)*srcDimsX + ts)*components

        dest.position(pd)
        src.limit(ps + copyX*components)
        src.position(ps)
        dest.put(src)

        y += 1
      }
      z += 1
    }
  }
  
  final def copyBuffer3d(
    components: Int,
    dest: CharBuffer, destDimsX: Int, destDimsY: Int, destOffsetX: Int, destOffsetY: Int, destOffsetZ: Int,
    src: CharBuffer, srcDimsX: Int, srcDimsY: Int, srcOffsetX: Int, srcOffsetY: Int, srcOffsetZ: Int,
    copyX: Int, copyY: Int, copyZ: Int
  ) {
    val dmz = destDimsX*destDimsY
    val smz = srcDimsX*srcDimsY
    
    var z = 0; while (z < copyZ) {
      val td = destOffsetX + (z + destOffsetZ)*dmz
      val ts = srcOffsetX + (z + srcOffsetZ)*smz
      
      var y = 0; while (y < copyY) {
        val pd = ((y + destOffsetY)*destDimsX + td)*components
        val ps = ((y + srcOffsetY)*srcDimsX + ts)*components

        dest.position(pd)
        src.limit(ps + copyX*components)
        src.position(ps)
        dest.put(src)

        y += 1
      }
      z += 1
    }
  }
  
  final def copyBuffer3d(
    components: Int,
    dest: IntBuffer, destDimsX: Int, destDimsY: Int, destOffsetX: Int, destOffsetY: Int, destOffsetZ: Int,
    src: IntBuffer, srcDimsX: Int, srcDimsY: Int, srcOffsetX: Int, srcOffsetY: Int, srcOffsetZ: Int,
    copyX: Int, copyY: Int, copyZ: Int
  ) {
    val dmz = destDimsX*destDimsY
    val smz = srcDimsX*srcDimsY
    
    var z = 0; while (z < copyZ) {
      val td = destOffsetX + (z + destOffsetZ)*dmz
      val ts = srcOffsetX + (z + srcOffsetZ)*smz
      
      var y = 0; while (y < copyY) {
        val pd = ((y + destOffsetY)*destDimsX + td)*components
        val ps = ((y + srcOffsetY)*srcDimsX + ts)*components

        dest.position(pd)
        src.limit(ps + copyX*components)
        src.position(ps)
        dest.put(src)

        y += 1
      }
      z += 1
    }
  }
  
  final def copyBuffer3d(
    components: Int,
    dest: FloatBuffer, destDimsX: Int, destDimsY: Int, destOffsetX: Int, destOffsetY: Int, destOffsetZ: Int,
    src: FloatBuffer, srcDimsX: Int, srcDimsY: Int, srcOffsetX: Int, srcOffsetY: Int, srcOffsetZ: Int,
    copyX: Int, copyY: Int, copyZ: Int
  ) {
    val dmz = destDimsX*destDimsY
    val smz = srcDimsX*srcDimsY
    
    var z = 0; while (z < copyZ) {
      val td = destOffsetX + (z + destOffsetZ)*dmz
      val ts = srcOffsetX + (z + srcOffsetZ)*smz
      
      var y = 0; while (y < copyY) {
        val pd = ((y + destOffsetY)*destDimsX + td)*components
        val ps = ((y + srcOffsetY)*srcDimsX + ts)*components

        dest.position(pd)
        src.limit(ps + copyX*components)
        src.position(ps)
        dest.put(src)

        y += 1
      }
      z += 1
    }
  }
  
  final def copyBuffer3d(
    components: Int,
    dest: DoubleBuffer, destDimsX: Int, destDimsY: Int, destOffsetX: Int, destOffsetY: Int, destOffsetZ: Int,
    src: DoubleBuffer, srcDimsX: Int, srcDimsY: Int, srcOffsetX: Int, srcOffsetY: Int, srcOffsetZ: Int,
    copyX: Int, copyY: Int, copyZ: Int
  ) {
    val dmz = destDimsX*destDimsY
    val smz = srcDimsX*srcDimsY
    
    var z = 0; while (z < copyZ) {
      val td = destOffsetX + (z + destOffsetZ)*dmz
      val ts = srcOffsetX + (z + srcOffsetZ)*smz
      
      var y = 0; while (y < copyY) {
        val pd = ((y + destOffsetY)*destDimsX + td)*components
        val ps = ((y + srcOffsetY)*srcDimsX + ts)*components

        dest.position(pd)
        src.limit(ps + copyX*components)
        src.position(ps)
        dest.put(src)

        y += 1
      }
      z += 1
    }
  }
  
  final def copySeqInt3d(
    components: Int,
    dest: Contiguous[SInt, _], destDimsX: Int, destDimsY: Int, destOffsetX: Int, destOffsetY: Int, destOffsetZ: Int,
    src: inContiguous[SInt, _], srcDimsX: Int, srcDimsY: Int, srcOffsetX: Int, srcOffsetY: Int, srcOffsetZ: Int,
    copyX: Int, copyY: Int, copyZ: Int
  ) {
    (components: @switch) match {
      
      case 1 => def cp1() {
          val dmz = destDimsX*destDimsY
          val smz = srcDimsX*srcDimsY

          var z = 0; while (z < copyZ) {
            val dtz = destOffsetX + (z + destOffsetZ)*dmz
            val stz = srcOffsetX + (z + srcOffsetZ)*smz

            var y = 0; while (y < copyY) {
              val dty = (y + destOffsetY)*destDimsX + dtz
              val sty = (y + srcOffsetY)*srcDimsX + stz

              var x = 0; while (x < copyX) {
                val destIndex = (x + dty)
                val srcIndex = (x + sty)
                dest(destIndex) = src(srcIndex)

                x += 1
              }
              y += 1
            }
            z += 1
          }
        }; cp1()
        
      case 2 => def cp2() {
          val dmz = destDimsX*destDimsY
          val smz = srcDimsX*srcDimsY

          var z = 0; while (z < copyZ) {
            val dtz = destOffsetX + (z + destOffsetZ)*dmz
            val stz = srcOffsetX + (z + srcOffsetZ)*smz

            var y = 0; while (y < copyY) {
              val dty = (y + destOffsetY)*destDimsX + dtz
              val sty = (y + srcOffsetY)*srcDimsX + stz

              var x = 0; while (x < copyX) {
                val destIndex = (x + dty)*2
                val srcIndex = (x + sty)*2
                dest(destIndex) = src(srcIndex)
                dest(destIndex + 1) = src(srcIndex + 1)

                x += 1
              }
              y += 1
            }
            z += 1
          }
        }; cp2()
        
        case 3 => def cp3() {
          val dmz = destDimsX*destDimsY
          val smz = srcDimsX*srcDimsY

          var z = 0; while (z < copyZ) {
            val dtz = destOffsetX + (z + destOffsetZ)*dmz
            val stz = srcOffsetX + (z + srcOffsetZ)*smz

            var y = 0; while (y < copyY) {
              val dty = (y + destOffsetY)*destDimsX + dtz
              val sty = (y + srcOffsetY)*srcDimsX + stz

              var x = 0; while (x < copyX) {
                val destIndex = (x + dty)*3
                val srcIndex = (x + sty)*3
                dest(destIndex) = src(srcIndex)
                dest(destIndex + 1) = src(srcIndex + 1)
                dest(destIndex + 2) = src(srcIndex + 2)

                x += 1
              }
              y += 1
            }
            z += 1
          }
        }; cp3()
        
        case 4 => def cp4() {
          val dmz = destDimsX*destDimsY
          val smz = srcDimsX*srcDimsY

          var z = 0; while (z < copyZ) {
            val dtz = destOffsetX + (z + destOffsetZ)*dmz
            val stz = srcOffsetX + (z + srcOffsetZ)*smz

            var y = 0; while (y < copyY) {
              val dty = (y + destOffsetY)*destDimsX + dtz
              val sty = (y + srcOffsetY)*srcDimsX + stz

              var x = 0; while (x < copyX) {
                val destIndex = (x + dty)*4
                val srcIndex = (x + sty)*4
                dest(destIndex) = src(srcIndex)
                dest(destIndex + 1) = src(srcIndex + 1)
                dest(destIndex + 2) = src(srcIndex + 2)
                dest(destIndex + 3) = src(srcIndex + 3)

                x += 1
              }
              y += 1
            }
            z += 1
          }
        }; cp4()
        
      case _ => def cpAny() {
          val dmz = destDimsX*destDimsY
          val smz = srcDimsX*srcDimsY

          var z = 0; while (z < copyZ) {
            val dtz = destOffsetX + (z + destOffsetZ)*dmz
            val stz = srcOffsetX + (z + srcOffsetZ)*smz

            var y = 0; while (y < copyY) {
              val dty = (y + destOffsetY)*destDimsX + dtz
              val sty = (y + srcOffsetY)*srcDimsX + stz

              var x = 0; while (x < copyX) {
                val destIndex = (x + dty)*components
                val srcIndex = (x + sty)*components
                
                var j = 0; while (j < components) {
                  dest(destIndex + j) = src(srcIndex + j)
                  j += 1
                }

                x += 1
              }
              y += 1
            }
            z += 1
          }
        }; cpAny()
    }
  }
  
  final def copySeqFloat3d(
    components: Int,
    dest: Contiguous[RFloat, _], destDimsX: Int, destDimsY: Int, destOffsetX: Int, destOffsetY: Int, destOffsetZ: Int,
    src: inContiguous[RFloat, _], srcDimsX: Int, srcDimsY: Int, srcOffsetX: Int, srcOffsetY: Int, srcOffsetZ: Int,
    copyX: Int, copyY: Int, copyZ: Int
  ) {
    (components: @switch) match {
      
      case 1 => def cp1() {
          val dmz = destDimsX*destDimsY
          val smz = srcDimsX*srcDimsY

          var z = 0; while (z < copyZ) {
            val dtz = destOffsetX + (z + destOffsetZ)*dmz
            val stz = srcOffsetX + (z + srcOffsetZ)*smz

            var y = 0; while (y < copyY) {
              val dty = (y + destOffsetY)*destDimsX + dtz
              val sty = (y + srcOffsetY)*srcDimsX + stz

              var x = 0; while (x < copyX) {
                val destIndex = (x + dty)
                val srcIndex = (x + sty)
                dest(destIndex) = src(srcIndex)

                x += 1
              }
              y += 1
            }
            z += 1
          }
        }; cp1()
        
      case 2 => def cp2() {
          val dmz = destDimsX*destDimsY
          val smz = srcDimsX*srcDimsY

          var z = 0; while (z < copyZ) {
            val dtz = destOffsetX + (z + destOffsetZ)*dmz
            val stz = srcOffsetX + (z + srcOffsetZ)*smz

            var y = 0; while (y < copyY) {
              val dty = (y + destOffsetY)*destDimsX + dtz
              val sty = (y + srcOffsetY)*srcDimsX + stz

              var x = 0; while (x < copyX) {
                val destIndex = (x + dty)*2
                val srcIndex = (x + sty)*2
                dest(destIndex) = src(srcIndex)
                dest(destIndex + 1) = src(srcIndex + 1)

                x += 1
              }
              y += 1
            }
            z += 1
          }
        }; cp2()
        
        case 3 => def cp3() {
          val dmz = destDimsX*destDimsY
          val smz = srcDimsX*srcDimsY

          var z = 0; while (z < copyZ) {
            val dtz = destOffsetX + (z + destOffsetZ)*dmz
            val stz = srcOffsetX + (z + srcOffsetZ)*smz

            var y = 0; while (y < copyY) {
              val dty = (y + destOffsetY)*destDimsX + dtz
              val sty = (y + srcOffsetY)*srcDimsX + stz

              var x = 0; while (x < copyX) {
                val destIndex = (x + dty)*3
                val srcIndex = (x + sty)*3
                dest(destIndex) = src(srcIndex)
                dest(destIndex + 1) = src(srcIndex + 1)
                dest(destIndex + 2) = src(srcIndex + 2)

                x += 1
              }
              y += 1
            }
            z += 1
          }
        }; cp3()
        
        case 4 => def cp4() {
          val dmz = destDimsX*destDimsY
          val smz = srcDimsX*srcDimsY

          var z = 0; while (z < copyZ) {
            val dtz = destOffsetX + (z + destOffsetZ)*dmz
            val stz = srcOffsetX + (z + srcOffsetZ)*smz

            var y = 0; while (y < copyY) {
              val dty = (y + destOffsetY)*destDimsX + dtz
              val sty = (y + srcOffsetY)*srcDimsX + stz

              var x = 0; while (x < copyX) {
                val destIndex = (x + dty)*4
                val srcIndex = (x + sty)*4
                dest(destIndex) = src(srcIndex)
                dest(destIndex + 1) = src(srcIndex + 1)
                dest(destIndex + 2) = src(srcIndex + 2)
                dest(destIndex + 3) = src(srcIndex + 3)

                x += 1
              }
              y += 1
            }
            z += 1
          }
        }; cp4()
        
      case _ => def cpAny() {
          val dmz = destDimsX*destDimsY
          val smz = srcDimsX*srcDimsY

          var z = 0; while (z < copyZ) {
            val dtz = destOffsetX + (z + destOffsetZ)*dmz
            val stz = srcOffsetX + (z + srcOffsetZ)*smz

            var y = 0; while (y < copyY) {
              val dty = (y + destOffsetY)*destDimsX + dtz
              val sty = (y + srcOffsetY)*srcDimsX + stz

              var x = 0; while (x < copyX) {
                val destIndex = (x + dty)*components
                val srcIndex = (x + sty)*components
                
                var j = 0; while (j < components) {
                  dest(destIndex + j) = src(srcIndex + j)
                  j += 1
                }

                x += 1
              }
              y += 1
            }
            z += 1
          }
        }; cpAny()
    }
  }
  
  final def copySeqDouble3d(
    components: Int,
    dest: Contiguous[RDouble, _], destDimsX: Int, destDimsY: Int, destOffsetX: Int, destOffsetY: Int, destOffsetZ: Int,
    src: inContiguous[RDouble, _], srcDimsX: Int, srcDimsY: Int, srcOffsetX: Int, srcOffsetY: Int, srcOffsetZ: Int,
    copyX: Int, copyY: Int, copyZ: Int
  ) {
    (components: @switch) match {
      
      case 1 => def cp1() {
          val dmz = destDimsX*destDimsY
          val smz = srcDimsX*srcDimsY

          var z = 0; while (z < copyZ) {
            val dtz = destOffsetX + (z + destOffsetZ)*dmz
            val stz = srcOffsetX + (z + srcOffsetZ)*smz

            var y = 0; while (y < copyY) {
              val dty = (y + destOffsetY)*destDimsX + dtz
              val sty = (y + srcOffsetY)*srcDimsX + stz

              var x = 0; while (x < copyX) {
                val destIndex = (x + dty)
                val srcIndex = (x + sty)
                dest(destIndex) = src(srcIndex)

                x += 1
              }
              y += 1
            }
            z += 1
          }
        }; cp1()
        
      case 2 => def cp2() {
          val dmz = destDimsX*destDimsY
          val smz = srcDimsX*srcDimsY

          var z = 0; while (z < copyZ) {
            val dtz = destOffsetX + (z + destOffsetZ)*dmz
            val stz = srcOffsetX + (z + srcOffsetZ)*smz

            var y = 0; while (y < copyY) {
              val dty = (y + destOffsetY)*destDimsX + dtz
              val sty = (y + srcOffsetY)*srcDimsX + stz

              var x = 0; while (x < copyX) {
                val destIndex = (x + dty)*2
                val srcIndex = (x + sty)*2
                dest(destIndex) = src(srcIndex)
                dest(destIndex + 1) = src(srcIndex + 1)

                x += 1
              }
              y += 1
            }
            z += 1
          }
        }; cp2()
        
        case 3 => def cp3() {
          val dmz = destDimsX*destDimsY
          val smz = srcDimsX*srcDimsY

          var z = 0; while (z < copyZ) {
            val dtz = destOffsetX + (z + destOffsetZ)*dmz
            val stz = srcOffsetX + (z + srcOffsetZ)*smz

            var y = 0; while (y < copyY) {
              val dty = (y + destOffsetY)*destDimsX + dtz
              val sty = (y + srcOffsetY)*srcDimsX + stz

              var x = 0; while (x < copyX) {
                val destIndex = (x + dty)*3
                val srcIndex = (x + sty)*3
                dest(destIndex) = src(srcIndex)
                dest(destIndex + 1) = src(srcIndex + 1)
                dest(destIndex + 2) = src(srcIndex + 2)

                x += 1
              }
              y += 1
            }
            z += 1
          }
        }; cp3()
        
        case 4 => def cp4() {
          val dmz = destDimsX*destDimsY
          val smz = srcDimsX*srcDimsY

          var z = 0; while (z < copyZ) {
            val dtz = destOffsetX + (z + destOffsetZ)*dmz
            val stz = srcOffsetX + (z + srcOffsetZ)*smz

            var y = 0; while (y < copyY) {
              val dty = (y + destOffsetY)*destDimsX + dtz
              val sty = (y + srcOffsetY)*srcDimsX + stz

              var x = 0; while (x < copyX) {
                val destIndex = (x + dty)*4
                val srcIndex = (x + sty)*4
                dest(destIndex) = src(srcIndex)
                dest(destIndex + 1) = src(srcIndex + 1)
                dest(destIndex + 2) = src(srcIndex + 2)
                dest(destIndex + 3) = src(srcIndex + 3)

                x += 1
              }
              y += 1
            }
            z += 1
          }
        }; cp4()
        
      case _ => def cpAny() {
          val dmz = destDimsX*destDimsY
          val smz = srcDimsX*srcDimsY

          var z = 0; while (z < copyZ) {
            val dtz = destOffsetX + (z + destOffsetZ)*dmz
            val stz = srcOffsetX + (z + srcOffsetZ)*smz

            var y = 0; while (y < copyY) {
              val dty = (y + destOffsetY)*destDimsX + dtz
              val sty = (y + srcOffsetY)*srcDimsX + stz

              var x = 0; while (x < copyX) {
                val destIndex = (x + dty)*components
                val srcIndex = (x + sty)*components
                
                var j = 0; while (j < components) {
                  dest(destIndex + j) = src(srcIndex + j)
                  j += 1
                }

                x += 1
              }
              y += 1
            }
            z += 1
          }
        }; cpAny()
    }
  }
  
  
  final def reorderByteBuffer[T <: Accessor](
    ordering: DataOrdering,
    dest: Data[T], destFirst: Int,
    src: ReadData[T], srcFirst: Int, stride: Int, count: Int
  ) {
    val byteStride = src.byteStride
    val chunk = stride*byteStride
    
    val destBuffer = dest.bindingBufferSubData(destFirst, count*stride).asInstanceOf[ByteBuffer]
    val srcBuffer = src.bindingBuffer().asInstanceOf[ByteBuffer]
    
    
    chunk match {
      
      case 1 => def single() {
        var i = 0; while (i < count) {
          val srcIndex = srcFirst + ordering.indexOf(i)
          destBuffer.put(srcBuffer.get(srcIndex))
          
          i += 1
        }
      }; single()
      
      case 3 => def tri() {
        var i = 0; while (i < count) {
          val srcIndex = (srcFirst + ordering.indexOf(i)*stride)*byteStride
          destBuffer.put(srcBuffer.get(srcIndex))
          destBuffer.put(srcBuffer.get(srcIndex + 1))
          destBuffer.put(srcBuffer.get(srcIndex + 2))
          
          i += 1
        }
      }; tri()
    
      case _ => def multipart() {
        var i = 0; while (i < count) {
          val srcIndex = (srcFirst + ordering.indexOf(i)*stride)*byteStride
          srcBuffer.limit(srcIndex + chunk)
          srcBuffer.position(srcIndex)
          
          destBuffer.put(srcBuffer)
          
          i += 1
        }
      }; multipart()
    }
  }
  
  final def reorderCharBuffer[T <: Accessor](
    ordering: DataOrdering,
    dest: Data[T], destFirst: Int,
    src: ReadData[T], srcFirst: Int, stride: Int, count: Int
  ) {
    val components = src.components
    val chunk = stride*components
    
    val destBuffer = dest.buffer().asInstanceOf[CharBuffer]
    destBuffer.limit(destFirst*components + count*chunk)
    destBuffer.position(destFirst*components)
    val srcBuffer = src.readOnlyBuffer().asInstanceOf[CharBuffer]
    
    
    chunk match {
      
      case 1 => def single() {
        var i = 0; while (i < count) {
          val srcIndex = srcFirst + ordering.indexOf(i)
          destBuffer.put(srcBuffer.get(srcIndex))
          
          i += 1
        }
      }; single()
      
      case 3 => def tri() {
        var i = 0; while (i < count) {
          val srcIndex = (srcFirst + ordering.indexOf(i)*stride)*components
          destBuffer.put(srcBuffer.get(srcIndex))
          destBuffer.put(srcBuffer.get(srcIndex + 1))
          destBuffer.put(srcBuffer.get(srcIndex + 2))
          
          i += 1
        }
      }; tri()

      case _ => def multipart() {
        var i = 0; while (i < count) {
          val srcIndex = (srcFirst + ordering.indexOf(i)*stride)*components
          srcBuffer.limit(srcIndex + chunk)
          srcBuffer.position(srcIndex)
          
          destBuffer.put(srcBuffer)
          
          i += 1
        }
      }; multipart()
    }
  }
  
  final def reorderShortBuffer[T <: Accessor](
    ordering: DataOrdering,
    dest: Data[T], destFirst: Int,
    src: ReadData[T], srcFirst: Int, stride: Int, count: Int
  ) {
    val components = src.components
    val chunk = stride*components
    
    val destBuffer = dest.buffer().asInstanceOf[ShortBuffer]
    destBuffer.limit(destFirst*components + count*chunk)
    destBuffer.position(destFirst*components)
    val srcBuffer = src.readOnlyBuffer().asInstanceOf[ShortBuffer]
    
    
    chunk match {
      
      case 1 => def single() {
        var i = 0; while (i < count) {
          val srcIndex = srcFirst + ordering.indexOf(i)
          destBuffer.put(srcBuffer.get(srcIndex))
          
          i += 1
        }
      }; single()
      
      case 3 => def tri() {
        var i = 0; while (i < count) {
          val srcIndex = (srcFirst + ordering.indexOf(i)*stride)*components
          destBuffer.put(srcBuffer.get(srcIndex))
          destBuffer.put(srcBuffer.get(srcIndex + 1))
          destBuffer.put(srcBuffer.get(srcIndex + 2))
          
          i += 1
        }
      }; tri()

      case _ => def multipart() {
        var i = 0; while (i < count) {
          val srcIndex = (srcFirst + ordering.indexOf(i)*stride)*components
          srcBuffer.limit(srcIndex + chunk)
          srcBuffer.position(srcIndex)
          
          destBuffer.put(srcBuffer)
          
          i += 1
        }
      }; multipart()
    }
  }
  
  final def reorderIntBuffer[T <: Accessor](
    ordering: DataOrdering,
    dest: Data[T], destFirst: Int,
    src: ReadData[T], srcFirst: Int, stride: Int, count: Int
  ) {
    val components = src.components
    val chunk = stride*components
    
    val destBuffer = dest.buffer().asInstanceOf[IntBuffer]
    destBuffer.limit(destFirst*components + count*chunk)
    destBuffer.position(destFirst*components)
    val srcBuffer = src.readOnlyBuffer().asInstanceOf[IntBuffer]
    
    
    chunk match {
      
      case 1 => def single() {
        var i = 0; while (i < count) {
          val srcIndex = srcFirst + ordering.indexOf(i)
          destBuffer.put(srcBuffer.get(srcIndex))
          
          i += 1
        }
      }; single()
      
      case 3 => def tri() {
        var i = 0; while (i < count) {
          val srcIndex = (srcFirst + ordering.indexOf(i)*stride)*components
          destBuffer.put(srcBuffer.get(srcIndex))
          destBuffer.put(srcBuffer.get(srcIndex + 1))
          destBuffer.put(srcBuffer.get(srcIndex + 2))
          
          i += 1
        }
      }; tri()

      case _ => def multipart() {
        var i = 0; while (i < count) {
          val srcIndex = (srcFirst + ordering.indexOf(i)*stride)*components
          srcBuffer.limit(srcIndex + chunk)
          srcBuffer.position(srcIndex)
          
          destBuffer.put(srcBuffer)
          
          i += 1
        }
      }; multipart()
    }
  }
  
  final def reorderFloatBuffer[T <: Accessor](
    ordering: DataOrdering,
    dest: Data[T], destFirst: Int,
    src: ReadData[T], srcFirst: Int, stride: Int, count: Int
  ) {
    val components = src.components
    val chunk = stride*components
    
    val destBuffer = dest.buffer().asInstanceOf[FloatBuffer]
    destBuffer.limit(destFirst*components + count*chunk)
    destBuffer.position(destFirst*components)
    val srcBuffer = src.readOnlyBuffer().asInstanceOf[FloatBuffer]
    
    
    chunk match {
      
      case 1 => def single() {
        var i = 0; while (i < count) {
          val srcIndex = srcFirst + ordering.indexOf(i)
          destBuffer.put(srcBuffer.get(srcIndex))
          
          i += 1
        }
      }; single()
      
      case 3 => def tri() {
        var i = 0; while (i < count) {
          val srcIndex = (srcFirst + ordering.indexOf(i)*stride)*components
          destBuffer.put(srcBuffer.get(srcIndex))
          destBuffer.put(srcBuffer.get(srcIndex + 1))
          destBuffer.put(srcBuffer.get(srcIndex + 2))
          
          i += 1
        }
      }; tri()

      case _ => def multipart() {
        var i = 0; while (i < count) {
          val srcIndex = (srcFirst + ordering.indexOf(i)*stride)*components
          srcBuffer.limit(srcIndex + chunk)
          srcBuffer.position(srcIndex)
          
          destBuffer.put(srcBuffer)
          
          i += 1
        }
      }; multipart()
    }
  }
  
  final def reorderDoubleBuffer[T <: Accessor](
    ordering: DataOrdering,
    dest: Data[T], destFirst: Int,
    src: ReadData[T], srcFirst: Int, stride: Int, count: Int
  ) {
    val components = src.components
    val chunk = stride*components
    
    val destBuffer = dest.buffer().asInstanceOf[DoubleBuffer]
    destBuffer.limit(destFirst*components + count*chunk)
    destBuffer.position(destFirst*components)
    val srcBuffer = src.readOnlyBuffer().asInstanceOf[DoubleBuffer]
    
    
    chunk match {
      
      case 1 => def single() {
        var i = 0; while (i < count) {
          val srcIndex = srcFirst + ordering.indexOf(i)
          destBuffer.put(srcBuffer.get(srcIndex))
          
          i += 1
        }
      }; single()
      
      case 3 => def tri() {
        var i = 0; while (i < count) {
          val srcIndex = (srcFirst + ordering.indexOf(i)*stride)*components
          destBuffer.put(srcBuffer.get(srcIndex))
          destBuffer.put(srcBuffer.get(srcIndex + 1))
          destBuffer.put(srcBuffer.get(srcIndex + 2))
          
          i += 1
        }
      }; tri()

      case _ => def multipart() {
        var i = 0; while (i < count) {
          val srcIndex = (srcFirst + ordering.indexOf(i)*stride)*components
          srcBuffer.limit(srcIndex + chunk)
          srcBuffer.position(srcIndex)
          
          destBuffer.put(srcBuffer)
          
          i += 1
        }
      }; multipart()
    }
  }
}
