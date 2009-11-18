/*
 * Simplex3D, Math package
 * Copyright (C) 2009 Simplex3D team
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *
 * CLASSPATH EXCEPTION FOR UNMODIFIED WORK:
 * Linking this library statically or dynamically with other modules is making
 * a combined work based on this library. Thus, the terms and conditions of
 * the GNU General Public License cover the whole combination.
 *
 * As a special exception, the copyright holders of this library give you
 * permission to link this library with independent modules to produce
 * an executable, regardless of the license terms of these independent modules,
 * and to copy and distribute the resulting executable under terms of your
 * choice, provided that you also meet, for each linked independent module,
 * the terms and conditions of the license of that module. An independent module
 * is a module which is not derived from or based on this library. If you modify
 * this library in any way, then this exception is null and void and no longer
 * applies, in this case delete this exception statement from your version.
 */

package simplex3d.math.buffer

import simplex3d.math.buffer.BufferSupport._
import simplex3d.math.buffer.BufferCopy._
import java.nio._


/**
 * @author Aleksey Nikiforov (lex)
 */
object DataType {
    val Byte = 5120
    val UByte = 5121
    val Short = 5122
    val UShort = 5123
    val Int = 5124
    val UInt = 5125
    val Float = 5126
    val Double = 5130
}

abstract class DataSequence {
    /** sequence element */
    type E
    /** java data type */
    type D
    /** raw read/write type (either Int or Float) */
    type Raw
    
    type BufferType <: Buffer
    type SeqType <: DataSequence
    type ArraySeqType <: ArraySequence with SeqType
    type BufferSeqType <: BufferSequence with SeqType

    def offset: Int
    def stride: Int
    def length: Int
    protected val step: Int

    def buffer: BufferType
    def components: Int
    def dataType: Int
    def normalized: Boolean
    def dataTypeBytes: Int
    def byteSize = buffer.limit*dataTypeBytes
    def byteOffset = offset*dataTypeBytes
    def byteStride = stride*dataTypeBytes

    def apply(i: Int) :E
    def update(i: Int, v: E)

    private[buffer] def readRaw(i: Int) :Raw
    private[buffer] def writeRaw(i: Int, x: Raw) :Unit

    def copyTo(array: Array[D], offset: Int, stride: Int,
               first: Int, count: Int) :ArraySeqType =
    {
        val seq = makeArraySequence(array, offset, stride)
        copyToArray(seq, first, count)
        seq
    }
    def copyTo(array: Array[D], offset: Int, stride: Int) :ArraySeqType = {
        copyTo(array, offset, stride, 0, length)
    }

    def copyTo(buf: Buffer, offset: Int, stride: Int,
               first: Int, count: Int) :BufferSeqType

    def copyTo(buf: Buffer, offset: Int, stride: Int) :BufferSeqType = {
        copyTo(buf, offset, stride, 0, length)
    }
    
    def copyToArray(first: Int, count: Int) :ArraySeqType
    def copyToArray() :ArraySeqType = {
        copyToArray(0, length)
    }
    def copyToBuffer(first: Int, count: Int) :BufferSeqType = {
        val buf = allocateByteBuffer(count*dataTypeBytes*components)
        copyTo(buf, 0, 0, first, count)
    }
    def copyToBuffer() :BufferSeqType = {
        copyToBuffer(0, length)
    }
    def copyTo(s: SeqType, first: Int, count: Int) {
        if (buffer.isDirect) {
            copyToBuffer(s.asInstanceOf[BufferSeqType], first, count)
        } else {
            copyToArray(s.asInstanceOf[ArraySeqType], first, count)
        }
    }
    def copyTo(s: SeqType) {
        copyTo(s, 0, length)
    }
    def copyToArray(s: ArraySeqType, first: Int, count: Int)
    def copyToArray(s: ArraySeqType) {
        copyToArray(s, 0, length)
    }
    def copyToBuffer(s: BufferSeqType, first: Int, count: Int)
    def copyToBuffer(s: BufferSeqType) {
        copyToBuffer(s, 0, length)
    }

    private[buffer] def makeArraySequence(array: Array[D], stride: Int, offset: Int)
    :ArraySeqType
    private[buffer] def makeBufferSequence(buffer: BufferType, stride: Int,offset:Int)
    :BufferSeqType
}

trait ArraySequence extends DataSequence {
    def array: Array[D]
}

trait BufferSequence extends DataSequence {
    if (!buffer.isDirect) {
        throw new IllegalArgumentException("The buffer must be direct.")
    }
}

trait SignedSequence extends DataSequence
trait UnsignedSequence extends DataSequence

trait FloatSequence extends DataSequence {
    type D = Float
    type BufferType = FloatBuffer

    def dataType = DataType.Float
    def normalized = false
    def dataTypeBytes = 4

    def copyTo(buf: Buffer, offset: Int, stride: Int,
               first: Int, count: Int) :BufferSeqType =
    {
        val b = buf match {
            case b: FloatBuffer => b
            case b: ByteBuffer => b.asFloatBuffer
            case _ => throw new IllegalArgumentException(
                        "The buffers must be ByteBuffer or FloatBuffer.")
        }

        val seq = makeBufferSequence(b, offset, stride)
        copyToBuffer(seq, first, count)
        seq
    }
    def copyToArray(first: Int, count: Int) :ArraySeqType = {
        val array = new Array[Float](count*components)
        val seq = makeArraySequence(array, 0, 0)
        copyToArray(seq, first, count)
        seq
    }
}

trait ByteSequence extends DataSequence {
    type D = Byte
    type BufferType = ByteBuffer

    def dataTypeBytes = 1

    def copyTo(buf: Buffer, offset: Int, stride: Int,
               first: Int, count: Int) :BufferSeqType =
    {
        val b = buf match {
            case b: ByteBuffer => b
            case _ => throw new IllegalArgumentException(
                        "The buffers must be ByteBuffer.")
        }

        val seq = makeBufferSequence(b, offset, stride)
        copyToBuffer(seq, first, count)
        seq
    }
    def copyToArray(first: Int, count: Int) :ArraySeqType = {
        val array = new Array[Byte](count*components)
        val seq = makeArraySequence(array, 0, 0)
        copyToArray(seq, first, count)
        seq
    }
}

trait ShortSequence extends DataSequence {
    type D = Short
    type BufferType = ShortBuffer

    def dataTypeBytes = 2

    def copyTo(buf: Buffer, offset: Int, stride: Int,
               first: Int, count: Int) :BufferSeqType =
    {
        val b = buf match {
            case b: ShortBuffer => b
            case b: ByteBuffer => b.asShortBuffer
            case _ => throw new IllegalArgumentException(
                        "The buffers must be ByteBuffer or ShortBuffer.")
        }

        val seq = makeBufferSequence(b, offset, stride)
        copyToBuffer(seq, first, count)
        seq
    }
    def copyToArray(first: Int, count: Int) :ArraySeqType = {
        val array = new Array[Short](count*components)
        val seq = makeArraySequence(array, 0, 0)
        copyToArray(seq, first, count)
        seq
    }
}

trait IntSequence extends DataSequence {
    type D = Int
    type BufferType = IntBuffer

    def dataTypeBytes = 4

    def copyTo(buf: Buffer, offset: Int, stride: Int,
               first: Int, count: Int) :BufferSeqType =
    {
        val b = buf match {
            case b: IntBuffer => b
            case b: ByteBuffer => b.asIntBuffer
            case _ => throw new IllegalArgumentException(
                        "The buffers must be ByteBuffer or IntBuffer.")
        }

        val seq = makeBufferSequence(b, offset, stride)
        copyToBuffer(seq, first, count)
        seq
    }
    def copyToArray(first: Int, count: Int) :ArraySeqType = {
        val array = new Array[Int](count*components)
        val seq = makeArraySequence(array, 0, 0)
        copyToArray(seq, first, count)
        seq
    }
}

trait FloatArraySequence extends ArraySequence with FloatSequence {
    override type Raw = Float
    private[buffer] def readRaw(i: Int) :Raw = array(i)
    private[buffer] def writeRaw(i: Int, x: Raw) { array(i) = x }

    def copyToArray(s: ArraySeqType, first: Int, count: Int) {
        copy(components, count,
             array, first*step + offset, stride,
             s.array.asInstanceOf[Array[Float]], s.offset, s.stride)
    }
    def copyToBuffer(s: BufferSeqType, first: Int, count: Int) {
        copy(components, count,
             array, first*step + offset, stride,
             s.buffer.asInstanceOf[FloatBuffer], s.offset, s.stride)
    }
}

trait FloatBufferSequence extends BufferSequence with FloatSequence {
    override type Raw = Float
    private[buffer] def readRaw(i: Int) :Raw = buffer.get(i)
    private[buffer] def writeRaw(i: Int, x: Raw) { buffer.put(i, x) }

    def copyToArray(s: ArraySeqType, first: Int, count: Int) {
        copy(components, count,
             buffer, first*step + offset, stride,
             s.array.asInstanceOf[Array[Float]], s.offset, s.stride)
    }
    def copyToBuffer(s: BufferSeqType, first: Int, count: Int) {
        copy(components, count,
             buffer, first*step + offset, stride,
             s.buffer.asInstanceOf[FloatBuffer], s.offset, s.stride)
    }
}

trait ByteArraySequence extends ArraySequence with ByteSequence {
    def copyToArray(s: ArraySeqType, first: Int, count: Int) {
        copy(components, count,
             array, first*step + offset, stride,
             s.array.asInstanceOf[Array[Byte]], s.offset, s.stride)
    }
    def copyToBuffer(s: BufferSeqType, first: Int, count: Int) {
        copy(components, count,
             array, first*step + offset, stride,
             s.buffer.asInstanceOf[ByteBuffer], s.offset, s.stride)
    }
}

trait ByteBufferSequence extends BufferSequence with ByteSequence {
    def copyToArray(s: ArraySeqType, first: Int, count: Int) {
        copy(components, count,
             buffer, first*step + offset, stride,
             s.array.asInstanceOf[Array[Byte]], s.offset, s.stride)
    }
    def copyToBuffer(s: BufferSeqType, first: Int, count: Int) {
        copy(components, count,
             buffer, first*step + offset, stride,
             s.buffer.asInstanceOf[ByteBuffer], s.offset, s.stride)
    }
}

trait ShortArraySequence extends ArraySequence with ShortSequence {
    def copyToArray(s: ArraySeqType, first: Int, count: Int) {
        copy(components, count,
             array, first*step + offset, stride,
             s.array.asInstanceOf[Array[Short]], s.offset, s.stride)
    }
    def copyToBuffer(s: BufferSeqType, first: Int, count: Int) {
        copy(components, count,
             array, first*step + offset, stride,
             s.buffer.asInstanceOf[ShortBuffer], s.offset, s.stride)
    }
}

trait ShortBufferSequence extends BufferSequence with ShortSequence {
    def copyToArray(s: ArraySeqType, first: Int, count: Int) {
        copy(components, count,
             buffer, first*step + offset, stride,
             s.array.asInstanceOf[Array[Short]], s.offset, s.stride)
    }
    def copyToBuffer(s: BufferSeqType, first: Int, count: Int) {
        copy(components, count,
             buffer, first*step + offset, stride,
             s.buffer.asInstanceOf[ShortBuffer], s.offset, s.stride)
    }
}

trait IntArraySequence extends ArraySequence with IntSequence {
    def copyToArray(s: ArraySeqType, first: Int, count: Int) {
        copy(components, count,
             array, first*step + offset, stride,
             s.array.asInstanceOf[Array[Int]], s.offset, s.stride)
    }
    def copyToBuffer(s: BufferSeqType, first: Int, count: Int) {
        copy(components, count,
             array, first*step + offset, stride,
             s.buffer.asInstanceOf[IntBuffer], s.offset, s.stride)
    }
}

trait IntBufferSequence extends BufferSequence with IntSequence {
    def copyToArray(s: ArraySeqType, first: Int, count: Int) {
        copy(components, count,
             buffer, first*step + offset, stride,
             s.array.asInstanceOf[Array[Int]], s.offset, s.stride)
    }
    def copyToBuffer(s: BufferSeqType, first: Int, count: Int) {
        copy(components, count,
             buffer, first*step + offset, stride,
             s.buffer.asInstanceOf[IntBuffer], s.offset, s.stride)
    }
}
