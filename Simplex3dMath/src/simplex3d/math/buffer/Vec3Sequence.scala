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

import simplex3d.math.VecMath._
import simplex3d.math.ExtendedMath._
import simplex3d.math.buffer.BufferSupport._
import java.nio._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class Vec3Sequence extends DataSequence {
    type E = AnyVec3
    type Raw = Float

    override type SeqType <: Vec3Sequence

    def components = 3

    override private[buffer] def readRaw(i: Int) :Raw
    override private[buffer] def writeRaw(i: Int, x: Raw) :Unit
}

sealed trait Vec3ArraySequence extends Vec3Sequence with ArraySequence
sealed trait Vec3BufferSequence extends Vec3Sequence with BufferSequence

sealed abstract class Vec3FloatSequence extends Vec3Sequence with FloatSequence{
    type SeqType = Vec3FloatSequence
    type ArraySeqType = Vec3FloatArray
    type BufferSeqType = Vec3FloatBuffer

    private[buffer] def makeArraySequence(array: Array[D], stride: Int,
    offset: Int) :ArraySeqType = Vec3FloatArray(array, stride, offset)
    private[buffer] def makeBufferSequence(buffer: BufferType, stride: Int,
    offset:Int) :BufferSeqType = Vec3FloatBuffer(buffer, stride, offset)
}

final class Vec3FloatArray private (
        val array: Array[Float],
        val offset: Int,
        val stride: Int
) extends Vec3FloatSequence with FloatArraySequence with Vec3ArraySequence
{
    protected val step = components + stride
    val length = (array.length - offset)/step
    val buffer = FloatBuffer.wrap(array)

    def apply(i: Int) = {
        val j = i*step + offset
        ConstVec3(
            array(j + 0),
            array(j + 1),
            array(j + 2)
        )
    }
    def update(i: Int, u: AnyVec3) {
        val j = i*step + offset
        array(j + 0) = u.x
        array(j + 1) = u.y
        array(j + 2) = u.z
    }
}

object Vec3FloatArray {
    def apply(array: Array[Float]) = new Vec3FloatArray(array, 0, 0)
    def apply(array: Array[Float], offset: Int, stride: Int) = {
        new Vec3FloatArray(array, offset, stride)
    }
    def apply(vecCount: Int) = {
        new Vec3FloatArray(new Array[Float](vecCount*3), 0, 0)
    }

    implicit def arrayToView(array: Array[Float]) = {
        new Vec3FloatArray(array, 0, 0)
    }
}

final class Vec3FloatBuffer private (
        val buffer: FloatBuffer,
        val offset: Int,
        val stride: Int
) extends Vec3FloatSequence with FloatBufferSequence with Vec3BufferSequence
{
    protected val step = components + stride
    val length = (buffer.limit - offset)/step

    def apply(i: Int) = {
        val j = i*step + offset
        ConstVec3(
            buffer.get(j + 0),
            buffer.get(j + 1),
            buffer.get(j + 2)
        )
    }
    def update(i: Int, u: AnyVec3) {
        val j = i*step + offset
        buffer.put(j + 0, u.x)
        buffer.put(j + 1, u.y)
        buffer.put(j + 2, u.z)
    }
}

object Vec3FloatBuffer {
    def apply(buf: FloatBuffer) = new Vec3FloatBuffer(buf, 0, 0)
    def apply(buf: FloatBuffer, offset: Int, stride: Int) = {
        new Vec3FloatBuffer(buf, offset, stride)
    }

    def apply(vecCount: Int) = {
        new Vec3FloatBuffer(allocateFloatBuffer(vecCount*3), 0, 0)
    }

    implicit def bufferToView(buf: FloatBuffer) = {
        new Vec3FloatBuffer(buf, 0, 0)
    }
}


// Wrappers
private[buffer] trait Vec3WrapSeqUN extends Vec3Sequence {
    val wrapped: Vec3iSequence
    
    def apply(i: Int): ConstVec3 = {
        val j = i*step + offset
        ConstVec3(
            wrapped.readRaw(j + 0) * (1/255f),
            wrapped.readRaw(j + 1) * (1/255f),
            wrapped.readRaw(j + 2) * (1/255f)
        )
    }
    def update(i: Int, u: AnyVec3) {
        val j = i*step + offset
        wrapped.writeRaw(j + 0, clamp(int(u.x * 255), 0, 255))
        wrapped.writeRaw(j + 1, clamp(int(u.y * 255), 0, 255))
        wrapped.writeRaw(j + 2, clamp(int(u.z * 255), 0, 255))
    }

    private[buffer] def readRaw(i: Int) = wrapped.readRaw(i) * (1/255f)
    private[buffer] def writeRaw(i: Int, x: Float) {
        wrapped.writeRaw(i, clamp(int(x * 255), 0, 255))
    }
}

private[buffer] trait Vec3WrapSeqSN extends Vec3Sequence {
    val wrapped: Vec3iSequence

    def apply(i: Int): ConstVec3 = {
        val j = i*step + offset
        ConstVec3(
            clamp(wrapped.readRaw(j + 0) * (1/127f), -1, 1),
            clamp(wrapped.readRaw(j + 1) * (1/127f), -1, 1),
            clamp(wrapped.readRaw(j + 2) * (1/127f), -1, 1)
        )
    }
    def update(i: Int, u: AnyVec3) {
        val j = i*step + offset
        wrapped.writeRaw(j + 0, clamp(int(u.x * 127), -127, 127))
        wrapped.writeRaw(j + 1, clamp(int(u.y * 127), -127, 127))
        wrapped.writeRaw(j + 2, clamp(int(u.z * 127), -127, 127))
    }

    private[buffer] def readRaw(i: Int) = {
        clamp(wrapped.readRaw(i) * (1/127f), -1, 1)
    }
    private[buffer] def writeRaw(i: Int, x: Float) {
        wrapped.writeRaw(i, clamp(int(x * 127), -127, 127))
    }
}

private[buffer] trait Vec3WrapSeqD extends Vec3Sequence {
    val wrapped: Vec3iSequence

    def apply(i: Int): ConstVec3 = {
        val j = i*step + offset
        ConstVec3(
            wrapped.readRaw(j + 0),
            wrapped.readRaw(j + 1),
            wrapped.readRaw(j + 2)
        )
    }
    def update(i: Int, u: AnyVec3) {
        val j = i*step + offset
        wrapped.writeRaw(j + 0, int(u.x))
        wrapped.writeRaw(j + 1, int(u.y))
        wrapped.writeRaw(j + 2, int(u.z))
    }

    private[buffer] def readRaw(i: Int) :Float = wrapped.readRaw(i)
    private[buffer] def writeRaw(i: Int, x: Float) {wrapped.writeRaw(i, int(x))}
}

object Vec3Wrapper {
    def apply(seq: Vec3iSequence) :Vec3WrappedSequence = {
        seq match {
            case s: Vec3iArraySequence => apply(s)
            case s: Vec3iBufferSequence => apply(s)
        }
    }
    def apply(seq: Vec3iArraySequence) :Vec3ArraySequence = {
      if (!seq.normalized) {
        seq match {
            case s: ByteSequence => new Vec3WByteArray(seq) with Vec3WrapSeqD
            case s: ShortSequence => new Vec3WShortArray(seq) with Vec3WrapSeqD
            case s: IntSequence => new Vec3WIntArray(seq) with Vec3WrapSeqD
        }
      }
      else if (seq.isInstanceOf[UnsignedSequence]) {
         seq match {
            case s: ByteSequence => new Vec3WByteArray(seq) with Vec3WrapSeqUN
            case s: ShortSequence => new Vec3WShortArray(seq) with Vec3WrapSeqUN
            case s: IntSequence => new Vec3WIntArray(seq) with Vec3WrapSeqUN
        }
      }
      else {
        seq match {
            case s: ByteSequence => new Vec3WByteArray(seq) with Vec3WrapSeqSN
            case s: ShortSequence => new Vec3WShortArray(seq) with Vec3WrapSeqSN
            case s: IntSequence => new Vec3WIntArray(seq) with Vec3WrapSeqSN
        }
      }
    }
    def apply(seq: Vec3iBufferSequence) :Vec3BufferSequence = {
      if (!seq.normalized) {
        seq match {
            case s: ByteSequence => new Vec3WByteBuffer(seq) with Vec3WrapSeqD
            case s: ShortSequence => new Vec3WShortBuffer(seq) with Vec3WrapSeqD
            case s: IntSequence => new Vec3WIntBuffer(seq) with Vec3WrapSeqD
        }
      }
      else if (seq.isInstanceOf[UnsignedSequence]) {
         seq match {
            case s: ByteSequence => new Vec3WByteBuffer(seq) with Vec3WrapSeqUN
            case s: ShortSequence =>new Vec3WShortBuffer(seq) with Vec3WrapSeqUN
            case s: IntSequence => new Vec3WIntBuffer(seq) with Vec3WrapSeqUN
        }
      }
      else {
        seq match {
            case s: ByteSequence => new Vec3WByteBuffer(seq) with Vec3WrapSeqSN
            case s: ShortSequence =>new Vec3WShortBuffer(seq) with Vec3WrapSeqSN
            case s: IntSequence => new Vec3WIntBuffer(seq) with Vec3WrapSeqSN
        }
      }
    }

    def apply(seq: Vec3UByteArray) :Vec3WByteArray =
    apply(seq: Vec3iArraySequence).asInstanceOf[Vec3WByteArray]

    def apply(seq: Vec3ByteArray) :Vec3WByteArray =
    apply(seq: Vec3iArraySequence).asInstanceOf[Vec3WByteArray]

    def apply(seq: Vec3UShortArray) :Vec3WShortArray =
    apply(seq: Vec3iArraySequence).asInstanceOf[Vec3WShortArray]

    def apply(seq: Vec3ShortArray) :Vec3WShortArray =
    apply(seq: Vec3iArraySequence).asInstanceOf[Vec3WShortArray]

    def apply(seq: Vec3IntArray) :Vec3WIntArray =
    apply(seq: Vec3iArraySequence).asInstanceOf[Vec3WIntArray]

    def apply(seq: Vec3UByteBuffer) :Vec3WByteBuffer =
    apply(seq: Vec3iBufferSequence).asInstanceOf[Vec3WByteBuffer]

    def apply(seq: Vec3ByteBuffer) :Vec3WByteBuffer =
    apply(seq: Vec3iBufferSequence).asInstanceOf[Vec3WByteBuffer]

    def apply(seq: Vec3UShortBuffer) :Vec3WShortBuffer =
    apply(seq: Vec3iBufferSequence).asInstanceOf[Vec3WShortBuffer]

    def apply(seq: Vec3ShortBuffer) :Vec3WShortBuffer =
    apply(seq: Vec3iBufferSequence).asInstanceOf[Vec3WShortBuffer]

    def apply(seq: Vec3IntBuffer) :Vec3WIntBuffer =
    apply(seq: Vec3iBufferSequence).asInstanceOf[Vec3WIntBuffer]
}

sealed abstract class Vec3WrappedSequence extends Vec3Sequence {
    val wrapped: Vec3iSequence

    def dataType = wrapped.dataType
    val offset = wrapped.offset
    def stride = wrapped.stride
    def normalized = wrapped.normalized
    protected val step = components + stride
    def length = wrapped.length
    def buffer = wrapped.buffer.asInstanceOf[BufferType]
}

sealed abstract class Vec3WByteSequence
extends Vec3WrappedSequence with ByteSequence
{
    type SeqType = Vec3WByteSequence
    type ArraySeqType = Vec3WByteArray
    type BufferSeqType = Vec3WByteBuffer

    private[buffer] def makeArraySequence(array: Array[D], stride: Int,
    offset: Int) :ArraySeqType = Vec3Wrapper(
        wrapped.makeArraySequence(array.asInstanceOf[Array[wrapped.D]],
        stride, offset).asInstanceOf[Vec3iArraySequence]
    ).asInstanceOf[Vec3WByteArray]

    private[buffer] def makeBufferSequence(buffer: BufferType, stride: Int,
    offset:Int) :BufferSeqType = Vec3Wrapper(
        wrapped.makeBufferSequence(buffer.asInstanceOf[wrapped.BufferType],
        stride, offset).asInstanceOf[Vec3iBufferSequence]
    ).asInstanceOf[Vec3WByteBuffer]
}

sealed abstract class Vec3WByteArray private[buffer] (
    val wrapped: Vec3iSequence
) extends Vec3WByteSequence with ByteArraySequence with Vec3ArraySequence {
    def array = wrapped.buffer.array.asInstanceOf[Array[D]]
}

sealed abstract class Vec3WByteBuffer private[buffer] (
    val wrapped: Vec3iSequence
) extends Vec3WByteSequence with ByteBufferSequence with Vec3BufferSequence


sealed abstract class Vec3WShortSequence
extends Vec3WrappedSequence with ShortSequence
{
    type SeqType = Vec3WShortSequence
    type ArraySeqType = Vec3WShortArray
    type BufferSeqType = Vec3WShortBuffer

    private[buffer] def makeArraySequence(array: Array[D], stride: Int,
    offset: Int) :ArraySeqType = Vec3Wrapper(
        wrapped.makeArraySequence(array.asInstanceOf[Array[wrapped.D]],
        stride, offset).asInstanceOf[Vec3iArraySequence]
    ).asInstanceOf[Vec3WShortArray]

    private[buffer] def makeBufferSequence(buffer: BufferType, stride: Int,
    offset:Int) :BufferSeqType = Vec3Wrapper(
        wrapped.makeBufferSequence(buffer.asInstanceOf[wrapped.BufferType],
        stride, offset).asInstanceOf[Vec3iBufferSequence]
    ).asInstanceOf[Vec3WShortBuffer]
}

sealed abstract class Vec3WShortArray private[buffer] (
    val wrapped: Vec3iSequence
) extends Vec3WShortSequence with ShortArraySequence with Vec3ArraySequence {
    def array = wrapped.buffer.array.asInstanceOf[Array[D]]
}

sealed abstract class Vec3WShortBuffer private[buffer] (
    val wrapped: Vec3iSequence
) extends Vec3WShortSequence with ShortBufferSequence with Vec3BufferSequence


sealed abstract class Vec3WIntSequence
extends Vec3WrappedSequence with IntSequence
{
    type SeqType = Vec3WIntSequence
    type ArraySeqType = Vec3WIntArray
    type BufferSeqType = Vec3WIntBuffer

    private[buffer] def makeArraySequence(array: Array[D], stride: Int,
    offset: Int) :ArraySeqType = Vec3Wrapper(
        wrapped.makeArraySequence(array.asInstanceOf[Array[wrapped.D]],
        stride, offset).asInstanceOf[Vec3iArraySequence]
    ).asInstanceOf[Vec3WIntArray]

    private[buffer] def makeBufferSequence(buffer: BufferType, stride: Int,
    offset:Int) :BufferSeqType = Vec3Wrapper(
        wrapped.makeBufferSequence(buffer.asInstanceOf[wrapped.BufferType],
        stride, offset).asInstanceOf[Vec3iBufferSequence]
    ).asInstanceOf[Vec3WIntBuffer]
}

sealed abstract class Vec3WIntArray private[buffer] (
    val wrapped: Vec3iSequence
) extends Vec3WIntSequence with IntArraySequence with Vec3ArraySequence {
    def array = wrapped.buffer.array.asInstanceOf[Array[D]]
}

sealed abstract class Vec3WIntBuffer private[buffer] (
    val wrapped: Vec3iSequence
) extends Vec3WIntSequence with IntBufferSequence with Vec3BufferSequence
