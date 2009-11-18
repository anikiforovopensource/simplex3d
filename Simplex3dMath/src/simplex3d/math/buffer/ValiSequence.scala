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
sealed abstract class ValiSequence extends DataSequence {
    type E = Int
    type Raw = Int

    override type SeqType <: ValiSequence

    def components = 1

    // Overriding with specified E causes compiler to
    // generate code that does not require unboxing.
    override def apply(i: Int) :E
    override def update(i: Int, v: E)

    override private[buffer] def readRaw(i: Int) :Raw
    override private[buffer] def writeRaw(i: Int, x: Raw) :Unit
}

sealed trait ValiArraySequence extends ValiSequence with ArraySequence
sealed trait ValiBufferSequence extends ValiSequence with BufferSequence

sealed trait ValSignedSequence extends ValiSequence with SignedSequence
sealed trait ValUnsignedSequence extends ValiSequence with UnsignedSequence

sealed abstract class ValUByteSequence
extends ValUnsignedSequence with ByteSequence
{
    def dataType = DataType.UByte

    type SeqType = ValUByteSequence
    type ArraySeqType = ValUByteArray
    type BufferSeqType = ValUByteBuffer

    private[buffer] def makeArraySequence(array: Array[D], stride: Int, offset: Int)
    :ArraySeqType = ValUByteArray(array, stride, offset, normalized)
    private[buffer] def makeBufferSequence(buffer: BufferType, stride: Int,offset:Int)
    :BufferSeqType = ValUByteBuffer(buffer, stride, offset, normalized)
}

final class ValUByteArray private (
        val array: Array[Byte],
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends ValUByteSequence with ByteArraySequence with ValiArraySequence
{
    protected val step = components + stride
    val length = (array.length - offset)/step
    val buffer = ByteBuffer.wrap(array)

    def apply(i: Int) = int(array(i*step + offset)) & 0xFF
    def update(i: Int, x: Int) { array(i*step + offset) = byte(x) }

    private[buffer] def readRaw(i: Int) :Raw = int(array(i)) & 0xFF
    private[buffer] def writeRaw(i: Int, x: Raw) { array(i) = byte(x) }
}

object ValUByteArray {
    def apply(array: Array[Byte], normalized: Boolean) = {
        new ValUByteArray(array, 0, 0, normalized)
    }
    def apply(array: Array[Byte],
              offset: Int, stride: Int, normalized: Boolean) =
    {
        new ValUByteArray(array, offset, stride, normalized)
    }

    def apply(count: Int, normalized: Boolean) = {
        new ValUByteArray(new Array[Byte](count), 0, 0, normalized)
    }

    implicit def arrayToView(array: Array[Byte]) = {
        new ValUByteArray(array, 0, 0, false)
    }
}

final class ValUByteBuffer private (
        val buffer: ByteBuffer,
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends ValUByteSequence with ByteBufferSequence with ValiBufferSequence
{
    protected val step = components + stride
    val length = (buffer.limit - offset)/step

    def apply(i: Int) = int(buffer.get(i*step + offset)) & 0xFF
    def update(i: Int, x: Int) { buffer.put(i*step + offset, byte(x)) }

    private[buffer] def readRaw(i: Int) :Raw = int(buffer.get(i)) & 0xFF
    private[buffer] def writeRaw(i: Int, x: Raw) { buffer.put(i, byte(x)) }
}

object ValUByteBuffer {
    def apply(buf: ByteBuffer, normalized: Boolean) = {
        new ValUByteBuffer(buf, 0, 0, normalized)
    }
    def apply(buf: ByteBuffer, offset: Int, stride: Int, normalized: Boolean) ={
        new ValUByteBuffer(buf, offset, stride, normalized)
    }

    def apply(count: Int, normalized: Boolean) = {
        new ValUByteBuffer(allocateByteBuffer(count), 0, 0, normalized)
    }

    implicit def bufferToView(buf: ByteBuffer) = {
        new ValUByteBuffer(buf, 0, 0, false)
    }
}


// Signed byte
sealed abstract class ValByteSequence
extends ValSignedSequence with ByteSequence
{
    def dataType = DataType.Byte

    type SeqType = ValByteSequence
    type ArraySeqType = ValByteArray
    type BufferSeqType = ValByteBuffer

    private[buffer] def makeArraySequence(array: Array[D], stride: Int, offset: Int)
    :ArraySeqType = ValByteArray(array, stride, offset, normalized)
    private[buffer] def makeBufferSequence(buffer: BufferType, stride: Int,offset:Int)
    :BufferSeqType = ValByteBuffer(buffer, stride, offset, normalized)
}

final class ValByteArray private (
        val array: Array[Byte],
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends ValByteSequence with ByteArraySequence with ValiArraySequence
{
    protected val step = components + stride
    val length = (array.length - offset)/step
    val buffer = ByteBuffer.wrap(array)

    def apply(i: Int) = array(i*step + offset)
    def update(i: Int, x: Int) { array(i*step + offset) = byte(x) }

    private[buffer] def readRaw(i: Int) :Raw = { array(i) }
    private[buffer] def writeRaw(i: Int, x: Raw) { array(i) = byte(x) }
}

object ValByteArray {
    def apply(array: Array[Byte], normalized: Boolean) = {
        new ValByteArray(array, 0, 0, normalized)
    }
    def apply(array: Array[Byte],
              offset: Int, stride: Int, normalized: Boolean) =
    {
        new ValByteArray(array, offset, stride, normalized)
    }

    def apply(count: Int, normalized: Boolean) = {
        new ValByteArray(new Array[Byte](count), 0, 0, normalized)
    }

    implicit def arrayToView(array: Array[Byte]) = {
        new ValByteArray(array, 0, 0, false)
    }
}

final class ValByteBuffer private (
        val buffer: ByteBuffer,
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends ValByteSequence with ByteBufferSequence with ValiBufferSequence
{
    protected val step = components + stride
    val length = (buffer.limit - offset)/step

    def apply(i: Int) = buffer.get(i*step + offset)
    def update(i: Int, x: Int) { buffer.put(i*step + offset, byte(x)) }

    private[buffer] def readRaw(i: Int) :Raw = buffer.get(i)
    private[buffer] def writeRaw(i: Int, x: Raw) { buffer.put(i, byte(x)) }
}

object ValByteBuffer {
    def apply(buf: ByteBuffer, normalized: Boolean) = {
        new ValByteBuffer(buf, 0, 0, normalized)
    }
    def apply(buf: ByteBuffer,
              offset: Int, stride: Int, normalized: Boolean) =
    {
        new ValByteBuffer(buf, offset, stride, normalized)
    }

    def apply(count: Int, normalized: Boolean) = {
        new ValByteBuffer(allocateByteBuffer(count), 0, 0, normalized)
    }

    implicit def bufferToView(buf: ByteBuffer) = {
        new ValByteBuffer(buf, 0, 0, false)
    }
}


// Unsigned short
sealed abstract class ValUShortSequence
extends ValUnsignedSequence with ShortSequence
{
    def dataType = DataType.UShort

    type SeqType = ValUShortSequence
    type ArraySeqType = ValUShortArray
    type BufferSeqType = ValUShortBuffer

    private[buffer] def makeArraySequence(array: Array[D], stride: Int, offset: Int)
    :ArraySeqType = ValUShortArray(array, stride, offset, normalized)
    private[buffer] def makeBufferSequence(buffer: BufferType, stride: Int,offset:Int)
    :BufferSeqType = ValUShortBuffer(buffer, stride, offset, normalized)
}

final class ValUShortArray private (
        val array: Array[Short],
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends ValUShortSequence with ShortArraySequence with ValiArraySequence
{
    protected val step = components + stride
    val length = (array.length - offset)/step
    val buffer = ShortBuffer.wrap(array)

    def apply(i: Int) = int(array(i*step + offset)) & 0xFFFF
    def update(i: Int, x: Int) { array(i*step + offset) = short(x) }

    private[buffer] def readRaw(i: Int) :Raw = int(array(i)) & 0xFFFF
    private[buffer] def writeRaw(i: Int, x: Raw) { array(i) = short(x) }
}

object ValUShortArray {
    def apply(array: Array[Short], normalized: Boolean) = {
        new ValUShortArray(array, 0, 0, normalized)
    }
    def apply(array: Array[Short],
              offset: Int, stride: Int, normalized: Boolean) =
    {
        new ValUShortArray(array, offset, stride, normalized)
    }

    def apply(count: Int, normalized: Boolean) = {
        new ValUShortArray(new Array[Short](count), 0, 0, normalized)
    }

    implicit def arrayToView(array: Array[Short]) = {
        new ValUShortArray(array, 0, 0, false)
    }
}

final class ValUShortBuffer private (
        val buffer: ShortBuffer,
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends ValUShortSequence with ShortBufferSequence with ValiBufferSequence
{
    protected val step = components + stride
    val length = (buffer.limit - offset)/step

    def apply(i: Int) = int(buffer.get(i*step + offset)) & 0xFFFF
    def update(i: Int, x: Int) { buffer.put(i*step + offset, short(x)) }

    private[buffer] def readRaw(i: Int) :Raw = int(buffer.get(i)) & 0xFFFF
    private[buffer] def writeRaw(i: Int, x: Raw) { buffer.put(i, short(x)) }
}

object ValUShortBuffer {
    def apply(buf: ShortBuffer, normalized: Boolean) = {
        new ValUShortBuffer(buf, 0, 0, normalized)
    }
    def apply(buf: ShortBuffer,
              offset: Int, stride: Int, normalized: Boolean) =
    {
        new ValUShortBuffer(buf, offset, stride, normalized)
    }

    def apply(count: Int, normalized: Boolean) = {
        new ValUShortBuffer(allocateShortBuffer(count), 0, 0, normalized)
    }

    implicit def bufferToView(buf: ShortBuffer) = {
        new ValUShortBuffer(buf, 0, 0, false)
    }
}


// Signed short
sealed abstract class ValShortSequence
extends ValSignedSequence with ShortSequence
{
    def dataType = DataType.Short

    type SeqType = ValShortSequence
    type ArraySeqType = ValShortArray
    type BufferSeqType = ValShortBuffer

    private[buffer] def makeArraySequence(array: Array[D], stride: Int, offset: Int)
    :ArraySeqType = ValShortArray(array, stride, offset, normalized)
    private[buffer] def makeBufferSequence(buffer: BufferType, stride: Int,offset:Int)
    :BufferSeqType = ValShortBuffer(buffer, stride, offset, normalized)
}

final class ValShortArray private (
        val array: Array[Short],
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends ValShortSequence with ShortArraySequence with ValiArraySequence
{
    protected val step = components + stride
    val length = (array.length - offset)/step
    val buffer = ShortBuffer.wrap(array)

    def apply(i: Int) = array(i*step + offset)
    def update(i: Int, x: Int) { array(i*step + offset) = short(x) }

    private[buffer] def readRaw(i: Int) :Raw = array(i)
    private[buffer] def writeRaw(i: Int, x: Raw) { array(i) = short(x) }
}

object ValShortArray {
    def apply(array: Array[Short], normalized: Boolean) = {
        new ValShortArray(array, 0, 0, normalized)
    }
    def apply(array: Array[Short],
              offset: Int, stride: Int, normalized: Boolean) =
    {
        new ValShortArray(array, offset, stride, normalized)
    }

    def apply(count: Int, normalized: Boolean) = {
        new ValShortArray(new Array[Short](count), 0, 0, normalized)
    }

    implicit def arrayToView(array: Array[Short]) = {
        new ValShortArray(array, 0, 0, false)
    }
}

final class ValShortBuffer private (
        val buffer: ShortBuffer,
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends ValShortSequence with ShortBufferSequence with ValiBufferSequence
{
    protected val step = components + stride
    val length = (buffer.limit - offset)/step

    def apply(i: Int) = buffer.get(i*step + offset)
    def update(i: Int, x: Int) { buffer.put(i*step + offset, short(x)) }

    private[buffer] def readRaw(i: Int) :Raw = buffer.get(i)
    private[buffer] def writeRaw(i: Int, x: Raw) { buffer.put(i, short(x)) }
}

object ValShortBuffer {
    def apply(buf: ShortBuffer, normalized: Boolean) = {
        new ValShortBuffer(buf, 0, 0, normalized)
    }
    def apply(buf: ShortBuffer,
              offset: Int, stride: Int, normalized: Boolean) =
    {
        new ValShortBuffer(buf, offset, stride, normalized)
    }

    def apply(count: Int, normalized: Boolean) = {
        new ValShortBuffer(allocateShortBuffer(count), 0, 0, normalized)
    }

    implicit def bufferToView(buf: ShortBuffer) = {
        new ValShortBuffer(buf, 0, 0, false)
    }
}


// Signed int
sealed abstract class ValIntSequence
extends ValSignedSequence with IntSequence
{
    def dataType = DataType.Int

    type SeqType = ValIntSequence
    type ArraySeqType = ValIntArray
    type BufferSeqType = ValIntBuffer

    private[buffer] def makeArraySequence(array: Array[D], stride: Int, offset: Int)
    :ArraySeqType = ValIntArray(array, stride, offset, normalized)
    private[buffer] def makeBufferSequence(buffer: BufferType, stride: Int,offset:Int)
    :BufferSeqType = ValIntBuffer(buffer, stride, offset, normalized)
}

final class ValIntArray private (
        val array: Array[Int],
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends ValIntSequence with IntArraySequence with ValiArraySequence
{
    protected val step = components + stride
    val length = (array.length - offset)/stride
    val buffer = IntBuffer.wrap(array)

    def apply(i: Int) = array(i*step + offset)
    def update(i: Int, x: Int) { array(i*step + offset) = x }

    private[buffer] def readRaw(i: Int) :Raw = array(i)
    private[buffer] def writeRaw(i: Int, x: Raw) { array(i) = x }
}

object ValIntArray {
    def apply(array: Array[Int], normalized: Boolean) = {
        new ValIntArray(array, 0, 0, normalized)
    }
    def apply(array: Array[Int],
              offset: Int, stride: Int, normalized: Boolean) =
    {
        new ValIntArray(array, offset, stride, normalized)
    }

    def apply(count: Int, normalized: Boolean) = {
        new ValIntArray(new Array[Int](count), 0, 0, normalized)
    }

    implicit def arrayToView(array: Array[Int]) = {
        new ValIntArray(array, 0, 0, false)
    }
}

final class ValIntBuffer private (
        val buffer: IntBuffer,
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends ValIntSequence with IntBufferSequence with ValiBufferSequence
{
    protected val step = components + stride
    val length = (buffer.limit - offset)/step

    def apply(i: Int) = buffer.get(i*step + offset)
    def update(i: Int, x: Int) { buffer.put(i*step + offset, x) }

    private[buffer] def readRaw(i: Int) :Raw = buffer.get(i)
    private[buffer] def writeRaw(i: Int, x: Raw) { buffer.put(i, x) }
}

object ValIntBuffer {
    def apply(buf: IntBuffer, normalized: Boolean) = {
        new ValIntBuffer(buf, 0, 0, normalized)
    }
    def apply(buf: IntBuffer, offset: Int, stride: Int, normalized: Boolean) = {
        new ValIntBuffer(buf, offset, stride, normalized)
    }

    def apply(count: Int, normalized: Boolean) = {
        new ValIntBuffer(allocateIntBuffer(count), 0, 0, normalized)
    }

    implicit def bufferToView(buf: IntBuffer) = {
        new ValIntBuffer(buf, 0, 0, false)
    }
}
