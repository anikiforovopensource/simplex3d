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
sealed abstract class Vec2iSequence extends DataSequence {
    type E = AnyVec2i
    type Raw = Int

    override type SeqType <: Vec2iSequence

    def components = 2

    override private[buffer] def readRaw(i: Int) :Raw
    override private[buffer] def writeRaw(i: Int, x: Raw) :Unit
}

sealed trait Vec2iArraySequence extends Vec2iSequence with ArraySequence
sealed trait Vec2iBufferSequence extends Vec2iSequence with BufferSequence

sealed trait Vec2SignedSequence extends Vec2iSequence with SignedSequence
sealed trait Vec2UnsignedSequence extends Vec2iSequence with UnsignedSequence

sealed abstract class Vec2UByteSequence
extends Vec2UnsignedSequence with ByteSequence
{
    def dataType = DataType.UByte

    type SeqType = Vec2UByteSequence
    type ArraySeqType = Vec2UByteArray
    type BufferSeqType = Vec2UByteBuffer

    private[buffer] def makeArraySequence(array: Array[D], stride: Int, offset: Int)
    :ArraySeqType = Vec2UByteArray(array, stride, offset, normalized)
    private[buffer] def makeBufferSequence(buffer: BufferType, stride: Int,offset:Int)
    :BufferSeqType = Vec2UByteBuffer(buffer, stride, offset, normalized)
}

final class Vec2UByteArray private (
        val array: Array[Byte],
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends Vec2UByteSequence with ByteArraySequence with Vec2iArraySequence
{
    protected val step = components + stride
    val length = (array.length - offset)/step
    val buffer = ByteBuffer.wrap(array)

    def apply(i: Int) = {
        val j = i*step + offset
        ConstVec2i(
            int(array(j + 0)) & 0xFF,
            int(array(j + 1)) & 0xFF
        )
    }
    def update(i: Int, u: AnyVec2i) {
        val j = i*step + offset
        array(j + 0) = byte(u.x)
        array(j + 1) = byte(u.y)
    }

    private[buffer] def readRaw(i: Int) :Raw = int(array(i)) & 0xFF
    private[buffer] def writeRaw(i: Int, x: Raw) { array(i) = byte(x) }
}

object Vec2UByteArray {
    def apply(array: Array[Byte], normalized: Boolean) = {
        new Vec2UByteArray(array, 0, 0, normalized)
    }
    def apply(array: Array[Byte],
              offset: Int, stride: Int, normalized: Boolean) =
    {
        new Vec2UByteArray(array, offset, stride, normalized)
    }

    def apply(vecCount: Int, normalized: Boolean) = {
        new Vec2UByteArray(new Array[Byte](vecCount*2), 0, 0, normalized)
    }

    implicit def arrayToView(array: Array[Byte]) = {
        new Vec2UByteArray(array, 0, 0, false)
    }
}

final class Vec2UByteBuffer private (
        val buffer: ByteBuffer,
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends Vec2UByteSequence with ByteBufferSequence with Vec2iBufferSequence
{
    protected val step = components + stride
    val length = (buffer.limit - offset)/step

    def apply(i: Int) = {
        val j = i*step + offset
        ConstVec2i(
            int(buffer.get(j + 0)) & 0xFF,
            int(buffer.get(j + 1)) & 0xFF
        )
    }
    def update(i: Int, u: AnyVec2i) {
        val j = i*step + offset
        buffer.put(j + 0, byte(u.x))
        buffer.put(j + 1, byte(u.y))
    }

    private[buffer] def readRaw(i: Int) :Raw = int(buffer.get(i)) & 0xFF
    private[buffer] def writeRaw(i: Int, x: Raw) { buffer.put(i, byte(x)) }
}

object Vec2UByteBuffer {
    def apply(buf: ByteBuffer, normalized: Boolean) = {
        new Vec2UByteBuffer(buf, 0, 0, normalized)
    }
    def apply(buf: ByteBuffer, offset: Int, stride: Int, normalized: Boolean) ={
        new Vec2UByteBuffer(buf, offset, stride, normalized)
    }

    def apply(vecCount: Int, normalized: Boolean) = {
        new Vec2UByteBuffer(allocateByteBuffer(vecCount*2), 0, 0, normalized)
    }

    implicit def bufferToView(buf: ByteBuffer) = {
        new Vec2UByteBuffer(buf, 0, 0, false)
    }
}


// Signed byte
sealed abstract class Vec2ByteSequence
extends Vec2SignedSequence with ByteSequence
{
    def dataType = DataType.Byte

    type SeqType = Vec2ByteSequence
    type ArraySeqType = Vec2ByteArray
    type BufferSeqType = Vec2ByteBuffer

    private[buffer] def makeArraySequence(array: Array[D], stride: Int, offset: Int)
    :ArraySeqType = Vec2ByteArray(array, stride, offset, normalized)
    private[buffer] def makeBufferSequence(buffer: BufferType, stride: Int,offset:Int)
    :BufferSeqType = Vec2ByteBuffer(buffer, stride, offset, normalized)
}

final class Vec2ByteArray private (
        val array: Array[Byte],
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends Vec2ByteSequence with ByteArraySequence with Vec2iArraySequence
{
    protected val step = components + stride
    val length = (array.length - offset)/step
    val buffer = ByteBuffer.wrap(array)

    def apply(i: Int) = {
        val j = i*step + offset
        ConstVec2i(
            array(j + 0),
            array(j + 1)
        )
    }
    def update(i: Int, u: AnyVec2i) {
        val j = i*step + offset
        array(j + 0) = byte(u.x)
        array(j + 1) = byte(u.y)
    }

    private[buffer] def readRaw(i: Int) :Raw = { array(i) }
    private[buffer] def writeRaw(i: Int, x: Raw) { array(i) = byte(x) }
}

object Vec2ByteArray {
    def apply(array: Array[Byte], normalized: Boolean) = {
        new Vec2ByteArray(array, 0, 0, normalized)
    }
    def apply(array: Array[Byte],
              offset: Int, stride: Int, normalized: Boolean) =
    {
        new Vec2ByteArray(array, offset, stride, normalized)
    }

    def apply(vecCount: Int, normalized: Boolean) = {
        new Vec2ByteArray(new Array[Byte](vecCount*2), 0, 0, normalized)
    }

    implicit def arrayToView(array: Array[Byte]) = {
        new Vec2ByteArray(array, 0, 0, false)
    }
}

final class Vec2ByteBuffer private (
        val buffer: ByteBuffer,
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends Vec2ByteSequence with ByteBufferSequence with Vec2iBufferSequence
{
    protected val step = components + stride
    val length = (buffer.limit - offset)/step

    def apply(i: Int) = {
        val j = i*step + offset
        ConstVec2i(
            buffer.get(j + 0),
            buffer.get(j + 1)
        )
    }
    def update(i: Int, u: AnyVec2i) {
        val j = i*step + offset
        buffer.put(j + 0, byte(u.x))
        buffer.put(j + 1, byte(u.y))
    }

    private[buffer] def readRaw(i: Int) :Raw = buffer.get(i)
    private[buffer] def writeRaw(i: Int, x: Raw) { buffer.put(i, byte(x)) }
}

object Vec2ByteBuffer {
    def apply(buf: ByteBuffer, normalized: Boolean) = {
        new Vec2ByteBuffer(buf, 0, 0, normalized)
    }
    def apply(buf: ByteBuffer, offset: Int, stride: Int, normalized: Boolean) ={
        new Vec2ByteBuffer(buf, offset, stride, normalized)
    }

    def apply(vecCount: Int, normalized: Boolean) = {
        new Vec2ByteBuffer(allocateByteBuffer(vecCount*2), 0, 0, normalized)
    }

    implicit def bufferToView(buf: ByteBuffer) = {
        new Vec2ByteBuffer(buf, 0, 0, false)
    }
}


// Unsigned short
sealed abstract class Vec2UShortSequence
extends Vec2UnsignedSequence with ShortSequence
{
    def dataType = DataType.UShort

    type SeqType = Vec2UShortSequence
    type ArraySeqType = Vec2UShortArray
    type BufferSeqType = Vec2UShortBuffer

    private[buffer] def makeArraySequence(array: Array[D], stride: Int, offset: Int)
    :ArraySeqType = Vec2UShortArray(array, stride, offset, normalized)
    private[buffer] def makeBufferSequence(buffer: BufferType, stride: Int,offset:Int)
    :BufferSeqType = Vec2UShortBuffer(buffer, stride, offset, normalized)
}

final class Vec2UShortArray private (
        val array: Array[Short],
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends Vec2UShortSequence with ShortArraySequence with Vec2iArraySequence
{
    protected val step = components + stride
    val length = (array.length - offset)/step
    val buffer = ShortBuffer.wrap(array)

    def apply(i: Int) = {
        val j = i*step + offset
        ConstVec2i(
            int(array(j + 0)) & 0xFFFF,
            int(array(j + 1)) & 0xFFFF
        )
    }
    def update(i: Int, u: AnyVec2i) {
        val j = i*step + offset
        array(j + 0) = short(u.x)
        array(j + 1) = short(u.y)
    }

    private[buffer] def readRaw(i: Int) :Raw = int(array(i)) & 0xFFFF
    private[buffer] def writeRaw(i: Int, x: Raw) { array(i) = short(x) }
}

object Vec2UShortArray {
    def apply(array: Array[Short], normalized: Boolean) = {
        new Vec2UShortArray(array, 0, 0, normalized)
    }
    def apply(array: Array[Short],
              offset: Int, stride: Int, normalized: Boolean) =
    {
        new Vec2UShortArray(array, offset, stride, normalized)
    }

    def apply(vecCount: Int, normalized: Boolean) = {
        new Vec2UShortArray(new Array[Short](vecCount*2), 0, 0, normalized)
    }

    implicit def arrayToView(array: Array[Short]) = {
        new Vec2UShortArray(array, 0, 0, false)
    }
}

final class Vec2UShortBuffer private (
        val buffer: ShortBuffer,
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends Vec2UShortSequence with ShortBufferSequence with Vec2iBufferSequence
{
    protected val step = components + stride
    val length = (buffer.limit - offset)/step

    def apply(i: Int) = {
        val j = i*step + offset
        ConstVec2i(
            int(buffer.get(j + 0)) & 0xFFFF,
            int(buffer.get(j + 1)) & 0xFFFF
        )
    }
    def update(i: Int, u: AnyVec2i) {
        val j = i*step + offset
        buffer.put(j + 0, short(u.x))
        buffer.put(j + 1, short(u.y))
    }

    private[buffer] def readRaw(i: Int) :Raw = int(buffer.get(i)) & 0xFFFF
    private[buffer] def writeRaw(i: Int, x: Raw) { buffer.put(i, short(x)) }
}

object Vec2UShortBuffer {
    def apply(buf: ShortBuffer, normalized: Boolean) = {
        new Vec2UShortBuffer(buf, 0, 0, normalized)
    }
    def apply(buf: ShortBuffer,
              offset: Int, stride: Int, normalized: Boolean) =
    {
        new Vec2UShortBuffer(buf, offset, stride, normalized)
    }

    def apply(vecCount: Int, normalized: Boolean) = {
        new Vec2UShortBuffer(allocateShortBuffer(vecCount*2), 0, 0, normalized)
    }

    implicit def bufferToView(buf: ShortBuffer) = {
        new Vec2UShortBuffer(buf, 0, 0, false)
    }
}


// Signed short
sealed abstract class Vec2ShortSequence
extends Vec2SignedSequence with ShortSequence
{
    def dataType = DataType.Short

    type SeqType = Vec2ShortSequence
    type ArraySeqType = Vec2ShortArray
    type BufferSeqType = Vec2ShortBuffer

    private[buffer] def makeArraySequence(array: Array[D], stride: Int, offset: Int)
    :ArraySeqType = Vec2ShortArray(array, stride, offset, normalized)
    private[buffer] def makeBufferSequence(buffer: BufferType, stride: Int,offset:Int)
    :BufferSeqType = Vec2ShortBuffer(buffer, stride, offset, normalized)
}

final class Vec2ShortArray private (
        val array: Array[Short],
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends Vec2ShortSequence with ShortArraySequence with Vec2iArraySequence
{
    protected val step = components + stride
    val length = (array.length - offset)/step
    val buffer = ShortBuffer.wrap(array)

    def apply(i: Int) = {
        val j = i*step + offset
        ConstVec2i(
            array(j + 0),
            array(j + 1)
        )
    }
    def update(i: Int, u: AnyVec2i) {
        val j = i*step + offset
        array(j + 0) = short(u.x)
        array(j + 1) = short(u.y)
    }

    private[buffer] def readRaw(i: Int) :Raw = array(i)
    private[buffer] def writeRaw(i: Int, x: Raw) { array(i) = short(x) }
}

object Vec2ShortArray {
    def apply(array: Array[Short], normalized: Boolean) = {
        new Vec2ShortArray(array, 0, 0, normalized)
    }
    def apply(array: Array[Short],
              offset: Int, stride: Int, normalized: Boolean) =
    {
        new Vec2ShortArray(array, offset, stride, normalized)
    }

    def apply(vecCount: Int, normalized: Boolean) = {
        new Vec2ShortArray(new Array[Short](vecCount*2), 0, 0, normalized)
    }

    implicit def arrayToView(array: Array[Short]) = {
        new Vec2ShortArray(array, 0, 0, false)
    }
}

final class Vec2ShortBuffer private (
        val buffer: ShortBuffer,
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends Vec2ShortSequence with ShortBufferSequence with Vec2iBufferSequence
{
    protected val step = components + stride
    val length = (buffer.limit - offset)/step

    def apply(i: Int) = {
        val j = i*step + offset
        ConstVec2i(
            buffer.get(j + 0),
            buffer.get(j + 1)
        )
    }
    def update(i: Int, u: AnyVec2i) {
        val j = i*step + offset
        buffer.put(j + 0, short(u.x))
        buffer.put(j + 1, short(u.y))
    }

    private[buffer] def readRaw(i: Int) :Raw = buffer.get(i)
    private[buffer] def writeRaw(i: Int, x: Raw) { buffer.put(i, short(x)) }
}

object Vec2ShortBuffer {
    def apply(buf: ShortBuffer, normalized: Boolean) = {
        new Vec2ShortBuffer(buf, 0, 0, normalized)
    }
    def apply(buf: ShortBuffer,
              offset: Int, stride: Int, normalized: Boolean) =
    {
        new Vec2ShortBuffer(buf, offset, stride, normalized)
    }

    def apply(vecCount: Int, normalized: Boolean) = {
        new Vec2ShortBuffer(allocateShortBuffer(vecCount*2), 0, 0, normalized)
    }

    implicit def bufferToView(buf: ShortBuffer) = {
        new Vec2ShortBuffer(buf, 0, 0, false)
    }
}


// Signed int
sealed abstract class Vec2IntSequence
extends Vec2SignedSequence with IntSequence
{
    def dataType = DataType.Int
    
    type SeqType = Vec2IntSequence
    type ArraySeqType = Vec2IntArray
    type BufferSeqType = Vec2IntBuffer

    private[buffer] def makeArraySequence(array: Array[D], stride: Int, offset: Int)
    :ArraySeqType = Vec2IntArray(array, stride, offset, normalized)
    private[buffer] def makeBufferSequence(buffer: BufferType, stride: Int,offset:Int)
    :BufferSeqType = Vec2IntBuffer(buffer, stride, offset, normalized)
}

final class Vec2IntArray private (
        val array: Array[Int],
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends Vec2IntSequence with IntArraySequence with Vec2iArraySequence
{
    protected val step = components + stride
    val length = (array.length - offset)/step
    val buffer = IntBuffer.wrap(array)

    def apply(i: Int) = {
        val j = i*step + offset
        ConstVec2i(
            array(j + 0),
            array(j + 1)
        )
    }
    def update(i: Int, u: AnyVec2i) {
        val j = i*step + offset
        array(j + 0) = u.x
        array(j + 1) = u.y
    }

    private[buffer] def readRaw(i: Int) :Raw = array(i)
    private[buffer] def writeRaw(i: Int, x: Raw) { array(i) = x }
}

object Vec2IntArray {
    def apply(array: Array[Int], normalized: Boolean) = {
        new Vec2IntArray(array, 0, 0, normalized)
    }
    def apply(array: Array[Int],
              offset: Int, stride: Int, normalized: Boolean) =
    {
        new Vec2IntArray(array, offset, stride, normalized)
    }

    def apply(vecCount: Int, normalized: Boolean) = {
        new Vec2IntArray(new Array[Int](vecCount*2), 0, 0, normalized)
    }

    implicit def arrayToView(array: Array[Int]) = {
        new Vec2IntArray(array, 0, 0, false)
    }
}

final class Vec2IntBuffer private (
        val buffer: IntBuffer,
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends Vec2IntSequence with IntBufferSequence with Vec2iBufferSequence
{
    protected val step = components + stride
    val length = (buffer.limit - offset)/step

    def apply(i: Int) = {
        val j = i*step + offset
        ConstVec2i(
            buffer.get(j + 0),
            buffer.get(j + 1)
        )
    }
    def update(i: Int, u: AnyVec2i) {
        val j = i*step + offset
        buffer.put(j + 0, u.x)
        buffer.put(j + 1, u.y)
    }

    private[buffer] def readRaw(i: Int) :Raw = buffer.get(i)
    private[buffer] def writeRaw(i: Int, x: Raw) { buffer.put(i, x) }
}

object Vec2IntBuffer {
    def apply(buf: IntBuffer, normalized: Boolean) = {
        new Vec2IntBuffer(buf, 0, 0, normalized)
    }
    def apply(buf: IntBuffer, offset: Int, stride: Int, normalized: Boolean) = {
        new Vec2IntBuffer(buf, offset, stride, normalized)
    }

    def apply(vecCount: Int, normalized: Boolean) = {
        new Vec2IntBuffer(allocateIntBuffer(vecCount*2), 0, 0, normalized)
    }

    implicit def bufferToView(buf: IntBuffer) = {
        new Vec2IntBuffer(buf, 0, 0, false)
    }
}
