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
sealed abstract class Vec4iSequence extends DataSequence {
    type E = AnyVec4i
    type Raw = Int

    override type SeqType <: Vec4iSequence

    def components = 4

    override private[buffer] def readRaw(i: Int) :Raw
    override private[buffer] def writeRaw(i: Int, x: Raw) :Unit
}

sealed trait Vec4iArraySequence extends Vec4iSequence with ArraySequence
sealed trait Vec4iBufferSequence extends Vec4iSequence with BufferSequence

sealed trait Vec4SignedSequence extends Vec4iSequence with SignedSequence
sealed trait Vec4UnsignedSequence extends Vec4iSequence with UnsignedSequence

sealed abstract class Vec4UByteSequence
extends Vec4UnsignedSequence with ByteSequence
{
    def dataType = DataType.UByte

    type SeqType = Vec4UByteSequence
    type ArraySeqType = Vec4UByteArray
    type BufferSeqType = Vec4UByteBuffer

    private[buffer] def makeArraySequence(array: Array[D], stride: Int, offset: Int)
    :ArraySeqType = Vec4UByteArray(array, stride, offset, normalized)
    private[buffer] def makeBufferSequence(buffer: BufferType, stride: Int,offset:Int)
    :BufferSeqType = Vec4UByteBuffer(buffer, stride, offset, normalized)
}

final class Vec4UByteArray private (
        val array: Array[Byte],
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends Vec4UByteSequence with ByteArraySequence with Vec4iArraySequence
{
    protected val step = components + stride
    val length = (array.length - offset)/step
    val buffer = ByteBuffer.wrap(array)

    def apply(i: Int) = {
        val j = i*step + offset
        ConstVec4i(
            int(array(j + 0)) & 0xFF,
            int(array(j + 1)) & 0xFF,
            int(array(j + 2)) & 0xFF,
            int(array(j + 3)) & 0xFF
        )
    }
    def update(i: Int, u: AnyVec4i) {
        val j = i*step + offset
        array(j + 0) = byte(u.x)
        array(j + 1) = byte(u.y)
        array(j + 2) = byte(u.z)
        array(j + 3) = byte(u.w)
    }

    private[buffer] def readRaw(i: Int) :Raw = int(array(i)) & 0xFF
    private[buffer] def writeRaw(i: Int, x: Raw) { array(i) = byte(x) }
}

object Vec4UByteArray {
    def apply(array: Array[Byte], normalized: Boolean) = {
        new Vec4UByteArray(array, 0, 0, normalized)
    }
    def apply(array: Array[Byte],
              offset: Int, stride: Int, normalized: Boolean) =
    {
        new Vec4UByteArray(array, offset, stride, normalized)
    }

    def apply(vecCount: Int, normalized: Boolean) = {
        new Vec4UByteArray(new Array[Byte](vecCount*4), 0, 0, normalized)
    }

    implicit def arrayToView(array: Array[Byte]) = {
        new Vec4UByteArray(array, 0, 0, false)
    }
}

final class Vec4UByteBuffer private (
        val buffer: ByteBuffer,
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends Vec4UByteSequence with ByteBufferSequence with Vec4iBufferSequence
{
    protected val step = components + stride
    val length = (buffer.limit - offset)/step

    def apply(i: Int) = {
        val j = i*step + offset
        ConstVec4i(
            int(buffer.get(j + 0)) & 0xFF,
            int(buffer.get(j + 1)) & 0xFF,
            int(buffer.get(j + 2)) & 0xFF,
            int(buffer.get(j + 3)) & 0xFF
        )
    }
    def update(i: Int, u: AnyVec4i) {
        val j = i*step + offset
        buffer.put(j + 0, byte(u.x))
        buffer.put(j + 1, byte(u.y))
        buffer.put(j + 2, byte(u.z))
        buffer.put(j + 3, byte(u.w))
    }

    private[buffer] def readRaw(i: Int) :Raw = int(buffer.get(i)) & 0xFF
    private[buffer] def writeRaw(i: Int, x: Raw) { buffer.put(i, byte(x)) }
}

object Vec4UByteBuffer {
    def apply(buf: ByteBuffer, normalized: Boolean) = {
        new Vec4UByteBuffer(buf, 0, 0, normalized)
    }
    def apply(buf: ByteBuffer, offset: Int, stride: Int, normalized: Boolean) ={
        new Vec4UByteBuffer(buf, offset, stride, normalized)
    }

    def apply(vecCount: Int, normalized: Boolean) = {
        new Vec4UByteBuffer(allocateByteBuffer(vecCount*4), 0, 0, normalized)
    }

    implicit def bufferToView(buf: ByteBuffer) = {
        new Vec4UByteBuffer(buf, 0, 0, false)
    }
}


// Signed byte
sealed abstract class Vec4ByteSequence
extends Vec4SignedSequence with ByteSequence
{
    def dataType = DataType.Byte

    type SeqType = Vec4ByteSequence
    type ArraySeqType = Vec4ByteArray
    type BufferSeqType = Vec4ByteBuffer

    private[buffer] def makeArraySequence(array: Array[D], stride: Int, offset: Int)
    :ArraySeqType = Vec4ByteArray(array, stride, offset, normalized)
    private[buffer] def makeBufferSequence(buffer: BufferType, stride: Int,offset:Int)
    :BufferSeqType = Vec4ByteBuffer(buffer, stride, offset, normalized)
}

final class Vec4ByteArray private (
        val array: Array[Byte],
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends Vec4ByteSequence with ByteArraySequence with Vec4iArraySequence
{
    protected val step = components + stride
    val length = (array.length - offset)/step
    val buffer = ByteBuffer.wrap(array)

    def apply(i: Int) = {
        val j = i*step + offset
        ConstVec4i(
            array(j + 0),
            array(j + 1),
            array(j + 2),
            array(j + 3)
        )
    }
    def update(i: Int, u: AnyVec4i) {
        val j = i*step + offset
        array(j + 0) = byte(u.x)
        array(j + 1) = byte(u.y)
        array(j + 2) = byte(u.z)
        array(j + 3) = byte(u.w)
    }

    private[buffer] def readRaw(i: Int) :Raw = { array(i) }
    private[buffer] def writeRaw(i: Int, x: Raw) { array(i) = byte(x) }
}

object Vec4ByteArray {
    def apply(array: Array[Byte], normalized: Boolean) = {
        new Vec4ByteArray(array, 0, 0, normalized)
    }
    def apply(array: Array[Byte],
              offset: Int, stride: Int, normalized: Boolean) =
    {
        new Vec4ByteArray(array, offset, stride, normalized)
    }

    def apply(vecCount: Int, normalized: Boolean) = {
        new Vec4ByteArray(new Array[Byte](vecCount*4), 0, 0, normalized)
    }

    implicit def arrayToView(array: Array[Byte]) = {
        new Vec4ByteArray(array, 0, 0, false)
    }
}

final class Vec4ByteBuffer private (
        val buffer: ByteBuffer,
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends Vec4ByteSequence with ByteBufferSequence with Vec4iBufferSequence
{
    protected val step = components + stride
    val length = (buffer.limit - offset)/step

    def apply(i: Int) = {
        val j = i*step + offset
        ConstVec4i(
            buffer.get(j + 0),
            buffer.get(j + 1),
            buffer.get(j + 2),
            buffer.get(j + 3)
        )
    }
    def update(i: Int, u: AnyVec4i) {
        val j = i*step + offset
        buffer.put(j + 0, byte(u.x))
        buffer.put(j + 1, byte(u.y))
        buffer.put(j + 2, byte(u.z))
        buffer.put(j + 3, byte(u.w))
    }

    private[buffer] def readRaw(i: Int) :Raw = buffer.get(i)
    private[buffer] def writeRaw(i: Int, x: Raw) { buffer.put(i, byte(x)) }
}

object Vec4ByteBuffer {
    def apply(buf: ByteBuffer, normalized: Boolean) = {
        new Vec4ByteBuffer(buf, 0, 0, normalized)
    }
    def apply(buf: ByteBuffer, offset: Int, stride: Int, normalized: Boolean) ={
        new Vec4ByteBuffer(buf, offset, stride, normalized)
    }

    def apply(vecCount: Int, normalized: Boolean) = {
        new Vec4ByteBuffer(allocateByteBuffer(vecCount*4), 0, 0, normalized)
    }

    implicit def bufferToView(buf: ByteBuffer) = {
        new Vec4ByteBuffer(buf, 0, 0, false)
    }
}


// Unsigned short
sealed abstract class Vec4UShortSequence
extends Vec4UnsignedSequence with ShortSequence
{
    def dataType = DataType.UShort

    type SeqType = Vec4UShortSequence
    type ArraySeqType = Vec4UShortArray
    type BufferSeqType = Vec4UShortBuffer

    private[buffer] def makeArraySequence(array: Array[D], stride: Int, offset: Int)
    :ArraySeqType = Vec4UShortArray(array, stride, offset, normalized)
    private[buffer] def makeBufferSequence(buffer: BufferType, stride: Int,offset:Int)
    :BufferSeqType = Vec4UShortBuffer(buffer, stride, offset, normalized)
}

final class Vec4UShortArray private (
        val array: Array[Short],
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends Vec4UShortSequence with ShortArraySequence with Vec4iArraySequence
{
    protected val step = components + stride
    val length = (array.length - offset)/step
    val buffer = ShortBuffer.wrap(array)

    def apply(i: Int) = {
        val j = i*step + offset
        ConstVec4i(
            int(array(j + 0)) & 0xFFFF,
            int(array(j + 1)) & 0xFFFF,
            int(array(j + 2)) & 0xFFFF,
            int(array(j + 3)) & 0xFFFF
        )
    }
    def update(i: Int, u: AnyVec4i) {
        val j = i*step + offset
        array(j + 0) = short(u.x)
        array(j + 1) = short(u.y)
        array(j + 2) = short(u.z)
        array(j + 3) = short(u.w)
    }

    private[buffer] def readRaw(i: Int) :Raw = int(array(i)) & 0xFFFF
    private[buffer] def writeRaw(i: Int, x: Raw) { array(i) = short(x) }
}

object Vec4UShortArray {
    def apply(array: Array[Short], normalized: Boolean) = {
        new Vec4UShortArray(array, 0, 0, normalized)
    }
    def apply(array: Array[Short],
              offset: Int, stride: Int, normalized: Boolean) =
    {
        new Vec4UShortArray(array, offset, stride, normalized)
    }

    def apply(vecCount: Int, normalized: Boolean) = {
        new Vec4UShortArray(new Array[Short](vecCount*4), 0, 0, normalized)
    }

    implicit def arrayToView(array: Array[Short]) = {
        new Vec4UShortArray(array, 0, 0, false)
    }
}

final class Vec4UShortBuffer private (
        val buffer: ShortBuffer,
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends Vec4UShortSequence with ShortBufferSequence with Vec4iBufferSequence
{
    protected val step = components + stride
    val length = (buffer.limit - offset)/step

    def apply(i: Int) = {
        val j = i*step + offset
        ConstVec4i(
            int(buffer.get(j + 0)) & 0xFFFF,
            int(buffer.get(j + 1)) & 0xFFFF,
            int(buffer.get(j + 2)) & 0xFFFF,
            int(buffer.get(j + 3)) & 0xFFFF
        )
    }
    def update(i: Int, u: AnyVec4i) {
        val j = i*step + offset
        buffer.put(j + 0, short(u.x))
        buffer.put(j + 1, short(u.y))
        buffer.put(j + 2, short(u.z))
        buffer.put(j + 3, short(u.w))
    }

    private[buffer] def readRaw(i: Int) :Raw = int(buffer.get(i)) & 0xFFFF
    private[buffer] def writeRaw(i: Int, x: Raw) { buffer.put(i, short(x)) }
}

object Vec4UShortBuffer {
    def apply(buf: ShortBuffer, normalized: Boolean) = {
        new Vec4UShortBuffer(buf, 0, 0, normalized)
    }
    def apply(buf: ShortBuffer,
              offset: Int, stride: Int, normalized: Boolean) =
    {
        new Vec4UShortBuffer(buf, offset, stride, normalized)
    }

    def apply(vecCount: Int, normalized: Boolean) = {
        new Vec4UShortBuffer(allocateShortBuffer(vecCount*4), 0, 0, normalized)
    }

    implicit def bufferToView(buf: ShortBuffer) = {
        new Vec4UShortBuffer(buf, 0, 0, false)
    }
}


// Signed short
sealed abstract class Vec4ShortSequence
extends Vec4SignedSequence with ShortSequence
{
    def dataType = DataType.Short

    type SeqType = Vec4ShortSequence
    type ArraySeqType = Vec4ShortArray
    type BufferSeqType = Vec4ShortBuffer

    private[buffer] def makeArraySequence(array: Array[D], stride: Int, offset: Int)
    :ArraySeqType = Vec4ShortArray(array, stride, offset, normalized)
    private[buffer] def makeBufferSequence(buffer: BufferType, stride: Int,offset:Int)
    :BufferSeqType = Vec4ShortBuffer(buffer, stride, offset, normalized)
}

final class Vec4ShortArray private (
        val array: Array[Short],
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends Vec4ShortSequence with ShortArraySequence with Vec4iArraySequence
{
    protected val step = components + stride
    val length = (array.length - offset)/step
    val buffer = ShortBuffer.wrap(array)

    def apply(i: Int) = {
        val j = i*step + offset
        ConstVec4i(
            array(j + 0),
            array(j + 1),
            array(j + 2),
            array(j + 3)
        )
    }
    def update(i: Int, u: AnyVec4i) {
        val j = i*step + offset
        array(j + 0) = short(u.x)
        array(j + 1) = short(u.y)
        array(j + 2) = short(u.z)
        array(j + 3) = short(u.w)
    }

    private[buffer] def readRaw(i: Int) :Raw = array(i)
    private[buffer] def writeRaw(i: Int, x: Raw) { array(i) = short(x) }
}

object Vec4ShortArray {
    def apply(array: Array[Short], normalized: Boolean) = {
        new Vec4ShortArray(array, 0, 0, normalized)
    }
    def apply(array: Array[Short],
              offset: Int, stride: Int, normalized: Boolean) =
    {
        new Vec4ShortArray(array, offset, stride, normalized)
    }

    def apply(vecCount: Int, normalized: Boolean) = {
        new Vec4ShortArray(new Array[Short](vecCount*4), 0, 0, normalized)
    }

    implicit def arrayToView(array: Array[Short]) = {
        new Vec4ShortArray(array, 0, 0, false)
    }
}

final class Vec4ShortBuffer private (
        val buffer: ShortBuffer,
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends Vec4ShortSequence with ShortBufferSequence with Vec4iBufferSequence
{
    protected val step = components + stride
    val length = (buffer.limit - offset)/step

    def apply(i: Int) = {
        val j = i*step + offset
        ConstVec4i(
            buffer.get(j + 0),
            buffer.get(j + 1),
            buffer.get(j + 2),
            buffer.get(j + 3)
        )
    }
    def update(i: Int, u: AnyVec4i) {
        val j = i*step + offset
        buffer.put(j + 0, short(u.x))
        buffer.put(j + 1, short(u.y))
        buffer.put(j + 2, short(u.z))
        buffer.put(j + 3, short(u.w))
    }

    private[buffer] def readRaw(i: Int) :Raw = buffer.get(i)
    private[buffer] def writeRaw(i: Int, x: Raw) { buffer.put(i, short(x)) }
}

object Vec4ShortBuffer {
    def apply(buf: ShortBuffer, normalized: Boolean) = {
        new Vec4ShortBuffer(buf, 0, 0, normalized)
    }
    def apply(buf: ShortBuffer,
              offset: Int, stride: Int, normalized: Boolean) =
    {
        new Vec4ShortBuffer(buf, offset, stride, normalized)
    }

    def apply(vecCount: Int, normalized: Boolean) = {
        new Vec4ShortBuffer(allocateShortBuffer(vecCount*4), 0, 0, normalized)
    }

    implicit def bufferToView(buf: ShortBuffer) = {
        new Vec4ShortBuffer(buf, 0, 0, false)
    }
}


// Signed int
sealed abstract class Vec4IntSequence
extends Vec4SignedSequence with IntSequence
{
    def dataType = DataType.Int

    type SeqType = Vec4IntSequence
    type ArraySeqType = Vec4IntArray
    type BufferSeqType = Vec4IntBuffer

    private[buffer] def makeArraySequence(array: Array[D], stride: Int, offset: Int)
    :ArraySeqType = Vec4IntArray(array, stride, offset, normalized)
    private[buffer] def makeBufferSequence(buffer: BufferType, stride: Int,offset:Int)
    :BufferSeqType = Vec4IntBuffer(buffer, stride, offset, normalized)
}

final class Vec4IntArray private (
        val array: Array[Int],
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends Vec4IntSequence with IntArraySequence with Vec4iArraySequence
{
    protected val step = components + stride
    val length = (array.length - offset)/step
    val buffer = IntBuffer.wrap(array)

    def apply(i: Int) = {
        val j = i*step + offset
        ConstVec4i(
            array(j + 0),
            array(j + 1),
            array(j + 2),
            array(j + 3)
        )
    }
    def update(i: Int, u: AnyVec4i) {
        val j = i*step + offset
        array(j + 0) = u.x
        array(j + 1) = u.y
        array(j + 2) = u.z
        array(j + 3) = u.w
    }

    private[buffer] def readRaw(i: Int) :Raw = array(i)
    private[buffer] def writeRaw(i: Int, x: Raw) { array(i) = x }
}

object Vec4IntArray {
    def apply(array: Array[Int], normalized: Boolean) = {
        new Vec4IntArray(array, 0, 0, normalized)
    }
    def apply(array: Array[Int],
              offset: Int, stride: Int, normalized: Boolean) =
    {
        new Vec4IntArray(array, offset, stride, normalized)
    }

    def apply(vecCount: Int, normalized: Boolean) = {
        new Vec4IntArray(new Array[Int](vecCount*4), 0, 0, normalized)
    }

    implicit def arrayToView(array: Array[Int]) = {
        new Vec4IntArray(array, 0, 0, false)
    }
}

final class Vec4IntBuffer private (
        val buffer: IntBuffer,
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends Vec4IntSequence with IntBufferSequence with Vec4iBufferSequence
{
    protected val step = components + stride
    val length = (buffer.limit - offset)/step

    def apply(i: Int) = {
        val j = i*step + offset
        ConstVec4i(
            buffer.get(j + 0),
            buffer.get(j + 1),
            buffer.get(j + 2),
            buffer.get(j + 3)
        )
    }
    def update(i: Int, u: AnyVec4i) {
        val j = i*step + offset
        buffer.put(j + 0, u.x)
        buffer.put(j + 1, u.y)
        buffer.put(j + 2, u.z)
        buffer.put(j + 3, u.w)
    }

    private[buffer] def readRaw(i: Int) :Raw = buffer.get(i)
    private[buffer] def writeRaw(i: Int, x: Raw) { buffer.put(i, x) }
}

object Vec4IntBuffer {
    def apply(buf: IntBuffer, normalized: Boolean) = {
        new Vec4IntBuffer(buf, 0, 0, normalized)
    }
    def apply(buf: IntBuffer, offset: Int, stride: Int, normalized: Boolean) = {
        new Vec4IntBuffer(buf, offset, stride, normalized)
    }

    def apply(vecCount: Int, normalized: Boolean) = {
        new Vec4IntBuffer(allocateIntBuffer(vecCount*4), 0, 0, normalized)
    }

    implicit def bufferToView(buf: IntBuffer) = {
        new Vec4IntBuffer(buf, 0, 0, false)
    }
}
