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
sealed abstract class Vec3iSequence extends DataSequence {
    type E = AnyVec3i
    type Raw = Int

    override type SeqType <: Vec3iSequence

    def components = 3

    override private[buffer] def readRaw(i: Int) :Raw
    override private[buffer] def writeRaw(i: Int, x: Raw) :Unit
}

sealed trait Vec3iArraySequence extends Vec3iSequence with ArraySequence
sealed trait Vec3iBufferSequence extends Vec3iSequence with BufferSequence

sealed trait Vec3SignedSequence extends Vec3iSequence with SignedSequence
sealed trait Vec3UnsignedSequence extends Vec3iSequence with UnsignedSequence

sealed abstract class Vec3UByteSequence
extends Vec3UnsignedSequence with ByteSequence
{
    def dataType = DataType.UByte

    type SeqType = Vec3UByteSequence
    type ArraySeqType = Vec3UByteArray
    type BufferSeqType = Vec3UByteBuffer

    private[buffer] def makeArraySequence(array: Array[D], stride: Int, offset: Int)
    :ArraySeqType = Vec3UByteArray(array, stride, offset, normalized)
    private[buffer] def makeBufferSequence(buffer: BufferType, stride: Int,offset:Int)
    :BufferSeqType = Vec3UByteBuffer(buffer, stride, offset, normalized)
}

final class Vec3UByteArray private (
        val array: Array[Byte],
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends Vec3UByteSequence with ByteArraySequence with Vec3iArraySequence
{
    protected val step = components + stride
    val length = (array.length - offset)/step
    val buffer = ByteBuffer.wrap(array)

    def apply(i: Int) = {
        val j = i*step + offset
        ConstVec3i(
            int(array(j + 0)) & 0xFF,
            int(array(j + 1)) & 0xFF,
            int(array(j + 2)) & 0xFF
        )
    }
    def update(i: Int, u: AnyVec3i) {
        val j = i*step + offset
        array(j + 0) = byte(u.x)
        array(j + 1) = byte(u.y)
        array(j + 2) = byte(u.z)
    }

    private[buffer] def readRaw(i: Int) :Raw = int(array(i)) & 0xFF
    private[buffer] def writeRaw(i: Int, x: Raw) { array(i) = byte(x) }
}

object Vec3UByteArray {
    def apply(array: Array[Byte], normalized: Boolean) = {
        new Vec3UByteArray(array, 0, 0, normalized)
    }
    def apply(array: Array[Byte],
              offset: Int, stride: Int, normalized: Boolean) =
    {
        new Vec3UByteArray(array, offset, stride, normalized)
    }

    def apply(vecCount: Int, normalized: Boolean) = {
        new Vec3UByteArray(new Array[Byte](vecCount*3), 0, 0, normalized)
    }

    implicit def arrayToView(array: Array[Byte]) = {
        new Vec3UByteArray(array, 0, 0, false)
    }
}

final class Vec3UByteBuffer private (
        val buffer: ByteBuffer,
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends Vec3UByteSequence with ByteBufferSequence with Vec3iBufferSequence
{
    protected val step = components + stride
    val length = (buffer.limit - offset)/step

    def apply(i: Int) = {
        val j = i*step + offset
        ConstVec3i(
            int(buffer.get(j + 0)) & 0xFF,
            int(buffer.get(j + 1)) & 0xFF,
            int(buffer.get(j + 2)) & 0xFF
        )
    }
    def update(i: Int, u: AnyVec3i) {
        val j = i*step + offset
        buffer.put(j + 0, byte(u.x))
        buffer.put(j + 1, byte(u.y))
        buffer.put(j + 2, byte(u.z))
    }

    private[buffer] def readRaw(i: Int) :Raw = int(buffer.get(i)) & 0xFF
    private[buffer] def writeRaw(i: Int, x: Raw) { buffer.put(i, byte(x)) }
}

object Vec3UByteBuffer {
    def apply(buf: ByteBuffer, normalized: Boolean) = {
        new Vec3UByteBuffer(buf, 0, 0, normalized)
    }
    def apply(buf: ByteBuffer, offset: Int, stride: Int, normalized: Boolean) ={
        new Vec3UByteBuffer(buf, offset, stride, normalized)
    }

    def apply(vecCount: Int, normalized: Boolean) = {
        new Vec3UByteBuffer(allocateByteBuffer(vecCount*3), 0, 0, normalized)
    }

    implicit def bufferToView(buf: ByteBuffer) = {
        new Vec3UByteBuffer(buf, 0, 0, false)
    }
}


// Signed byte
sealed abstract class Vec3ByteSequence
extends Vec3SignedSequence with ByteSequence
{
    def dataType = DataType.Byte

    type SeqType = Vec3ByteSequence
    type ArraySeqType = Vec3ByteArray
    type BufferSeqType = Vec3ByteBuffer

    private[buffer] def makeArraySequence(array: Array[D], stride: Int, offset: Int)
    :ArraySeqType = Vec3ByteArray(array, stride, offset, normalized)
    private[buffer] def makeBufferSequence(buffer: BufferType, stride: Int,offset:Int)
    :BufferSeqType = Vec3ByteBuffer(buffer, stride, offset, normalized)
}

final class Vec3ByteArray private (
        val array: Array[Byte],
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends Vec3ByteSequence with ByteArraySequence with Vec3iArraySequence
{
    protected val step = components + stride
    val length = (array.length - offset)/step
    val buffer = ByteBuffer.wrap(array)

    def apply(i: Int) = {
        val j = i*step + offset
        ConstVec3i(
            array(j + 0),
            array(j + 1),
            array(j + 2)
        )
    }
    def update(i: Int, u: AnyVec3i) {
        val j = i*step + offset
        array(j + 0) = byte(u.x)
        array(j + 1) = byte(u.y)
        array(j + 2) = byte(u.z)
    }

    private[buffer] def readRaw(i: Int) :Raw = { array(i) }
    private[buffer] def writeRaw(i: Int, x: Raw) { array(i) = byte(x) }
}

object Vec3ByteArray {
    def apply(array: Array[Byte], normalized: Boolean) = {
        new Vec3ByteArray(array, 0, 0, normalized)
    }
    def apply(array: Array[Byte],
              offset: Int, stride: Int, normalized: Boolean) =
    {
        new Vec3ByteArray(array, offset, stride, normalized)
    }

    def apply(vecCount: Int, normalized: Boolean) = {
        new Vec3ByteArray(new Array[Byte](vecCount*3), 0, 0, normalized)
    }

    implicit def arrayToView(array: Array[Byte]) = {
        new Vec3ByteArray(array, 0, 0, false)
    }
}

final class Vec3ByteBuffer private (
        val buffer: ByteBuffer,
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends Vec3ByteSequence with ByteBufferSequence with Vec3iBufferSequence
{
    protected val step = components + stride
    val length = (buffer.limit - offset)/step

    def apply(i: Int) = {
        val j = i*step + offset
        ConstVec3i(
            buffer.get(j + 0),
            buffer.get(j + 1),
            buffer.get(j + 2)
        )
    }
    def update(i: Int, u: AnyVec3i) {
        val j = i*step + offset
        buffer.put(j + 0, byte(u.x))
        buffer.put(j + 1, byte(u.y))
        buffer.put(j + 2, byte(u.z))
    }

    private[buffer] def readRaw(i: Int) :Raw = buffer.get(i)
    private[buffer] def writeRaw(i: Int, x: Raw) { buffer.put(i, byte(x)) }
}

object Vec3ByteBuffer {
    def apply(buf: ByteBuffer, normalized: Boolean) = {
        new Vec3ByteBuffer(buf, 0, 0, normalized)
    }
    def apply(buf: ByteBuffer, offset: Int, stride: Int, normalized: Boolean) ={
        new Vec3ByteBuffer(buf, offset, stride, normalized)
    }

    def apply(vecCount: Int, normalized: Boolean) = {
        new Vec3ByteBuffer(allocateByteBuffer(vecCount*3), 0, 0, normalized)
    }

    implicit def bufferToView(buf: ByteBuffer) = {
        new Vec3ByteBuffer(buf, 0, 0, false)
    }
}


// Unsigned short
sealed abstract class Vec3UShortSequence
extends Vec3UnsignedSequence with ShortSequence
{
    def dataType = DataType.UShort

    type SeqType = Vec3UShortSequence
    type ArraySeqType = Vec3UShortArray
    type BufferSeqType = Vec3UShortBuffer

    private[buffer] def makeArraySequence(array: Array[D], stride: Int, offset: Int)
    :ArraySeqType = Vec3UShortArray(array, stride, offset, normalized)
    private[buffer] def makeBufferSequence(buffer: BufferType, stride: Int,offset:Int)
    :BufferSeqType = Vec3UShortBuffer(buffer, stride, offset, normalized)
}

final class Vec3UShortArray private (
        val array: Array[Short],
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends Vec3UShortSequence with ShortArraySequence with Vec3iArraySequence
{
    protected val step = components + stride
    val length = (array.length - offset)/step
    val buffer = ShortBuffer.wrap(array)

    def apply(i: Int) = {
        val j = i*step + offset
        ConstVec3i(
            int(array(j + 0)) & 0xFFFF,
            int(array(j + 1)) & 0xFFFF,
            int(array(j + 2)) & 0xFFFF
        )
    }
    def update(i: Int, u: AnyVec3i) {
        val j = i*step + offset
        array(j + 0) = short(u.x)
        array(j + 1) = short(u.y)
        array(j + 2) = short(u.z)
    }

    private[buffer] def readRaw(i: Int) :Raw = int(array(i)) & 0xFFFF
    private[buffer] def writeRaw(i: Int, x: Raw) { array(i) = short(x) }
}

object Vec3UShortArray {
    def apply(array: Array[Short], normalized: Boolean) = {
        new Vec3UShortArray(array, 0, 0, normalized)
    }
    def apply(array: Array[Short],
              offset: Int, stride: Int, normalized: Boolean) =
    {
        new Vec3UShortArray(array, offset, stride, normalized)
    }

    def apply(vecCount: Int, normalized: Boolean) = {
        new Vec3UShortArray(new Array[Short](vecCount*3), 0, 0, normalized)
    }

    implicit def arrayToView(array: Array[Short]) = {
        new Vec3UShortArray(array, 0, 0, false)
    }
}

final class Vec3UShortBuffer private (
        val buffer: ShortBuffer,
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends Vec3UShortSequence with ShortBufferSequence with Vec3iBufferSequence
{
    protected val step = components + stride
    val length = (buffer.limit - offset)/step

    def apply(i: Int) = {
        val j = i*step + offset
        ConstVec3i(
            int(buffer.get(j + 0)) & 0xFFFF,
            int(buffer.get(j + 1)) & 0xFFFF,
            int(buffer.get(j + 2)) & 0xFFFF
        )
    }
    def update(i: Int, u: AnyVec3i) {
        val j = i*step + offset
        buffer.put(j + 0, short(u.x))
        buffer.put(j + 1, short(u.y))
        buffer.put(j + 2, short(u.z))
    }

    private[buffer] def readRaw(i: Int) :Raw = int(buffer.get(i)) & 0xFFFF
    private[buffer] def writeRaw(i: Int, x: Raw) { buffer.put(i, short(x)) }
}

object Vec3UShortBuffer {
    def apply(buf: ShortBuffer, normalized: Boolean) = {
        new Vec3UShortBuffer(buf, 0, 0, normalized)
    }
    def apply(buf: ShortBuffer,
              offset: Int, stride: Int, normalized: Boolean) =
    {
        new Vec3UShortBuffer(buf, offset, stride, normalized)
    }

    def apply(vecCount: Int, normalized: Boolean) = {
        new Vec3UShortBuffer(allocateShortBuffer(vecCount*3), 0, 0, normalized)
    }

    implicit def bufferToView(buf: ShortBuffer) = {
        new Vec3UShortBuffer(buf, 0, 0, false)
    }
}


// Signed short
sealed abstract class Vec3ShortSequence
extends Vec3SignedSequence with ShortSequence
{
    def dataType = DataType.Short

    type SeqType = Vec3ShortSequence
    type ArraySeqType = Vec3ShortArray
    type BufferSeqType = Vec3ShortBuffer

    private[buffer] def makeArraySequence(array: Array[D], stride: Int, offset: Int)
    :ArraySeqType = Vec3ShortArray(array, stride, offset, normalized)
    private[buffer] def makeBufferSequence(buffer: BufferType, stride: Int,offset:Int)
    :BufferSeqType = Vec3ShortBuffer(buffer, stride, offset, normalized)
}

final class Vec3ShortArray private (
        val array: Array[Short],
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends Vec3ShortSequence with ShortArraySequence with Vec3iArraySequence
{
    protected val step = components + stride
    val length = (array.length - offset)/step
    val buffer = ShortBuffer.wrap(array)

    def apply(i: Int) = {
        val j = i*step + offset
        ConstVec3i(
            array(j + 0),
            array(j + 1),
            array(j + 2)
        )
    }
    def update(i: Int, u: AnyVec3i) {
        val j = i*step + offset
        array(j + 0) = short(u.x)
        array(j + 1) = short(u.y)
        array(j + 2) = short(u.z)
    }

    private[buffer] def readRaw(i: Int) :Raw = array(i)
    private[buffer] def writeRaw(i: Int, x: Raw) { array(i) = short(x) }
}

object Vec3ShortArray {
    def apply(array: Array[Short], normalized: Boolean) = {
        new Vec3ShortArray(array, 0, 0, normalized)
    }
    def apply(array: Array[Short],
              offset: Int, stride: Int, normalized: Boolean) =
    {
        new Vec3ShortArray(array, offset, stride, normalized)
    }

    def apply(vecCount: Int, normalized: Boolean) = {
        new Vec3ShortArray(new Array[Short](vecCount*3), 0, 0, normalized)
    }

    implicit def arrayToView(array: Array[Short]) = {
        new Vec3ShortArray(array, 0, 0, false)
    }
}

final class Vec3ShortBuffer private (
        val buffer: ShortBuffer,
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends Vec3ShortSequence with ShortBufferSequence with Vec3iBufferSequence
{
    protected val step = components + stride
    val length = (buffer.limit - offset)/step

    def apply(i: Int) = {
        val j = i*step + offset
        ConstVec3i(
            buffer.get(j + 0),
            buffer.get(j + 1),
            buffer.get(j + 2)
        )
    }
    def update(i: Int, u: AnyVec3i) {
        val j = i*step + offset
        buffer.put(j + 0, short(u.x))
        buffer.put(j + 1, short(u.y))
        buffer.put(j + 2, short(u.z))
    }

    private[buffer] def readRaw(i: Int) :Raw = buffer.get(i)
    private[buffer] def writeRaw(i: Int, x: Raw) { buffer.put(i, short(x)) }
}

object Vec3ShortBuffer {
    def apply(buf: ShortBuffer, normalized: Boolean) = {
        new Vec3ShortBuffer(buf, 0, 0, normalized)
    }
    def apply(buf: ShortBuffer,
              offset: Int, stride: Int, normalized: Boolean) =
    {
        new Vec3ShortBuffer(buf, offset, stride, normalized)
    }

    def apply(vecCount: Int, normalized: Boolean) = {
        new Vec3ShortBuffer(allocateShortBuffer(vecCount*3), 0, 0, normalized)
    }

    implicit def bufferToView(buf: ShortBuffer) = {
        new Vec3ShortBuffer(buf, 0, 0, false)
    }
}


// Signed int
sealed abstract class Vec3IntSequence
extends Vec3SignedSequence with IntSequence
{
    def dataType = DataType.Int

    type SeqType = Vec3IntSequence
    type ArraySeqType = Vec3IntArray
    type BufferSeqType = Vec3IntBuffer

    private[buffer] def makeArraySequence(array: Array[D], stride: Int, offset: Int)
    :ArraySeqType = Vec3IntArray(array, stride, offset, normalized)
    private[buffer] def makeBufferSequence(buffer: BufferType, stride: Int,offset:Int)
    :BufferSeqType = Vec3IntBuffer(buffer, stride, offset, normalized)
}

final class Vec3IntArray private (
        val array: Array[Int],
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends Vec3IntSequence with IntArraySequence with Vec3iArraySequence
{
    protected val step = components + stride
    val length = (array.length - offset)/step
    val buffer = IntBuffer.wrap(array)

    def apply(i: Int) = {
        val j = i*step + offset
        ConstVec3i(
            array(j + 0),
            array(j + 1),
            array(j + 2)
        )
    }
    def update(i: Int, u: AnyVec3i) {
        val j = i*step + offset
        array(j + 0) = u.x
        array(j + 1) = u.y
        array(j + 2) = u.z
    }

    private[buffer] def readRaw(i: Int) :Raw = array(i)
    private[buffer] def writeRaw(i: Int, x: Raw) { array(i) = x }
}

object Vec3IntArray {
    def apply(array: Array[Int], normalized: Boolean) = {
        new Vec3IntArray(array, 0, 0, normalized)
    }
    def apply(array: Array[Int],
              offset: Int, stride: Int, normalized: Boolean) =
    {
        new Vec3IntArray(array, offset, stride, normalized)
    }

    def apply(vecCount: Int, normalized: Boolean) = {
        new Vec3IntArray(new Array[Int](vecCount*3), 0, 0, normalized)
    }

    implicit def arrayToView(array: Array[Int]) = {
        new Vec3IntArray(array, 0, 0, false)
    }
}

final class Vec3IntBuffer private (
        val buffer: IntBuffer,
        val offset: Int,
        val stride: Int,
        val normalized: Boolean
) extends Vec3IntSequence with IntBufferSequence with Vec3iBufferSequence
{
    protected val step = components + stride
    val length = (buffer.limit - offset)/step

    def apply(i: Int) = {
        val j = i*step + offset
        ConstVec3i(
            buffer.get(j + 0),
            buffer.get(j + 1),
            buffer.get(j + 2)
        )
    }
    def update(i: Int, u: AnyVec3i) {
        val j = i*step + offset
        buffer.put(j + 0, u.x)
        buffer.put(j + 1, u.y)
        buffer.put(j + 2, u.z)
    }

    private[buffer] def readRaw(i: Int) :Raw = buffer.get(i)
    private[buffer] def writeRaw(i: Int, x: Raw) { buffer.put(i, x) }
}

object Vec3IntBuffer {
    def apply(buf: IntBuffer, normalized: Boolean) = {
        new Vec3IntBuffer(buf, 0, 0, normalized)
    }
    def apply(buf: IntBuffer, offset: Int, stride: Int, normalized: Boolean) = {
        new Vec3IntBuffer(buf, offset, stride, normalized)
    }

    def apply(vecCount: Int, normalized: Boolean) = {
        new Vec3IntBuffer(allocateIntBuffer(vecCount*3), 0, 0, normalized)
    }

    implicit def bufferToView(buf: IntBuffer) = {
        new Vec3IntBuffer(buf, 0, 0, false)
    }
}
