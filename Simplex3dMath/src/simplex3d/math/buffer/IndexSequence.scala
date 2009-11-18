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
sealed abstract class IndexSequence {
    val length: Int
    def apply(i: Int) :Int
    def update(i: Int, x: Int) :Unit

    val buffer :Buffer
    def dataType :Int
    def dataTypeBytes :Int
    def byteSize() = length*dataTypeBytes
}

final class IndexByteArray private (
        val array: Array[Byte]
) extends IndexSequence
{
    val length = array.length
    val buffer = ByteBuffer.wrap(array)
    def dataType = DataType.UByte
    def dataTypeBytes = 1

    def apply(i: Int) = int(array(i)) & 0xFF
    def update(i: Int, x: Int) { array(i) = byte(x) }
}

object IndexByteArray {
    def apply(array: Array[Byte]) = new IndexByteArray(array)

    def apply(indexCount: Int) = {
        new IndexByteArray(new Array[Byte](indexCount))
    }

    implicit def arrayToView(array: Array[Byte]) = {
        new IndexByteArray(array)
    }
}

final class IndexByteBuffer private (
        val buffer: ByteBuffer
) extends IndexSequence
{
    if (!buffer.isDirect) {
        throw new IllegalArgumentException(
                    "The buffer must be direct.")
    }

    val length = buffer.limit
    def dataType = DataType.UByte
    def dataTypeBytes = 1

    def apply(i: Int) = int(buffer.get(i)) & 0xFF
    def update(i: Int, x: Int) { buffer.put(i, byte(x)) }
}

object IndexByteBuffer {
    def apply(buf: ByteBuffer) = new IndexByteBuffer(buf)

    def apply(indexCount: Int) = {
        new IndexByteBuffer(allocateByteBuffer(indexCount))
    }

    implicit def bufferToView(buf: ByteBuffer) = {
        new IndexByteBuffer(buf)
    }
}

final class IndexShortArray private (
        val array: Array[Short]
) extends IndexSequence
{
    val length = array.length
    val buffer = ShortBuffer.wrap(array)
    def dataType = DataType.UShort
    def dataTypeBytes = 2

    def apply(i: Int) = int(array(i)) & 0xFFFF
    def update(i: Int, x: Int) { array(i) = short(x) }
}

object IndexShortArray {
    def apply(array: Array[Short]) = new IndexShortArray(array)

    def apply(indexCount: Int) = {
        new IndexShortArray(new Array[Short](indexCount))
    }

    implicit def arrayToView(array: Array[Short]) = {
        new IndexShortArray(array)
    }
}

final class IndexShortBuffer private (
        val buffer: ShortBuffer
) extends IndexSequence
{
    if (!buffer.isDirect) {
        throw new IllegalArgumentException(
                    "The buffer must be direct.")
    }

    val length = buffer.limit
    def dataType = DataType.UShort
    def dataTypeBytes = 2

    def apply(i: Int) = int(buffer.get(i)) & 0xFFFF
    def update(i: Int, x: Int) { buffer.put(i, short(x)) }
}

object IndexShortBuffer {
    def apply(buf: ShortBuffer) = new IndexShortBuffer(buf)

    def apply(indexCount: Int) = {
        new IndexShortBuffer(allocateShortBuffer(indexCount))
    }

    implicit def bufferToView(buf: ShortBuffer) = {
        new IndexShortBuffer(buf)
    }
}

final class IndexIntArray private (
        val array: Array[Int]
) extends IndexSequence
{
    val length = array.length
    val buffer = IntBuffer.wrap(array)
    def dataType = DataType.UInt
    def dataTypeBytes = 4

    def apply(i: Int) = array(i)
    def update(i: Int, x: Int) { array(i) = x }
}

object IndexIntArray {
    def apply(array: Array[Int]) = new IndexIntArray(array)

    def apply(indexCount: Int) = {
        new IndexIntArray(new Array[Int](indexCount))
    }

    implicit def arrayToView(array: Array[Int]) = {
        new IndexIntArray(array)
    }
}

final class IndexIntBuffer private (
        val buffer: IntBuffer
) extends IndexSequence
{
    if (!buffer.isDirect) {
        throw new IllegalArgumentException(
                    "The buffer must be direct.")
    }

    val length = buffer.limit
    def dataType = DataType.UInt
    def dataTypeBytes = 4

    def apply(i: Int) = buffer.get(i)
    def update(i: Int, x: Int) { buffer.put(i, x) }
}

object IndexIntBuffer {
    def apply(buf: IntBuffer) = new IndexIntBuffer(buf)

    def apply(indexCount: Int) = {
        new IndexIntBuffer(allocateIntBuffer(indexCount))
    }

    implicit def bufferToView(buf: IntBuffer) = {
        new IndexIntBuffer(buf)
    }
}
