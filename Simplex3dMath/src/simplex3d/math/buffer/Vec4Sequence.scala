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
sealed abstract class Vec4Sequence extends DataSequence {
    type E = AnyVec4
    type Raw = Float

    override type SeqType <: Vec4Sequence

    def components = 4

    override private[buffer] def readRaw(i: Int) :Raw
    override private[buffer] def writeRaw(i: Int, x: Raw) :Unit
}

sealed trait Vec4ArraySequence extends Vec4Sequence with ArraySequence
sealed trait Vec4BufferSequence extends Vec4Sequence with BufferSequence

sealed abstract class Vec4FloatSequence extends Vec4Sequence with FloatSequence{
    type SeqType = Vec4FloatSequence
    type ArraySeqType = Vec4FloatArray
    type BufferSeqType = Vec4FloatBuffer

    private[buffer] def makeArraySequence(array: Array[D], stride: Int, offset: Int)
    :ArraySeqType = Vec4FloatArray(array, stride, offset)
    private[buffer] def makeBufferSequence(buffer: BufferType, stride: Int,offset:Int)
    :BufferSeqType = Vec4FloatBuffer(buffer, stride, offset)
}

final class Vec4FloatArray private (
        val array: Array[Float],
        val offset: Int,
        val stride: Int
) extends Vec4FloatSequence with FloatArraySequence with Vec4ArraySequence
{
    protected val step = components + stride
    val length = (array.length - offset)/step
    val buffer = FloatBuffer.wrap(array)

    def apply(i: Int) = {
        val j = i*step + offset
        ConstVec4(
            array(j + 0),
            array(j + 1),
            array(j + 2),
            array(j + 3)
        )
    }
    def update(i: Int, u: AnyVec4) {
        val j = i*step + offset
        array(j + 0) = u.x
        array(j + 1) = u.y
        array(j + 2) = u.z
        array(j + 3) = u.z
    }
}

object Vec4FloatArray {
    def apply(array: Array[Float]) = new Vec4FloatArray(array, 0, 0)
    def apply(array: Array[Float], offset: Int, stride: Int) = {
        new Vec4FloatArray(array, offset, stride)
    }
    def apply(vecCount: Int) = {
        new Vec4FloatArray(new Array[Float](vecCount*4), 0, 0)
    }

    implicit def arrayToView(array: Array[Float]) = {
        new Vec4FloatArray(array, 0, 0)
    }
}

final class Vec4FloatBuffer private (
        val buffer: FloatBuffer,
        val offset: Int,
        val stride: Int
) extends Vec4FloatSequence with FloatBufferSequence with Vec4BufferSequence
{
    protected val step = components + stride
    val length = (buffer.limit - offset)/step

    def apply(i: Int) = {
        val j = i*step + offset
        ConstVec4(
            buffer.get(j + 0),
            buffer.get(j + 1),
            buffer.get(j + 2),
            buffer.get(j + 3)
        )
    }
    def update(i: Int, u: AnyVec4) {
        val j = i*step + offset
        buffer.put(j + 0, u.x)
        buffer.put(j + 1, u.y)
        buffer.put(j + 2, u.z)
        buffer.put(j + 3, u.w)
    }
}

object Vec4FloatBuffer {
    def apply(buf: FloatBuffer) = new Vec4FloatBuffer(buf, 0, 0)
    def apply(buf: FloatBuffer, offset: Int, stride: Int) = {
        new Vec4FloatBuffer(buf, offset, stride)
    }

    def apply(vecCount: Int) = {
        new Vec4FloatBuffer(allocateFloatBuffer(vecCount*4), 0, 0)
    }

    implicit def bufferToView(buf: FloatBuffer) = {
        new Vec4FloatBuffer(buf, 0, 0)
    }
}
