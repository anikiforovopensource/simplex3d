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

import java.nio._


/**
 * @author Aleksey Nikiforov (lex)
 */
object BufferCopy {

    // Float *****
    def copy(components: Int, size: Int,
            src: Array[Float], soffset: Int, sstride: Int,
            dest: Array[Float], doffset: Int, dstride: Int)
    {
        if (sstride == 0 && dstride == 0) {
            System.arraycopy(src, soffset, dest, doffset, size*components);
            return;
        }

        val sstep = sstride + components
        val dstep = dstride + components
        val end = size*sstep + soffset

        if (end - sstep + components > src.length) {
            throw new IndexOutOfBoundsException("src array is too short")
        }
        if ((size - 1)*dstep + doffset + components > dest.length) {
            throw new IndexOutOfBoundsException("dest array is too short")
        }

        var s = soffset
        var d = doffset

        components match {
            case 4 =>
                while (s < end) {
                    dest(d + 0) = src(s + 0)
                    dest(d + 1) = src(s + 1)
                    dest(d + 2) = src(s + 2)
                    dest(d + 3) = src(s + 3)
                    s += sstep; d += dstep
                }
            case 3 =>
                while (s < end) {
                    dest(d + 0) = src(s + 0)
                    dest(d + 1) = src(s + 1)
                    dest(d + 2) = src(s + 2)
                    s += sstep; d += dstep
                }
            case 2 =>
                while (s < end) {
                    dest(d + 0) = src(s + 0)
                    dest(d + 1) = src(s + 1)
                    s += sstep; d += dstep
                }
            case 1 =>
                while (s < end) {
                    dest(d + 0) = src(s + 0)
                    s += sstep; d += dstep
                }
        }
    }

    def copy(components: Int, size: Int,
            src: FloatBuffer, soffset: Int, sstride: Int,
            dest: FloatBuffer, doffset: Int, dstride: Int)
    {
        if (sstride == 0 && dstride == 0) {
            val slim = src.limit
            src.limit(size*components + soffset)

            src.position(soffset)
            dest.position(doffset)
            dest.put(src)

            src.limit(slim)
            src.position(0)
            dest.position(0)
            return;
        }

        val sstep = sstride + components
        val dstep = dstride + components
        val end = size*sstep + soffset

        if (end - sstep + components > src.limit) {
            throw new IndexOutOfBoundsException("src array is too short")
        }
        if ((size - 1)*dstep + doffset + components > dest.limit) {
            throw new IndexOutOfBoundsException("dest array is too short")
        }

        var s = soffset
        var d = doffset

        components match {
            case 4 =>
                while (s < end) {
                    dest.put((d + 0), src.get(s + 0))
                    dest.put((d + 1), src.get(s + 1))
                    dest.put((d + 2), src.get(s + 2))
                    dest.put((d + 3), src.get(s + 3))
                    s += sstep; d += dstep
                }
            case 3 =>
                while (s < end) {
                    dest.put((d + 0), src.get(s + 0))
                    dest.put((d + 1), src.get(s + 1))
                    dest.put((d + 2), src.get(s + 2))
                    s += sstep; d += dstep
                }
            case 2 =>
                while (s < end) {
                    dest.put((d + 0), src.get(s + 0))
                    dest.put((d + 1), src.get(s + 1))
                    s += sstep; d += dstep
                }
            case 1 =>
                while (s < end) {
                    dest.put((d + 0), src.get(s + 0))
                    s += sstep; d += dstep
                }
        }
    }

    def copy(components: Int, size: Int,
            src: FloatBuffer, soffset: Int, sstride: Int,
            dest: Array[Float], doffset: Int, dstride: Int)
    {
        copy(components, size, src, soffset, sstride, FloatBuffer.wrap(dest), doffset, dstride)
    }

    def copy(components: Int, size: Int,
            src: Array[Float], soffset: Int, sstride: Int,
            dest: FloatBuffer, doffset: Int, dstride: Int)
    {
        copy(components, size, FloatBuffer.wrap(src), soffset, sstride, dest, doffset, dstride)
    }

    // Byte *****
    def copy(components: Int, size: Int,
            src: Array[Byte], soffset: Int, sstride: Int,
            dest: Array[Byte], doffset: Int, dstride: Int)
    {
        if (sstride == 0 && dstride == 0) {
            System.arraycopy(src, soffset, dest, doffset, size*components);
            return;
        }

        val sstep = sstride + components
        val dstep = dstride + components
        val end = size*sstep + soffset

        if (end - sstep + components > src.length) {
            throw new IndexOutOfBoundsException("src array is too short")
        }
        if ((size - 1)*dstep + doffset + components > dest.length) {
            throw new IndexOutOfBoundsException("dest array is too short")
        }

        var s = soffset
        var d = doffset

        components match {
            case 4 =>
                while (s < end) {
                    dest(d + 0) = src(s + 0)
                    dest(d + 1) = src(s + 1)
                    dest(d + 2) = src(s + 2)
                    dest(d + 3) = src(s + 3)
                    s += sstep; d += dstep
                }
            case 3 =>
                while (s < end) {
                    dest(d + 0) = src(s + 0)
                    dest(d + 1) = src(s + 1)
                    dest(d + 2) = src(s + 2)
                    s += sstep; d += dstep
                }
            case 2 =>
                while (s < end) {
                    dest(d + 0) = src(s + 0)
                    dest(d + 1) = src(s + 1)
                    s += sstep; d += dstep
                }
            case 1 =>
                while (s < end) {
                    dest(d + 0) = src(s + 0)
                    s += sstep; d += dstep
                }
        }
    }

    def copy(components: Int, size: Int,
            src: ByteBuffer, soffset: Int, sstride: Int,
            dest: ByteBuffer, doffset: Int, dstride: Int)
    {
        if (sstride == 0 && dstride == 0) {
            val slim = src.limit
            src.limit(size*components + soffset)

            src.position(soffset)
            dest.position(doffset)
            dest.put(src)

            src.limit(slim)
            src.position(0)
            dest.position(0)
            return;
        }

        val sstep = sstride + components
        val dstep = dstride + components
        val end = size*sstep + soffset

        if (end - sstep + components > src.limit) {
            throw new IndexOutOfBoundsException("src array is too short")
        }
        if ((size - 1)*dstep + doffset + components > dest.limit) {
            throw new IndexOutOfBoundsException("dest array is too short")
        }

        var s = soffset
        var d = doffset

        components match {
            case 4 =>
                while (s < end) {
                    dest.put((d + 0), src.get(s + 0))
                    dest.put((d + 1), src.get(s + 1))
                    dest.put((d + 2), src.get(s + 2))
                    dest.put((d + 3), src.get(s + 3))
                    s += sstep; d += dstep
                }
            case 3 =>
                while (s < end) {
                    dest.put((d + 0), src.get(s + 0))
                    dest.put((d + 1), src.get(s + 1))
                    dest.put((d + 2), src.get(s + 2))
                    s += sstep; d += dstep
                }
            case 2 =>
                while (s < end) {
                    dest.put((d + 0), src.get(s + 0))
                    dest.put((d + 1), src.get(s + 1))
                    s += sstep; d += dstep
                }
            case 1 =>
                while (s < end) {
                    dest.put((d + 0), src.get(s + 0))
                    s += sstep; d += dstep
                }
        }
    }

    def copy(components: Int, size: Int,
            src: ByteBuffer, soffset: Int, sstride: Int,
            dest: Array[Byte], doffset: Int, dstride: Int)
    {
        copy(components, size, src, soffset, sstride, ByteBuffer.wrap(dest), doffset, dstride)
    }

    def copy(components: Int, size: Int,
            src: Array[Byte], soffset: Int, sstride: Int,
            dest: ByteBuffer, doffset: Int, dstride: Int)
    {
        copy(components, size, ByteBuffer.wrap(src), soffset, sstride, dest, doffset, dstride)
    }

    // Short *****
    def copy(components: Int, size: Int,
            src: Array[Short], soffset: Int, sstride: Int,
            dest: Array[Short], doffset: Int, dstride: Int)
    {
        if (sstride == 0 && dstride == 0) {
            System.arraycopy(src, soffset, dest, doffset, size*components);
            return;
        }

        val sstep = sstride + components
        val dstep = dstride + components
        val end = size*sstep + soffset

        if (end - sstep + components > src.length) {
            throw new IndexOutOfBoundsException("src array is too short")
        }
        if ((size - 1)*dstep + doffset + components > dest.length) {
            throw new IndexOutOfBoundsException("dest array is too short")
        }

        var s = soffset
        var d = doffset

        components match {
            case 4 =>
                while (s < end) {
                    dest(d + 0) = src(s + 0)
                    dest(d + 1) = src(s + 1)
                    dest(d + 2) = src(s + 2)
                    dest(d + 3) = src(s + 3)
                    s += sstep; d += dstep
                }
            case 3 =>
                while (s < end) {
                    dest(d + 0) = src(s + 0)
                    dest(d + 1) = src(s + 1)
                    dest(d + 2) = src(s + 2)
                    s += sstep; d += dstep
                }
            case 2 =>
                while (s < end) {
                    dest(d + 0) = src(s + 0)
                    dest(d + 1) = src(s + 1)
                    s += sstep; d += dstep
                }
            case 1 =>
                while (s < end) {
                    dest(d + 0) = src(s + 0)
                    s += sstep; d += dstep
                }
        }
    }

    def copy(components: Int, size: Int,
            src: ShortBuffer, soffset: Int, sstride: Int,
            dest: ShortBuffer, doffset: Int, dstride: Int)
    {
        if (sstride == 0 && dstride == 0) {
            val slim = src.limit
            src.limit(size*components + soffset)

            src.position(soffset)
            dest.position(doffset)
            dest.put(src)

            src.limit(slim)
            src.position(0)
            dest.position(0)
            return;
        }

        val sstep = sstride + components
        val dstep = dstride + components
        val end = size*sstep + soffset

        if (end - sstep + components > src.limit) {
            throw new IndexOutOfBoundsException("src array is too short")
        }
        if ((size - 1)*dstep + doffset + components > dest.limit) {
            throw new IndexOutOfBoundsException("dest array is too short")
        }

        var s = soffset
        var d = doffset

        components match {
            case 4 =>
                while (s < end) {
                    dest.put((d + 0), src.get(s + 0))
                    dest.put((d + 1), src.get(s + 1))
                    dest.put((d + 2), src.get(s + 2))
                    dest.put((d + 3), src.get(s + 3))
                    s += sstep; d += dstep
                }
            case 3 =>
                while (s < end) {
                    dest.put((d + 0), src.get(s + 0))
                    dest.put((d + 1), src.get(s + 1))
                    dest.put((d + 2), src.get(s + 2))
                    s += sstep; d += dstep
                }
            case 2 =>
                while (s < end) {
                    dest.put((d + 0), src.get(s + 0))
                    dest.put((d + 1), src.get(s + 1))
                    s += sstep; d += dstep
                }
            case 1 =>
                while (s < end) {
                    dest.put((d + 0), src.get(s + 0))
                    s += sstep; d += dstep
                }
        }
    }

    def copy(components: Int, size: Int,
            src: ShortBuffer, soffset: Int, sstride: Int,
            dest: Array[Short], doffset: Int, dstride: Int)
    {
        copy(components, size, src, soffset, sstride, ShortBuffer.wrap(dest), doffset, dstride)
    }

    def copy(components: Int, size: Int,
            src: Array[Short], soffset: Int, sstride: Int,
            dest: ShortBuffer, doffset: Int, dstride: Int)
    {
        copy(components, size, ShortBuffer.wrap(src), soffset, sstride, dest, doffset, dstride)
    }

    // Int *****
    def copy(components: Int, size: Int,
            src: Array[Int], soffset: Int, sstride: Int,
            dest: Array[Int], doffset: Int, dstride: Int)
    {
        if (sstride == 0 && dstride == 0) {
            System.arraycopy(src, soffset, dest, doffset, size*components);
            return;
        }

        val sstep = sstride + components
        val dstep = dstride + components
        val end = size*sstep + soffset

        if (end - sstep + components > src.length) {
            throw new IndexOutOfBoundsException("src array is too short")
        }
        if ((size - 1)*dstep + doffset + components > dest.length) {
            throw new IndexOutOfBoundsException("dest array is too short")
        }

        var s = soffset
        var d = doffset

        components match {
            case 4 =>
                while (s < end) {
                    dest(d + 0) = src(s + 0)
                    dest(d + 1) = src(s + 1)
                    dest(d + 2) = src(s + 2)
                    dest(d + 3) = src(s + 3)
                    s += sstep; d += dstep
                }
            case 3 =>
                while (s < end) {
                    dest(d + 0) = src(s + 0)
                    dest(d + 1) = src(s + 1)
                    dest(d + 2) = src(s + 2)
                    s += sstep; d += dstep
                }
            case 2 =>
                while (s < end) {
                    dest(d + 0) = src(s + 0)
                    dest(d + 1) = src(s + 1)
                    s += sstep; d += dstep
                }
            case 1 =>
                while (s < end) {
                    dest(d + 0) = src(s + 0)
                    s += sstep; d += dstep
                }
        }
    }

    def copy(components: Int, size: Int,
            src: IntBuffer, soffset: Int, sstride: Int,
            dest: IntBuffer, doffset: Int, dstride: Int)
    {
        if (sstride == 0 && dstride == 0) {
            val slim = src.limit
            src.limit(size*components + soffset)

            src.position(soffset)
            dest.position(doffset)
            dest.put(src)

            src.limit(slim)
            src.position(0)
            dest.position(0)
            return;
        }

        val sstep = sstride + components
        val dstep = dstride + components
        val end = size*sstep + soffset

        if (end - sstep + components > src.limit) {
            throw new IndexOutOfBoundsException("src array is too short")
        }
        if ((size - 1)*dstep + doffset + components > dest.limit) {
            throw new IndexOutOfBoundsException("dest array is too short")
        }

        var s = soffset
        var d = doffset

        components match {
            case 4 =>
                while (s < end) {
                    dest.put((d + 0), src.get(s + 0))
                    dest.put((d + 1), src.get(s + 1))
                    dest.put((d + 2), src.get(s + 2))
                    dest.put((d + 3), src.get(s + 3))
                    s += sstep; d += dstep
                }
            case 3 =>
                while (s < end) {
                    dest.put((d + 0), src.get(s + 0))
                    dest.put((d + 1), src.get(s + 1))
                    dest.put((d + 2), src.get(s + 2))
                    s += sstep; d += dstep
                }
            case 2 =>
                while (s < end) {
                    dest.put((d + 0), src.get(s + 0))
                    dest.put((d + 1), src.get(s + 1))
                    s += sstep; d += dstep
                }
            case 1 =>
                while (s < end) {
                    dest.put((d + 0), src.get(s + 0))
                    s += sstep; d += dstep
                }
        }
    }

    def copy(components: Int, size: Int,
            src: IntBuffer, soffset: Int, sstride: Int,
            dest: Array[Int], doffset: Int, dstride: Int)
    {
        copy(components, size, src, soffset, sstride, IntBuffer.wrap(dest), doffset, dstride)
    }

    def copy(components: Int, size: Int,
            src: Array[Int], soffset: Int, sstride: Int,
            dest: IntBuffer, doffset: Int, dstride: Int)
    {
        copy(components, size, IntBuffer.wrap(src), soffset, sstride, dest, doffset, dstride)
    }
}
