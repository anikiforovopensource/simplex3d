/*
 * Simplex3d, MathTest package
 * Copyright (C) 2009-2010 Simplex3d Team
 *
 * This file is part of Simplex3dMathTest.
 *
 * Simplex3dMathTest is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dMathTest is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package visual.math

import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors._

import simplex3d.math.BaseMath._
import simplex3d.math.intm.IntMath
import simplex3d.math.floatm.renamed._
import simplex3d.math.floatm.FloatMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
object FunPainter {

    private final def rgb(c: AnyVec3) :Int = {
        (IntMath.clamp(int(c.r*255), 0, 255) << 16) |
        (IntMath.clamp(int(c.g*255), 0, 255) << 8) |
        IntMath.clamp(int(c.b*255), 0, 255) |
        0xFF000000
    }

    private class FunJob(fun: Fun, es: ExecutorService) extends Job(es) {
        var buffer: Array[Int] = null
        var width = 0
        var height = 0
        var time: Float = 0
        var dim: ConstVec2 = null

        private var yoffset = 0

        def setData(buffer: Array[Int],
                    width: Int, height: Int,
                    time: Float)
        {
            if (isExecuting) throw new IllegalStateException(
                "Cannot change while executing.")

            this.buffer = buffer
            this.width = width
            this.height = height
            yoffset = 0
            this.time = time
            dim = ConstVec2(width, height)
        }
        def runSingleThreaded() {
            val h1 = height - 1
            var y = 0; while(y < height) {
                val h = h1 - y
                var x = 0; while(x < width) {

                    buffer(x + y*width) = rgb(fun(ConstVec2(x, h), time, dim))

                    x += 1
                }
                y += 1
            }
        }
        final def hasMoreChunks() = yoffset < height
        final def nextChunk() = {
            val ystart = yoffset
            yoffset += 5
            if (yoffset > height) yoffset = height
            val yend = yoffset
            new Chunk() {
                final def run() {
                    val h1 = height - 1
                    var y = ystart; while (y < yend) {
                        loop(y, h1 - y)
                        y += 1
                    }
                }
                final def loop(y :Int, h: Int) {
                    var x = 0; while(x < width) {
                      buffer(x + y*width) = rgb(fun(ConstVec2(x, h), time, dim))

                      x += 1
                    }
                }
            }
        }
    }

    def apply(fun: Fun) :Painter = new Painter() {
        private val start = System.currentTimeMillis
        private val job = new FunJob(fun, newCachedThreadPool())

        private var curFrame = 0
        // the rendering algorithm only uses 2 frames
        private val frames = new Array[(Array[Int], Int, Int)](2)
        frames(0) = (new Array[Int](0), 0, 0)
        frames(1) = (new Array[Int](0), 0, 0)

        def paint(width: Int, height: Int) :Array[Int] = {
            def time = (System.currentTimeMillis - start)/1000f

            val retBuffer: Array[Int] =
                if (job.width != width || job.height != height) {
                    job.cancelAndWait()
                    val buffer = new Array[Int](width*height)
                    frames(curFrame) = (buffer, width, height)
                    job.setData(buffer, width, height, time)
                    job.execAndWait()
                    job.buffer
                }
                else {
                    job.waitForCompletion()
                    job.buffer
                }

            curFrame += 1
            if (curFrame >= frames.length) curFrame = 0
            var (buffer, w, h) = frames(curFrame)

            if (w != width || h != height) {
                buffer = new Array[Int](width*height)
                frames(curFrame) = (buffer, width, height)
            }

            job.setData(buffer, width, height, time)
            job.exec
            retBuffer
        }
    }

    def testLoad(fun: Fun, iterations: Int) {
        val timer = new FpsTimer()

        def time = (System.currentTimeMillis % 100000000)/1000f
        val width = 640
        val height = 480
        val buffer = new Array[Int](width*height)

        val job = new FunJob(fun, newCachedThreadPool())

        var i = 0; while (i < iterations) {
            timer.update()

            job.setData(buffer, width, height, time)
            job.execAndWait()

            if (i % 10 == 0) println("fps: " + timer.fps)
            i += 1
        }
    }
}
