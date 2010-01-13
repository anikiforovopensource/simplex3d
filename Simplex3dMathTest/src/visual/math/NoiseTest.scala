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

import manual.math.NoiseFrame.Painter

import simplex3d.math.BaseMath._
import simplex3d.math.intm.IntMath
import simplex3d.math.floatm.renamed._
import simplex3d.math.floatm.FloatMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
object NoiseTest {

    def rgb(c: Vec3) :Int = {
        (IntMath.clamp(int(c.r*255), 0, 255) << 16) |
        (IntMath.clamp(int(c.g*255), 0, 255) << 8) |
        IntMath.clamp(int(c.b*255), 0, 255) |
        0xFF000000
    }

    def run(noise: (Int, Int, Float) => Vec3) {
        val pool = java.util.concurrent.Executors.newCachedThreadPool()

        val job = new Job(pool) {
            val freeProcessors = 0

            var buffer: Array[Int] = null
            var width = 0
            var height = 0
            var time: Float = 0
            
            private var yOffset = 0

            def setData(buffer: Array[Int],
                        width: Int, height: Int,
                        time: Float)
            {
                if (isExecuting) throw new IllegalStateException(
                    "Cannot change while executing.")

                this.buffer = buffer
                this.width = width
                this.height = height
                yOffset = 0
                this.time = time
            }
            def runSingleThreaded() {
                var y = 0; while(y < height) {
                    var x = 0; while(x < width) {

                        
                        buffer(x + y*width) = rgb(noise(x, y, time))

                        x += 1
                    }
                    y += 1
                }
            }
            def hasMoreChunks() = yOffset < height
            def nextChunk() = {
                val y = yOffset
                yOffset += 1
                new Chunk() {
                    def run() {
                        var x = 0; while(x < width) {
                            buffer(x + y*width) = rgb(noise(x, y, time))

                            x += 1
                        }
                    }
                }
            }
        }

//        val width = 640
//        val height = 480
//        val buffer = new Array[Int](width*height)
//        for (i <- 0 until 1000) {
//            job.setImageBuffer(buffer, width, height)
//            job.execAndWait()
//        }
//        pool.shutdown()

        NoiseFrame.run(new Painter() {
            private val frames = new Array[(Array[Int], Int, Int)](2)
            frames(0) = (new Array[Int](0), 0, 0)
            frames(1) = (new Array[Int](0), 0, 0)
            private var curFrame = 0

            def paint(width: Int, height: Int) :Array[Int] = {
                def time = (System.currentTimeMillis % 100000000)/1000f
                
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
        });
    }
}
