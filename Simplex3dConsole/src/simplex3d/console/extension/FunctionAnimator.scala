/*
 * Simplex3dConsole
 * Copyright (C) 2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dConsole.
 *
 * Simplex3dConsole is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dConsole is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.console.extension

import java.util.concurrent.ExecutorService

import java.util.concurrent.Executors
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.doublex.functions._


trait FunctionAnimator {
  def nextFrame(dims: inVec2i, time: Double) :Array[Int]
  def dispose() :Unit
}


/**
 * @author Aleksey Nikiforov (lex)
 */
private[extension] object FunctionAnimator {

  private class Painter (
    function: (inVec2, Double, inVec2) => ReadVec3,
    es: ExecutorService,
    exceptionHandler: Throwable => Unit
  ) extends Job(es, exceptionHandler) {

    var buffer: Array[Int] = null
    var dims = ConstVec2i(0)
    private var time: java.lang.Double = _

    private var yoffset = 0

    
    def setData(
      buffer: Array[Int],
      dimensions: inVec2i,
      time: Double
    ) {
      if (isExecuting) throw new IllegalStateException(
        "Cannot change while executing.")

      this.buffer = buffer
      this.dims = dimensions
      yoffset = 0
      this.time = time
    }

    def runSingleThreaded() {
      val h1 = dims.y - 1

      var y = 0; while(y < dims.y) {
        val h = h1 - y

        var x = 0; while(x < dims.x) {

          buffer(x + y*dims.x) = ImageUtils.rgb(function(dims, time.asInstanceOf[Double], ConstVec2(x, h)))

          x += 1
        }

        y += 1
      }
    }

    final def hasMoreChunks() = yoffset < dims.y

    final def nextChunk() = {
      val ystart = yoffset
      yoffset += 5
      if (yoffset > dims.y) yoffset = dims.y
      val yend = yoffset

      new Chunk() {
        final def run() {
          val h1 = dims.y - 1
          var y = ystart; while (y < yend) {
            loop(y, h1 - y)
            y += 1
          }
        }

        final def loop(y :Int, h: Int) {
          var x = 0; while(x < dims.x) {
            buffer(x + y*dims.x) = ImageUtils.rgb(function(dims, time.asInstanceOf[Double], ConstVec2(x, h)))
            x += 1
          }
        }
      }

    }
  }

  def apply(
    function: (inVec2, Double, inVec2) => ReadVec3,
    exceptionHandler: Throwable => Unit = _.printStackTrace()
  ) = new FunctionAnimator() {
    private val start = System.currentTimeMillis
    private val job = new Painter(function, Executors.newCachedThreadPool(), exceptionHandler)

    private var current = 0

    // the rendering algorithm only uses 2 frames
    private val frameBuffers = new Array[(Array[Int], ConstVec2i)](2)
    frameBuffers(0) = (new Array[Int](0), ConstVec2i(0))
    frameBuffers(1) = (new Array[Int](0), ConstVec2i(0))

    def nextFrame(dims: inVec2i, time: Double) :Array[Int] = {

      // Handle resize and/or wait for rendering to finish.
      val retBuffer: Array[Int] = {
        if (job.dims != dims) {
          job.cancelAndWait()
          val buffer = new Array[Int](dims.x*dims.y)
          frameBuffers(current) = (buffer, dims)
          job.setData(buffer, dims, time)
          job.execAndWait()
          job.buffer
        }
        else {
          job.waitForCompletion()
          job.buffer
        }
      }

      // Setup the next rendering (handle resize if necessary).
      current += 1
      if (current >= frameBuffers.length) current = 0
      var (buffer, buffDims) = frameBuffers(current)
      if (buffDims != dims) {
        buffer = new Array[Int](dims.x*dims.y)
        frameBuffers(current) = (buffer, dims)
      }

      // Start the next rendering in a parallel thread.
      job.setData(buffer, dims, time)
      job.exec

      // Return perviously competed rendering.
      retBuffer
    }

    def dispose() {
      job.dispose()
    }
  }

  def testLoad(function: (inVec2, Double, inVec2) => ReadVec3, iterations: Int) {
    val timer = new SystemTimer()

    val dims = ConstVec2i(640, 480)
    val buffer = new Array[Int](dims.x*dims.y)

    val job = new Painter(function, Executors.newCachedThreadPool(), _.printStackTrace())

    var i = 0; while (i < iterations) {
      timer.update()

      job.setData(buffer, dims, timer.uptime)
      job.execAndWait()

      if (i % 10 == 0) println("fps: " + timer.averageFps)
      i += 1
    }
  }
}
