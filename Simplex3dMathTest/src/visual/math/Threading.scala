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

import java.util.concurrent.{ConcurrentLinkedQueue => Queue}
import java.util.concurrent.ExecutorService
import scala.concurrent.ops.spawn


/**
 * @author Aleksey Nikiforov (lex)
 */
object ThreadingTest {
    def main(args: Array[String]) {
        val pool = java.util.concurrent.Executors.newCachedThreadPool()

        class TestJob extends Job(pool) {
            val f = Integer.MAX_VALUE - 3
            var t = 1
            def hasMoreChunks() :Boolean = t < f
            def nextChunk() :Chunk = {
                val s = t
                t += 100000
                if (t > f || t < 0) t = f
                val end = t
                new Chunk {
                    def run() {
                        for (i <- s until end) {
                            val r = f % i
                            if (r == 0) println(i)
                        }
                    }
                }
            }

            def runSingleThreaded() {
                for (i <- 1 until f) {
                    val r = f % i
                    if (r == 0) println(i)
                }
            }
        }

        val t = new TestJob
        t.execAndWait
        println("end method")
        pool.shutdown()
    }
}

abstract class Chunk {
    def run() :Unit
}
abstract class Job(private val threadPool: ExecutorService = null) {
    private var error: Exception = null
    private var stop = false
    private val queue = new Queue[Chunk]
    private var liveThreads = 1
    private var executing = false

    val unusedProcessors :Int = 0
    val preferredBatchSize :Int = 200

    private def batchSize = {
        if (preferredBatchSize <= 100) 100
        else if (preferredBatchSize >= 10000) 10000
        else preferredBatchSize
    }

    final def exec() {
        synchronized {
            if (executing) throw new IllegalStateException("Already executing.")
            executing = true
            stop = false
        }

        val bias = if (unusedProcessors > 0) unusedProcessors else 0
        liveThreads = Runtime.getRuntime.availableProcessors - bias
        
        if (liveThreads < 2) {
            liveThreads = 1
            val runner = new Runnable() { def run() {
                    try {
                        runSingleThreaded()
                    }
                    catch {
                        case e: Exception => handleError(e)
                    }
                    exitingThread()
                }
            }

            if (threadPool != null) threadPool.execute(runner)
            else spawn { runner.run() }
        }
        else {
            while(queue.poll != null) {}

            val batch = batchSize
            var i = 0; while (hasMoreChunks() && i < batch) {
                queue.offer(nextChunk())
                i += 1
            }

            val runners = for (i <- 0 until liveThreads) yield {
                new Runnable() { def run() {
                    try {
                        var chunk :Chunk = null

                        do {
                            chunk = queue.poll
                            if (chunk == null) chunk = moreJobs()
                            if (chunk != null) chunk.run()
                        }
                        while (chunk != null)
                    }
                    catch {
                        case e: Exception => handleError(e)
                    }
                    exitingThread()
                }}
            }

            for (runner <- runners) {
                if (threadPool != null) threadPool.execute(runner)
                else spawn { runner.run() }
            }
        }
    }

    private def moreJobs() :Chunk = {
        synchronized {
            if (stop) return null

            val batch = batchSize
            var i = 0; while (hasMoreChunks() && i < batch) {
                queue.offer(nextChunk())
                i += 1
            }

            queue.poll
        }
    }

    private def handleError(e: Exception) {
        synchronized {
            if (error == null) {
                error = e
                e.printStackTrace() //or an error handler
            }
            stop = true
        }
    }

    private def exitingThread() {
        synchronized {
            liveThreads -= 1
            if (liveThreads == 0) {
                executing = false
                notifyAll
            }
        }
    }

    final def isExecuting() = {
        synchronized {
            executing
        }
    }

    final def execAndWait() {
        exec()
        waitForCompletion()
        if (error != null) throw error
    }

    final def cancel() {
        synchronized {
            stop = true
        }
    }

    final def cancelAndWait() {
        cancel()
        waitForCompletion()
    }

    final def waitForCompletion() {
        synchronized {
            while (executing) wait()
        }
    }

    def runSingleThreaded() :Unit
    def hasMoreChunks() :Boolean
    def nextChunk() :Chunk
}
