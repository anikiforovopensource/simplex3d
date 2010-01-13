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
            val freeProcessors = 0
            
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

//        println(t.isExecuting)
//        t.cancelAndWait
//        println(t.isExecuting)

//        t.singleThreaded

        println("end method")
        pool.shutdown()
    }
}

abstract class Chunk {
    def run() :Unit
}
abstract class Job(private val threadPool: ExecutorService = null) {
    private var error: Throwable = null
    private var stop = false
    private val queue = new Queue[Chunk]
    private val batchSize = 200
    private var liveThreads = 1
    private var executing = false

    def freeProcessors :Int

    private def execute() {
        synchronized {
            if (executing) throw new IllegalStateException("Already executing.")
            executing = true
            stop = false
        }

        liveThreads = Runtime.getRuntime.availableProcessors - freeProcessors
        
        if (liveThreads < 2) {
            liveThreads = 1
            val runner = new Runnable() { def run() {
                    try {
                        runSingleThreaded()
                    }
                    catch {
                        case t: Throwable => handleError(t)
                    }
                    exitingThread()
                }
            }

            if (threadPool != null) threadPool.execute(runner)
            else spawn { runner.run() }
        }
        else {
            while(queue.poll != null) {}

            var i = 0
            while (hasMoreChunks() && i < batchSize) {
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
                        case t: Throwable => handleError(t)
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

            var i = 0
            while (hasMoreChunks() && i < batchSize) {
                queue.offer(nextChunk())
                i += 1
            }

            queue.poll
        }
    }

    private def handleError(t: Throwable) {
        synchronized {
            if (error != null) {
                error = t
                t.printStackTrace() //or an error handler
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

    final def exec() {
        execute()
    }

    final def execAndWait() {
        execute()
        synchronized {
            while (executing) wait()
        }
        if (error != null) throw error
    }

    final def cancel() {
        synchronized {
            stop = true
        }
    }

    final def cancelAndWait() {
        synchronized {
            stop = true
            while(executing) wait()
        }
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
