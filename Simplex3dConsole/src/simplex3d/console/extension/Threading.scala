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

import java.util.concurrent.{ConcurrentLinkedQueue => Queue}
import java.security.AccessControlException
import java.util.concurrent.ExecutorService
import scala.concurrent.ops.spawn


/**
 * @author Aleksey Nikiforov (lex)
 */
private[extension] object ThreadingTest {

  def main(args: Array[String]) {
    testFactoring()
  }

  def testFactoring() {
    var pool = java.util.concurrent.Executors.newCachedThreadPool()

    // Not meant to be fast, just load the CPU and have some meaningful output.
    class TestJob extends Job(pool) {
      override val maxThreads = 4

      val factoring = Integer.MAX_VALUE - 3
      var current = 1

      def hasMoreChunks() :Boolean = current < factoring

      def nextChunk() :Chunk = {
        val start = current
        current += 100000
        if (current > factoring || current < 0) current = factoring
        val end = current

        new Chunk {
          def run() {
            var i = start; while (i < end) {
              val remainder = factoring % i
              if (remainder == 0) println(i)

              i += 1
            }
          }
        }

      }

      def runSingleThreaded() {
        var i = 1; while (i < factoring) {
          val remainder = factoring % i
          if (remainder == 0) println(i)
          i += 1
        }
      }
    }

    val test = new TestJob
    test.execAndWait()
    println("end method")

    if (pool != null) pool.shutdown()
  }

  def testCustomMultithreading() {
    var pool = java.util.concurrent.Executors.newCachedThreadPool()

    val factoring = Integer.MAX_VALUE - 3
    class TestIndependent(val factoring: Int, val start: Int, val end: Int)
    extends Runnable
    {
      def run() {
        var i = start; while(i < end) {
          val remainder = factoring % i
          if (remainder == 0) println(i)
          i += 1
        }
      }
    }

    val threads = 4
    val step = factoring/threads
    val runners = for (i <- 0 until threads) yield {
      new TestIndependent(factoring, i*step + 1, (i + 1)*step)
    }
    for (runner <- runners) {
      if (pool != null) pool.execute(runner)
      else spawn { runner.run() }
    }

    if (pool != null) pool.shutdown()
  }
}


private[extension] abstract class Chunk {
  def run() :Unit
}

private[extension] abstract class Job(private val threadPool: ExecutorService = null) {
  private var error: Exception = null
  private var stop = false
  private val queue = new Queue[Chunk]
  private var liveThreads = 1
  private var executing = false

  val maxThreads :Int = 0
  val unusedProcessors :Int = 0
  val preferredBatchSize :Int = 200

  private[this] final def batchSize = {
    if (preferredBatchSize <= 100) 100
    else if (preferredBatchSize >= 1000) 1000
    else preferredBatchSize
  }

  final def exec() {
    synchronized {
      if (executing) throw new IllegalStateException("Already executing.")
      executing = true
      stop = false
    }

    val unused = if (unusedProcessors > 0) unusedProcessors else 0
    liveThreads = Runtime.getRuntime.availableProcessors - unused
    if (maxThreads > 0 && maxThreads < liveThreads) liveThreads = maxThreads
    
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
              Thread.`yield`()
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

  private[this] final def moreJobs() :Chunk = {
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

  private[this] final def handleError(e: Exception) {
    synchronized {
      if (error == null) {
        error = e
        e.printStackTrace() //or an error handler
      }
      stop = true
    }
  }

  private[this] final def exitingThread() {
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

  def dispose() {
    cancelAndWait()
    
    if (threadPool != null) try {
      threadPool.shutdown()
    }
    catch {
      case e: AccessControlException => // do nothing
    }
  }
}
