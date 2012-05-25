/*
 * Simplex3dEngine - Core Module
 * Copyright (C) 2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dEngine.
 *
 * Simplex3dEngine is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dEngine is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.engine
package util


final class SystemTimer extends Timer {
  private[this] var startNanos: Long = _
  private[this] var lastNanos: Long = _

  private[this] val fpsSampleRateNanos = 500*1000*1000L
  private[this] var lastFpsSample: Long = _
  private[this] var frameCount: Int = _

  private[this] var approxFps: Double = 0
  private[this] var lastStamp = new TimeStamp(0, 0)

  reset()

  def reset() {
    startNanos = System.nanoTime
    lastNanos = startNanos
    lastFpsSample = startNanos
    frameCount = 0
  }
  
  def fps = approxFps

  def update() {
    var cur = System.nanoTime
    if (cur < lastNanos || cur > lastNanos + 1000*1000*1000) cur = lastNanos + 10*1000*1000
    val lastFrameInterval = (cur - lastNanos)*1e-9
    val uptime = (cur - startNanos)*1e-9
    lastNanos = cur

    frameCount += 1
    if (cur - lastFpsSample >= fpsSampleRateNanos) {
      approxFps = frameCount*1e9 / (cur - lastFpsSample)

      frameCount = 0
      lastFpsSample = cur
    }
    
    lastStamp = new TimeStamp(uptime, lastFrameInterval)
  }
  
  def timeStamp = lastStamp
  
  val frameTimer = new FrameTimer {
    def frameTime() :Double = (System.nanoTime - lastNanos)*1e-9
  }
}
