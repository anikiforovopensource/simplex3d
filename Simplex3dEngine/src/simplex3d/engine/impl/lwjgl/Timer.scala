/*
 * Simplex3dEngine - LWJGL Module
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

package simplex3d
package engine
package impl.lwjgl

import org.lwjgl.Sys


private[lwjgl] final class Timer extends engine.Timer {
  private[this] val timeScale = 1.0/Sys.getTimerResolution()
  
  private[this] var start: Long = _
  private[this] var last: Long = _

  private[this] val fpsSampleRate = 2.5/timeScale
  private[this] var lastFpsSample: Long = _
  private[this] var frameCount: Int = _

  private[this] var approxFps: Double = 0
  private[this] var lastStamp = new TimeStamp(0, 0)

  reset()

  def reset() {
    start = Sys.getTime()
    last = start
    lastFpsSample = start
    frameCount = 0
  }
  
  def fps = approxFps

  def update() {
    val cur = Sys.getTime()
    val lastFrameInterval = (cur - last)*timeScale
    val uptime = (cur - start)*timeScale
    last = cur

    frameCount += 1
    if (cur - lastFpsSample >= fpsSampleRate) {
      approxFps = frameCount / ((cur - lastFpsSample)*timeScale)

      frameCount = 0
      lastFpsSample = cur
    }
    
    lastStamp = new TimeStamp(uptime, lastFrameInterval)
  }
  
  def timeStamp = lastStamp
  
  def frameTimer = new FrameTimer {
    def frameTime() :Double = (Sys.getTime() - last)*timeScale
  }
}
