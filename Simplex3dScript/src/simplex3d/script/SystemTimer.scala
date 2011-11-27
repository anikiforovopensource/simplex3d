/*
 * Simplex3dScript
 * Copyright (C) 2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dScript.
 *
 * Simplex3dScript is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dScript is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.script


/**
 * @author Aleksey Nikiforov (lex)
 */
private[script] final class SystemTimer {
  private[this] var start: Long = _
  private[this] var last: Long = _

  private[this] val fpsSampleRateNanos = 500*1000*1000L
  private[this] var lastFpsSample: Long = _
  private[this] var frameCount: Int = _

  private[this] var up: Double = _
  private[this] var tpf: Double = 0
  private[this] var fps: Double = 0

  reset()

  def reset() {
    start = System.nanoTime
    last = start
    lastFpsSample = start
    frameCount = 0
    up = 0
  }

  def uptime = up
  def timePerFrame = tpf
  def averageFps = fps

  def update() {
    val cur = System.nanoTime
    tpf = (cur - last)/1e9
    up = (cur - start)/1e9
    last = cur

    frameCount += 1
    if (cur - lastFpsSample >= fpsSampleRateNanos) {
      fps = frameCount*1e9 / (cur - lastFpsSample)

      frameCount = 0
      lastFpsSample = cur
    }
  }
}
