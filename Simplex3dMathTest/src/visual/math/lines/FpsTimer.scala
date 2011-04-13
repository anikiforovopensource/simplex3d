/*
 * Simplex3d, MathTest package
 * Copyright (C) 2010-2011, Aleksey Nikiforov
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

package visual.math.lines


/**
 * @author Aleksey Nikiforov (lex)
 */
class FpsTimer {
  private val start = System.currentTimeMillis
  private var last = System.currentTimeMillis

  private val fpsSampleRateMillis = 500
  private var timestamp = last
  private var count = 0

  private var _uptime: Float = 0
  private var _tpf: Float = 0
  private var _fps: Float = 0

  def uptime = _uptime
  def tpf = _tpf
  def fps = _fps

  def update() {
    val cur = System.currentTimeMillis
    _tpf = (cur - last)/1000f
    last = cur
    _uptime = (cur - start)/1000f

    count += 1
    if (cur - timestamp >= fpsSampleRateMillis) {
      _fps = count*1000f / (cur - timestamp)

      count = 0
      timestamp = cur
    }
  }
}
