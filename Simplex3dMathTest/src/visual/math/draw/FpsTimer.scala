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

package visual.math.draw


/**
 * @author Aleksey Nikiforov (lex)
 */
class FpsTimer {
    private val start = System.currentTimeMillis
    private var last = System.currentTimeMillis
    private var fpsVal: Float = 0
    private val fpsCounter = new Loop(5)
    private var uptimeVal: Float = 0

    def fps = fpsVal
    def uptime = uptimeVal

    def update() {
        val cur = System.currentTimeMillis
        fpsCounter.put(1000f/(cur - last))
        last = cur
        uptimeVal = (cur - start)/1000f
        fpsVal = fpsCounter.average
    }

    private class Loop(val size: Int) {
        private var i = 0
        private val a = new Array[Float](size)

        def put(x: Float) {
            a(i) = x
            i += 1
            if (i >= size) i = 0
        }

        def average() = {
            val sum = (0f /: a) (_ + _)
            sum / size
        }
    }
}
