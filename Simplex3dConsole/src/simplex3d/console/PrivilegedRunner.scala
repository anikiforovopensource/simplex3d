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

package simplex3d.console

import java.util.concurrent.{ConcurrentLinkedQueue => Queue}


/** This class is used to run tasks from trusted code.
 * The idea is to have the code from interpreter to only provides the data
 * while all the priviledged instructions are issued from the precompiled codebase.
 *
 * @author Aleksey Nikiforov (lex)
 */
private[console] object PrivilegedRunner {
  private[this] val taskQueue = new Queue[() => Unit]

  def queue(task: => Unit) { taskQueue.offer(() => task) }

  private[console] def runQueued() {
    var task = taskQueue.poll(); while (task != null) {
      task()
      task = taskQueue.poll()
    }
  }
}
