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

import java.io._
import java.awt._
import java.awt.event._
import javax.swing._
import scala.concurrent.ops._


/**
 * @author Aleksey Nikiforov (lex)
 */
class CachedPrintStream(
  private[this] val byteStream: ByteArrayOutputStream = new ByteArrayOutputStream
)
extends PrintStream(byteStream)
{
  def poll() :String = {
    val res = new String(byteStream.toByteArray, "UTF-8")
    byteStream.reset()
    res
  }
}

class StreamInterceptor(intercepted: PrintStream) {
  val cached = new CachedPrintStream
  
  def update() :String = {
    val msg = cached.poll()
    if (!msg.isEmpty) intercepted.print(msg)
    msg
  }
}

object StreamInterceptor {
  private var out: StreamInterceptor = _
  private var err: StreamInterceptor = _
  @volatile private[this] var externalLoop = false
  private val delay = 50
  
  def interceptSystemStreams() {
    out = new StreamInterceptor(System.out)
    System.setOut(out.cached)
    
    err = new StreamInterceptor(System.err)
    System.setErr(err.cached)
    
    spawn {
      while (!externalLoop) {
        update(null)
        Thread.sleep(delay)
      }
    }
  }
  
  private[this] def update(callback: String => Unit) {
    val msg = out.update() + err.update()
    if (callback != null && !msg.isEmpty) callback(msg)
  }
  
  def connectTo(textArea: JTextArea) {
    val callback: String => Unit = { msg =>
      val res = textArea.getText + msg
      textArea.setText(res.takeRight(math.min(res.length, 5000)))
      textArea.setCaretPosition(textArea.getDocument.getLength)
    }
    
    val task = new ActionListener() {
      def actionPerformed(evt: ActionEvent) { update(callback) }
    }
    
    val timer = new javax.swing.Timer(delay, task)
    timer.setInitialDelay(delay*2)
    
    externalLoop = true
    timer.start()
  }
}
