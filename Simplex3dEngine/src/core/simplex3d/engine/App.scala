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

import java.security._
import java.util.logging._
import scala.collection.mutable.ArrayBuffer
import simplex3d.math._
import simplex3d.engine.util._
import simplex3d.engine.input._
import simplex3d.engine.graphics._


private[engine] object App {
  private final val logger = Logger.getLogger(classOf[App].getName)
}

trait App { self =>
  import App.logger._
  
  final class Subtext {
    def settings = self.settings
    
    def timer = self.timer
    def renderManager = self.renderManager
    
    def init() = self.init()
    def preUpdate(time: TimeStamp) = self.preUpdate(time)
    def update(time: TimeStamp) = self.update(time)
    def render(time: TimeStamp) = self.render(time)
    def manage() = self.manage()
    def reshape(position: inVec2i, dimensions: inVec2i) = self.reshape(position, dimensions)
    def inputListeners = self.inputListeners
  }
  private final val subtext = new Subtext
  

  val config: Config
  
  val title: String
  val settings: Settings
  
  private[this] var _launcher: Launcher = _
  private[this] var _mainLoop: MainLoop = _
  private[this] var _renderManager: RenderManager = _
  private[this] var _timer: Timer = _
  
  protected final def launcher = _launcher
  protected final def mainLoop = _mainLoop
  protected final def renderManager = _renderManager
  protected final def timer = _timer
  
  protected def init() :Unit
  protected def preUpdate(time: TimeStamp) :Unit
  protected def update(time: TimeStamp) :Unit
  protected def render(time: TimeStamp) :Unit
  protected def manage() :Unit
  protected def reshape(position: inVec2i, dimensions: inVec2i) :Unit
  
  
  /** Depending on the launcher, this method will return an appropriate UI element wrapping
   * the rendering surface. If the application is launched in a native window, this method
   * will return null.
   *  
   * @return a UI element wrapping the rendering surface, or null when launched in a native window. 
   */
  def launch() :Object = {
    AccessController.doPrivileged(new PrivilegedAction[Object]() {
      def run() = {
        try {
          privilegedLaunch()
        }
        catch {
          case e =>
            log(Level.SEVERE, e + "\n" + e.getStackTrace().mkString("    at ", "\n    at ", ""))
            throw e
        }
      }
    })
  }
  
  private def privilegedLaunch() :Object = {
    if (launcher != null && launcher.isRunning) throw new IllegalStateException("Already launched.")
    
    _launcher = Class.forName(config.launcher).newInstance().asInstanceOf[Launcher]
    _mainLoop = Class.forName(config.mainLoop).newInstance().asInstanceOf[MainLoop]
    _renderManager = Class.forName(config.renderManager).newInstance().asInstanceOf[RenderManager]
    _timer = Class.forName(config.timer).newInstance().asInstanceOf[Timer]
    
    if (launcher.driver != mainLoop.driver || launcher.driver != renderManager.driver) {
      throw new RuntimeException(
        "Runtime configuration contains incompatible classes:\n" +
        "  launcher =       '" + config.launcher + "'\n" +
        "  mainLoop =       '" + config.mainLoop + "'\n" +
        "  renderManager =  '" + config.renderManager + "'."
      )
    }
    
    launcher.launch(title, settings, subtext, mainLoop)
  }
  
  def dispose() {
    launcher.dispose()
  }
  
  def disposeAndWait() {
    launcher.disposeAndWait()
  }
  
  
  private val _inputListeners = new ArrayBuffer[InputListener]
  protected val inputListeners = new ReadSeq(_inputListeners)
  
  def addInputListener(listener: InputListener) {
    if (_inputListeners.contains(listener)) throw new IllegalStateException("Listener is already registered.")
    _inputListeners += listener
  }
  def removeInputListener(listener: InputListener) {
    _inputListeners -= listener
  }
}
