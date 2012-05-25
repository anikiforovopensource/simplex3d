/*
 * Simplex3dEngine - LWJGL Module
 * Copyright (C) 2011-2012, Aleksey Nikiforov
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
package backend.lwjgl

import org.lwjgl.opengl._
import org.lwjgl.input.{Keyboard => RawKeyboard, Mouse => RawMouse }
import simplex3d.math._
import simplex3d.engine.input._
import simplex3d.engine.graphics._


final class SimpleLoop extends simplex3d.engine.MainLoop {
  val driver = "lwjgl"
  
  private val input = new Input
  private var lastFps = 0.0
  
  def init(app: App#Subtext) {}
  
  def body(app: App#Subtext) :Boolean = {
    import app._
    
    val renderManager = app.renderManager.asInstanceOf[RenderManager]
    
    renderManager.renderContext.resetState()
    timer.update()
    val time = timer.timeStamp
    
    Display.processMessages()
    handleInput(time, app)
    
    preUpdate(time)
    update(time)
    render(time)
    
    Display.update(false)
    Thread.`yield`()
    
    manage()
    renderManager.renderContext.manage()
    
    if (settings.logPerformance && lastFps != timer.fps) {
      lastFps = timer.fps
      println("fps: " + lastFps)
    }
    
    Display.isCloseRequested
  }
  
  def dispose() {}
  
  private def handleInput(time: TimeStamp, app: App#Subtext) {
    input.mouse.update() // Sync mouse cached state to system state.
    
    val listeners = app.inputListeners
    for (ls <- listeners if ls.isEnabled) { ls.update(input, time) }
    
    while (RawKeyboard.next()) {
      val event = new KeyEvent(
        input.keyboard.decode(RawKeyboard.getEventKey()),
        RawKeyboard.getEventCharacter(),
        time
      )
      
      if (RawKeyboard.getEventKeyState()) for (ls <- listeners if ls.isEnabled) {
        ls.keyboardListener.keyPressed(input, event)
      }
      else for (ls <- listeners if ls.isEnabled) {
        ls.keyboardListener.keyReleased(input, event); ls.keyboardListener.keyTyped(input, event)
      }
    }
    
    
    def position() = if (input.mouse.isGrabbed) None else Some(ConstVec2i(RawMouse.getEventX(), RawMouse.getEventY()))
    while (RawMouse.next()) {
      import simplex3d.math._
      import simplex3d.math.double._
      import simplex3d.math.double.functions._
      
      val delta = ConstVec2(RawMouse.getEventDX, RawMouse.getEventDY)
      
      if (RawMouse.getEventButton() >= 0) {
        val event = new MouseButtonEvent(RawMouse.getEventButton(), position, time)
        
        if (RawMouse.getEventButtonState()) for (ls <- listeners if ls.isEnabled) {
          ls.mouseListener.mousePressed(input, event)
        }
        else for (ls <- listeners if ls.isEnabled) {
          ls.mouseListener.mouseReleased(input, event); ls.mouseListener.mouseClicked(input, event)
        }
        
      }
      else if (any(notEqual(delta, Vec2.Zero))) {
        val event = new MouseMotionEvent(delta*input.mouse.sensitivity, position, time)
        
        for (ls <- listeners if ls.isEnabled) { ls.mouseListener.mouseMoved(input, event) }
      }
      else if (RawMouse.getEventDWheel() != 0) {
        val event = new MouseWheelEvent(RawMouse.getEventDWheel()*input.mouse.wheelSensitivity, position, time)
        
        for (ls <- listeners if ls.isEnabled) { ls.mouseListener.mouseWheelMoved(input, event) }
      }
      // TODO add mouse entered/exited using Mouse.isInsideWindow
    }
  }
}
