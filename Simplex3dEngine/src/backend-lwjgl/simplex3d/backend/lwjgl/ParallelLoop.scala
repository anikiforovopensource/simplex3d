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

package simplex3d.backend.lwjgl

import org.lwjgl.opengl._
import org.lwjgl.input.{Keyboard => RawKeyboard, Mouse => RawMouse }
import simplex3d.math._
import simplex3d.engine._
import simplex3d.engine.input._
import simplex3d.engine.graphics._


final class ParallelLoop extends simplex3d.engine.MainLoop {
  val driver = "lwjgl"//XXX replace driver with Backend
  
  @volatile private[this] var disposed = false
  
  private val input = new Input
  private var lastFps = 0.0
  
  private[this] object Stage extends Enumeration { val Processing, WaitingForSync, Synchronizing, WaitingToProccess = Value }
  private[this] final class Worker {
    var stage: Stage.type#Value = Stage.Processing
    var terminated = false
  }
  private[this] val worker = new Worker
  
  def init(app: App#Subtext) {
    new Thread { override def run() {
      
      while (!disposed) {
        
        val stage = worker.synchronized {
          worker.notifyAll()
          worker.wait()
          worker.stage
        }
        
        stage match {
          
          case Stage.Processing =>
            app.timer.update()
            val time = app.timer.timeStamp
            
            handleInput(time, app)
            
            app.preUpdate(time)
            app.update(time)
            
            worker.synchronized {
              worker.stage = Stage.WaitingForSync
            }
            
          case Stage.Synchronizing =>
            // XXX copy properties
            
            worker.synchronized {
              worker.stage = Stage.WaitingToProccess
            }
            
          case _ =>
            // do nothing
        }
      }
      
      worker.synchronized {
        worker.terminated = true
        worker.notifyAll()
      }

    }}.start()
  }
  
  def body(app: App#Subtext) :Boolean = {
    import app._
    
    // Wait for sync.
    worker.synchronized {
      while (!worker.terminated && worker.stage != Stage.WaitingForSync) {
        worker.notifyAll()
        worker.wait()
      }
    }

    
    // Joined stage.
    
    val renderManager = app.renderManager.asInstanceOf[RenderManager]
    renderManager.renderContext.resetState()
    
    manage()
    
    // XXX init shader programs
    // XXX perform shader property binding
    
    
    // Trigger sync phase.
    worker.synchronized {
      worker.stage = Stage.Synchronizing
      worker.notifyAll()
    }
    
    
    // Sync stage.
    
    // XXX load texture and mesh data onto GPU
    
    
    // Wait for worker to finish sync, poll input, and move on to processing stage.
    worker.synchronized {
      while (!worker.terminated && worker.stage != Stage.WaitingToProccess) {
        worker.notifyAll()
        worker.wait()
      }
      
      Display.processMessages() // Poll input.
      
      worker.stage = Stage.Processing
      worker.notifyAll()
    }
    
    
    // Processing stage: worker performs update while synced copy is rendered.
    
    render(app.timer.timeStamp)
    Display.update(false)
    
    renderManager.renderContext.manage()
    
    if (settings.logPerformance && lastFps != timer.fps) {
      lastFps = timer.fps
      println("fps: " + lastFps)
    }
    
    Display.isCloseRequested()
  }
  
  
  def dispose() {
    disposed = true
    
    worker.synchronized {
      while (!worker.terminated) {
        worker.notifyAll()
        worker.wait()
      }
    }
  }
  
  
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
      
      // XXX invest into while loops
      if (RawKeyboard.getEventKeyState()) for (ls <- listeners if ls.isEnabled && !event.isConsumed) {
        ls.keyboardListener.keyPressed(input, event)
      }
      else for (ls <- listeners if ls.isEnabled && !event.isConsumed) {
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
        
        if (RawMouse.getEventButtonState()) for (ls <- listeners if ls.isEnabled && !event.isConsumed) {
          ls.mouseListener.mousePressed(input, event)
        }
        else for (ls <- listeners if ls.isEnabled && !event.isConsumed) {
          ls.mouseListener.mouseReleased(input, event); ls.mouseListener.mouseClicked(input, event)
        }
        
      }
      else if (any(notEqual(delta, Vec2.Zero))) {
        val event = new MouseMotionEvent(delta*input.mouse.sensitivity, position, time)
        
        for (ls <- listeners if ls.isEnabled && !event.isConsumed) { ls.mouseListener.mouseMoved(input, event) }
      }
      else if (RawMouse.getEventDWheel() != 0) {
        val event = new MouseWheelEvent(RawMouse.getEventDWheel()*input.mouse.wheelSensitivity, position, time)
        
        for (ls <- listeners if ls.isEnabled && !event.isConsumed) { ls.mouseListener.mouseWheelMoved(input, event) }
      }
      // TODO add mouse entered/exited using Mouse.isInsideWindow
    }
  }
}
