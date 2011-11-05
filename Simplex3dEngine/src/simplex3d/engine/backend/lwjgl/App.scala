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

package simplex3d.engine
package backend.lwjgl

import java.util.logging._
import scala.annotation._
import org.lwjgl.opengl._
import org.lwjgl.input.{Keyboard => RawKeyboard, Mouse => RawMouse }
import simplex3d.math._
import simplex3d.engine.input._
import simplex3d.engine.graphics._


private[lwjgl] object App {
  private final val logger = Logger.getLogger(classOf[App].getName)
}


trait App extends simplex3d.engine.App {
  import App.logger._
  

  val profiler1 = new Profiler //XXX
  val profiler2 = new Profiler //XXX
  
  
  private val timer = new Timer
  protected val frameTimer = timer.frameTimer
  
  private var _renderManager: RenderManager = _
  protected def renderManager = _renderManager
  
  private val input = new Input
  private var lastFps = 0.0

  
  @volatile private var quit = false
  
  final def launch() {
    val desktopMode = Display.getDesktopDisplayMode()
    
    val resolution = settings.resolution.getOrElse(ConstVec2i(desktopMode.getWidth, desktopMode.getHeight))
    val fullscreen = if (settings.resolution.isDefined) settings.fullscreen else true
    
    val detectedMode = Display.getAvailableDisplayModes().filter( mode =>
      mode.getWidth == resolution.x &&
      mode.getHeight == resolution.y &&
      mode.getFrequency >= desktopMode.getFrequency &&
      mode.getBitsPerPixel >= 24 &&
      (if (fullscreen) mode.isFullscreenCapable else true)
    ).headOption
    
    val mode =
      if (detectedMode.isDefined) detectedMode.get
      else new DisplayMode(resolution.x, resolution.y)
    
    
    Display.setVSyncEnabled(settings.verticalSync)
    Display.setDisplayMode(mode)
    Display.setFullscreen(fullscreen)
    
    val pixelFormat = new PixelFormat().
      withBitsPerPixel(24).
      withAlphaBits(8).
      withDepthBits(24).
      withStencilBits(8).
      withSamples(settings.antiAliasingSamples)
      
    val glProfile = new ContextAttribs(2, 1)
    Display.setTitle(title)
    Display.create(pixelFormat, glProfile)
    
    RawKeyboard.create()
    RawMouse.create()
    
    _renderManager = new RenderManager(new RenderContext(detectGraphicsCaps(), settings.advanced))
    if (settings.capabilitiesLog) log(Level.INFO, renderManager.renderContext.capabilities.toString)
    
    
    timer.reset()
    
    init()
    reshape(ConstVec2i(0), resolution)
    
    while (!quit) {
      profiler1.begin()
      
      renderManager.renderContext.resetState()
      timer.update()
      val time = timer.timeStamp
      
      handleInput(time)
      if (Display.isCloseRequested) quit = true
      
      preUpdate(time)
      update(time)
      render(time)
      
      profiler2.begin()
      Display.update()
      profiler2.end()
      
      Thread.`yield`()
      
      manage()
      renderManager.renderContext.manage()
      
      if (settings.performanceLog && lastFps != timer.fps) {
        lastFps = timer.fps
        println("fps: " + lastFps)
      }
      
      //if (time.interval > 0.03) println(profiler1 + " | " + profiler2 +  " | " + time)
      profiler1.end()
    }
    
    renderManager.renderContext.cleanup()
    Display.destroy()
  }
  
  private def detectGraphicsCaps() = {
    import GL11._; import GL12._; import GL13._; import GL14._; import GL15._;
    import GL20._; import GL21._; import EXTTextureFilterAnisotropic._
    
    val extensions = glGetString(GL_EXTENSIONS)
    
    new GraphicsCapabilities(
      maxAnisotropyLevel =
        if (extensions.contains("GL_EXT_texture_filter_anisotropic")) glGetFloat(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT)
        else 1,
      maxVertexUniformComponents = glGetInteger(GL_MAX_VERTEX_UNIFORM_COMPONENTS),
      maxFragmentUniformComponents = glGetInteger(GL_MAX_FRAGMENT_UNIFORM_COMPONENTS),
      maxAttributes = glGetInteger(GL_MAX_VERTEX_ATTRIBS),
      maxVertexTextures = glGetInteger(GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS),
      maxFragmentTextures = glGetInteger(GL_MAX_TEXTURE_IMAGE_UNITS)
    )
  }
  
  private def handleInput(time: TimeStamp) {
    input.mouse.update() // Sync mouse cached state to system state.
    
    val listeners = inputListeners
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
      // XXX add mouse entered/exited using Mouse.isInsideWindow
    }
  }
  
  def dispose() {
    quit = true
  }
}
