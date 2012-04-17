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

import java.util.logging._
import org.lwjgl.opengl._
import org.lwjgl.input.{Keyboard => RawKeyboard, Mouse => RawMouse }
import simplex3d.math._
import simplex3d.engine.graphics._


class FixedResolutionLauncher extends simplex3d.engine.Launcher {
  val driver = "lwjgl"
    
  @volatile private var quit = false
  @volatile private var running = false
  
  def launch(title: String, settings: Settings, app: App#Subtext, loop: simplex3d.engine.MainLoop) :Object = {
    running = true
    
    val thread = new Thread(new Runnable() { def run() {
      
      if (Display.isCreated()) {
        running = false
        
        throw new IllegalStateException(
          "Already using the native display. Only one native display per JVM can be used with this launcher."
        )
      }
      
      running = true
      
      
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
      
      val graphicsCapabilities = detectGraphicsCapabilities()
      app.renderManager.init(graphicsCapabilities, settings.advanced)
      if (settings.logCapabilities) Logger.getLogger(this.getClass.getName).log(Level.INFO, graphicsCapabilities.toString)
      
      app.timer.reset()
      app.init()
      app.reshape(ConstVec2i(0), app.renderManager.renderContext.viewportDimensions())
      
      var localQuit = false
      while (!quit && !localQuit) {
        localQuit = loop.body(app)
      }
      
      
      app.renderManager.renderContext.cleanup()
      
      RawKeyboard.destroy()
      RawMouse.destroy()
      Display.destroy()
      
      quit = false// Allow to re-launch.
      running = false
    }})
    
    thread.start()
    
    null // Launching in a native window.
  }
  
  def detectGraphicsCapabilities() = {
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
  
  def isRunning = running
  
  def dispose() {
    quit = true
  }
  
  def disposeAndWait() {
    dispose()
    while (isRunning) { Thread.sleep(1) }
  }
}
