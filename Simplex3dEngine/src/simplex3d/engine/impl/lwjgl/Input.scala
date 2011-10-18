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
package impl.lwjgl

import scala.annotation._
import org.lwjgl.input.{ Keyboard => RawKeyboard, Mouse => RawMouse}
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.engine.input._


private[lwjgl] class Keyboard extends input.Keyboard {
  
  def isKeyDown(keyCode: Int) :Boolean = {
    RawKeyboard.isKeyDown(encode(keyCode))
  }
  
  def decode(keyCode: Int) :Int = {
    import RawKeyboard._
    
    (keyCode: @switch) match {
      case KEY_NUMPAD0 => KeyCode.Num_0
      case KEY_NUMPAD1 => KeyCode.Num_1
      case KEY_NUMPAD2 => KeyCode.Num_2
      case KEY_NUMPAD3 => KeyCode.Num_3
      case KEY_NUMPAD4 => KeyCode.Num_4
      case KEY_NUMPAD5 => KeyCode.Num_5
      case KEY_NUMPAD6 => KeyCode.Num_6
      case KEY_NUMPAD7 => KeyCode.Num_7
      case KEY_NUMPAD8 => KeyCode.Num_8
      case KEY_NUMPAD9 => KeyCode.Num_9
      
      case KEY_NUMLOCK => KeyCode.Num_Lock
      case KEY_DIVIDE => KeyCode.Num_/
      case KEY_MULTIPLY => KeyCode.Num_*
      case KEY_SUBTRACT => KeyCode.Num_-
      case KEY_ADD => KeyCode.Num_+
      case KEY_NUMPADENTER => KeyCode.Num_Enter
      case KEY_DECIMAL => KeyCode.Num_Decimal
      
      case KEY_A => KeyCode.K_a
      case KEY_B => KeyCode.K_b
      case KEY_C => KeyCode.K_c
      case KEY_D => KeyCode.K_d
      case KEY_E => KeyCode.K_e
      case KEY_F => KeyCode.K_f
      case KEY_G => KeyCode.K_g
      case KEY_H => KeyCode.K_h
      case KEY_I => KeyCode.K_i
      case KEY_J => KeyCode.K_j
      case KEY_K => KeyCode.K_k
      case KEY_L => KeyCode.K_l
      case KEY_M => KeyCode.K_m
      case KEY_N => KeyCode.K_n
      case KEY_O => KeyCode.K_o
      case KEY_P => KeyCode.K_p
      case KEY_Q => KeyCode.K_q
      case KEY_R => KeyCode.K_r
      case KEY_S => KeyCode.K_s
      case KEY_T => KeyCode.K_t
      case KEY_U => KeyCode.K_u
      case KEY_V => KeyCode.K_v
      case KEY_W => KeyCode.K_w
      case KEY_X => KeyCode.K_x
      case KEY_Y => KeyCode.K_y
      case KEY_Z => KeyCode.K_z
      case KEY_SPACE => KeyCode.K_Space
      
      case KEY_LBRACKET => KeyCode.K_BraceLeft
      case KEY_RBRACKET => KeyCode.K_BraceRight
      case KEY_BACKSLASH => KeyCode.K_\
      case KEY_COLON => KeyCode.K_:
      case KEY_APOSTROPHE => KeyCode.K_Quote
      case KEY_COMMA => KeyCode.K_Comma
      case KEY_PERIOD => KeyCode.K_Period
      case KEY_SLASH => KeyCode.K_/
      
      case KEY_GRAVE => KeyCode.K_BackQuote
      case KEY_1 => KeyCode.K_1
      case KEY_2 => KeyCode.K_2
      case KEY_3 => KeyCode.K_3
      case KEY_4 => KeyCode.K_4
      case KEY_5 => KeyCode.K_5
      case KEY_6 => KeyCode.K_6
      case KEY_7 => KeyCode.K_7
      case KEY_8 => KeyCode.K_8
      case KEY_9 => KeyCode.K_9
      case KEY_0 => KeyCode.K_0
      case KEY_MINUS => KeyCode.K_-
      case KEY_EQUALS => KeyCode.K_Equals
      
      case KEY_SYSRQ => KeyCode.K_Printscreen
      case KEY_SCROLL => KeyCode.K_ScrollLock
      case KEY_PAUSE => KeyCode.K_Pause
      
      case KEY_INSERT => KeyCode.K_Insert
      case KEY_DELETE => KeyCode.K_Delete
      case KEY_HOME => KeyCode.K_Home
      case KEY_END => KeyCode.K_End
      case KEY_PRIOR => KeyCode.K_PageUp
      case KEY_NEXT => KeyCode.K_PageDown
      
      case KEY_LEFT => KeyCode.K_Left
      case KEY_RIGHT => KeyCode.K_Right
      case KEY_DOWN => KeyCode.K_Down
      case KEY_UP => KeyCode.K_Up
      
      case KEY_ESCAPE => KeyCode.K_Escape
      case KEY_BACK => KeyCode.K_BackSpace
      case KEY_TAB => KeyCode.K_Tab
      case KEY_CAPITAL => KeyCode.K_CapsLock
      case KEY_LSHIFT => KeyCode.K_LeftShift
      case KEY_RSHIFT => KeyCode.K_RightShift
      case KEY_LCONTROL => KeyCode.K_LeftControl
      case KEY_RCONTROL => KeyCode.K_RightControl
      case KEY_LMENU => KeyCode.K_LeftAlt
      case KEY_RMENU => KeyCode.K_RightAlt
      case KEY_RETURN => KeyCode.K_Enter
      
      case KEY_F1 => KeyCode.K_F1
      case KEY_F2 => KeyCode.K_F2
      case KEY_F3 => KeyCode.K_F3
      case KEY_F4 => KeyCode.K_F4
      case KEY_F5 => KeyCode.K_F5
      case KEY_F6 => KeyCode.K_F6
      case KEY_F7 => KeyCode.K_F7
      case KEY_F8 => KeyCode.K_F8
      case KEY_F9 => KeyCode.K_F9
      case KEY_F10 => KeyCode.K_F10
      case KEY_F11 => KeyCode.K_F11
      case KEY_F12 => KeyCode.K_F12
      
      // other F keys
      // other keys
      
      case _ => KeyCode.K_Undefined
    }
  }
  
  def encode(keyCode: Int) :Int = {
    import RawKeyboard._
    
    (keyCode: @switch) match {
      case KeyCode.Num_0 => KEY_NUMPAD0
      case KeyCode.Num_1 => KEY_NUMPAD1
      case KeyCode.Num_2 => KEY_NUMPAD2
      case KeyCode.Num_3 => KEY_NUMPAD3
      case KeyCode.Num_4 => KEY_NUMPAD4
      case KeyCode.Num_5 => KEY_NUMPAD5
      case KeyCode.Num_6 => KEY_NUMPAD6
      case KeyCode.Num_7 => KEY_NUMPAD7
      case KeyCode.Num_8 => KEY_NUMPAD8
      case KeyCode.Num_9 => KEY_NUMPAD9
        
      case KeyCode.Num_Lock => KEY_NUMLOCK
      case KeyCode.Num_/ => KEY_DIVIDE
      case KeyCode.Num_* => KEY_MULTIPLY
      case KeyCode.Num_- => KEY_SUBTRACT
      case KeyCode.Num_+ => KEY_ADD
      case KeyCode.Num_Enter => KEY_NUMPADENTER
      case KeyCode.Num_Decimal => KEY_DECIMAL
        
      case KeyCode.K_a => KEY_A
      case KeyCode.K_b => KEY_B
      case KeyCode.K_c => KEY_C
      case KeyCode.K_d => KEY_D
      case KeyCode.K_e => KEY_E
      case KeyCode.K_f => KEY_F
      case KeyCode.K_g => KEY_G
      case KeyCode.K_h => KEY_H
      case KeyCode.K_i => KEY_I
      case KeyCode.K_j => KEY_J
      case KeyCode.K_k => KEY_K
      case KeyCode.K_l => KEY_L
      case KeyCode.K_m => KEY_M
      case KeyCode.K_n => KEY_N
      case KeyCode.K_o => KEY_O
      case KeyCode.K_p => KEY_P
      case KeyCode.K_q => KEY_Q
      case KeyCode.K_r => KEY_R
      case KeyCode.K_s => KEY_S
      case KeyCode.K_t => KEY_T
      case KeyCode.K_u => KEY_U
      case KeyCode.K_v => KEY_V
      case KeyCode.K_w => KEY_W
      case KeyCode.K_x => KEY_X
      case KeyCode.K_y => KEY_Y
      case KeyCode.K_z => KEY_Z
      case KeyCode.K_Space => KEY_SPACE
        
      case KeyCode.K_BraceLeft => KEY_LBRACKET
      case KeyCode.K_BraceRight => KEY_RBRACKET
      case KeyCode.K_\ => KEY_BACKSLASH
      case KeyCode.K_: => KEY_COLON
      case KeyCode.K_Quote => KEY_APOSTROPHE
      case KeyCode.K_Comma => KEY_COMMA
      case KeyCode.K_Period => KEY_PERIOD
      case KeyCode.K_/ => KEY_SLASH
        
      case KeyCode.K_BackQuote => KEY_GRAVE
      case KeyCode.K_1 => KEY_1
      case KeyCode.K_2 => KEY_2
      case KeyCode.K_3 => KEY_3
      case KeyCode.K_4 => KEY_4
      case KeyCode.K_5 => KEY_5
      case KeyCode.K_6 => KEY_6
      case KeyCode.K_7 => KEY_7
      case KeyCode.K_8 => KEY_8
      case KeyCode.K_9 => KEY_9
      case KeyCode.K_0 => KEY_0
      case KeyCode.K_- => KEY_MINUS
      case KeyCode.K_Equals => KEY_EQUALS
        
      case KeyCode.K_Printscreen => KEY_SYSRQ
      case KeyCode.K_ScrollLock => KEY_SCROLL
      case KeyCode.K_Pause => KEY_PAUSE
        
      case KeyCode.K_Insert => KEY_INSERT
      case KeyCode.K_Delete => KEY_DELETE
      case KeyCode.K_Home => KEY_HOME
      case KeyCode.K_End => KEY_END
      case KeyCode.K_PageUp => KEY_PRIOR
      case KeyCode.K_PageDown => KEY_NEXT
        
      case KeyCode.K_Left => KEY_LEFT
      case KeyCode.K_Right => KEY_RIGHT
      case KeyCode.K_Down => KEY_DOWN
      case KeyCode.K_Up => KEY_UP
        
      case KeyCode.K_Escape => KEY_ESCAPE
      case KeyCode.K_BackSpace => KEY_BACK
      case KeyCode.K_Tab => KEY_TAB
      case KeyCode.K_CapsLock => KEY_CAPITAL
      case KeyCode.K_LeftShift => KEY_LSHIFT
      case KeyCode.K_RightShift => KEY_RSHIFT
      case KeyCode.K_LeftControl => KEY_LCONTROL
      case KeyCode.K_RightControl => KEY_RCONTROL
      case KeyCode.K_LeftAlt => KEY_LMENU
      case KeyCode.K_RightAlt => KEY_RMENU
      case KeyCode.K_Enter => KEY_RETURN
        
      case KeyCode.K_F1 => KEY_F1
      case KeyCode.K_F2 => KEY_F2
      case KeyCode.K_F3 => KEY_F3
      case KeyCode.K_F4 => KEY_F4
      case KeyCode.K_F5 => KEY_F5
      case KeyCode.K_F6 => KEY_F6
      case KeyCode.K_F7 => KEY_F7
      case KeyCode.K_F8 => KEY_F8
      case KeyCode.K_F9 => KEY_F9
      case KeyCode.K_F10 => KEY_F10
      case KeyCode.K_F11 => KEY_F11
      case KeyCode.K_F12 => KEY_F12
        
      // other F keys
      // other keys
      
      case _ => KEY_NONE
    }
  }
}


private[lwjgl] class Mouse extends input.Mouse {
  
  private var _delta = ConstVec2(0)
  private var _wheelDelta = 0.0
  private var _grabbed = false
  
  def update() {
    _grabbed = RawMouse.isGrabbed()
    
    _delta = ConstVec2(RawMouse.getDX, RawMouse.getDY)
    if (isGrabbed) _delta *= sensitivity
    
    _wheelDelta = RawMouse.getDWheel*wheelSensitivity
  }
  
  var sensitivity = ConstVec2(1)
  var wheelSensitivity = 1.0
  
  def isGrabbed_=(grabbed: Boolean) { _grabbed = grabbed; RawMouse.setGrabbed(grabbed) }
  def isGrabbed: Boolean = _grabbed
  
  def setPosition(position: inVec2i) { RawMouse.setCursorPosition(position.x, position.y) }

  def isButtonDown(buttonCode: Int) :Boolean = org.lwjgl.input.Mouse.isButtonDown(buttonCode)
  def position: Option[ConstVec2i] = {
    if (isGrabbed) None
    else Some(ConstVec2i(RawMouse.getX, RawMouse.getY))
  }
  def delta = _delta
  def wheelDelta: Double = _wheelDelta
}


private[lwjgl] class Input extends input.Input {
  val keyboard = new Keyboard
  val mouse = new Mouse
}
