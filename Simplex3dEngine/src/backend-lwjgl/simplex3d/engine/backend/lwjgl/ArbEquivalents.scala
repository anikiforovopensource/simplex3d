/*
 * Simplex3dEngine - LWJGL Module
 * Copyright (C) 2012, Simplex3d Project
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

/**
 * 
 * @author Felix Dietze
 */
object ArbEquivalents {
  
  object GL15 {
    import ARBBufferObject._
    import ARBVertexBufferObject._
    
    @inline def glGenBuffers(buffers:java.nio.IntBuffer) = glGenBuffersARB(buffers)
    @inline def glBindBuffer(target:Int, buffer:Int) = glBindBufferARB(target, buffer)
    @inline def glBufferData(target:Int, data:java.nio.ByteBuffer, usage:Int) = glBufferDataARB(target, data, usage)
    @inline def glDeleteBuffers(buffer:Int) = glDeleteBuffersARB(buffer)
    @inline def glDeleteBuffers(buffers:java.nio.IntBuffer) = glDeleteBuffersARB(buffers)
    @inline def glBufferSubData(target:Int, offset:Long, data:java.nio.ByteBuffer) = glBufferSubDataARB(target, offset, data)
    
    @inline def GL_ELEMENT_ARRAY_BUFFER = GL_ELEMENT_ARRAY_BUFFER_ARB
    @inline def GL_ARRAY_BUFFER = GL_ARRAY_BUFFER_ARB
    @inline def GL_DYNAMIC_DRAW = GL_DYNAMIC_DRAW_ARB
    @inline def GL_STATIC_DRAW = GL_STATIC_DRAW_ARB
    @inline def GL_STREAM_DRAW = GL_STREAM_DRAW_ARB
  }


  object GL20 {
    import ARBShaderObjects._
    import ARBVertexShader._
    import ARBFragmentShader._
    import ARBPointSprite._

    @inline def glDeleteShader(obj:Int) = glDeleteObjectARB(obj)
    @inline def glDeleteProgram(obj:Int) = glDeleteObjectARB(obj)
    @inline def glCreateShader(shaderType:Int) = glCreateShaderObjectARB(shaderType)
    @inline def glShaderSource(shader:Int, string:java.lang.CharSequence) = glShaderSourceARB(shader,string)
    @inline def glCompileShader(shaderObj:Int) = glCompileShaderARB(shaderObj)
    @inline def glAttachShader(containerObj:Int, obj:Int) = glAttachObjectARB(containerObj, obj)
    @inline def glUseProgram(programObj:Int) = glUseProgramObjectARB(programObj)
    @inline def glCreateProgram() = glCreateProgramObjectARB()
    @inline def glLinkProgram(programObj:Int) = glLinkProgramARB(programObj)
    @inline def glGetActiveUniform(programObj:Int,index:Int,maxLength:Int, sizeType:java.nio.IntBuffer) = glGetActiveUniformARB(programObj,index,maxLength,sizeType)
    @inline def glGetUniformLocation(programObj:Int, name:java.lang.CharSequence) = glGetUniformLocationARB(programObj,name)
    @inline def glGetAttribLocation(programObj:Int, name:java.lang.CharSequence) = glGetAttribLocationARB(programObj,name)
    @inline def glGetActiveAttrib(programObj:Int, index:Int, maxLength:Int, sizeType:java.nio.IntBuffer) = glGetActiveAttribARB(programObj,index,maxLength,sizeType)
    @inline def glEnableVertexAttribArray(index:Int) = glEnableVertexAttribArrayARB(index)
    @inline def glVertexAttribPointer(index:Int, size:Int, `type`:Int, normalized:Boolean, stride:Int, buffer_buffer_offset:Long) = glVertexAttribPointerARB(index, size, `type`, normalized, stride, buffer_buffer_offset)
    @inline def glGetShaderInfoLog(obj:Int, maxLength:Int) = glGetInfoLogARB(obj,maxLength)
    @inline def glGetProgramInfoLog(obj:Int, maxLength:Int) = glGetInfoLogARB(obj,maxLength)
    @inline def glGetShader(shader:Int, pname:Int) = glGetObjectParameteriARB(shader,pname)
    @inline def glGetProgram(program:Int, pname:Int)= glGetObjectParameteriARB(program,pname)

    @inline def glUniformMatrix2(location:Int, transpose:Boolean, matrices:java.nio.FloatBuffer) = glUniformMatrix2ARB(location,transpose,matrices)
    @inline def glUniformMatrix3(location:Int, transpose:Boolean, matrices:java.nio.FloatBuffer) = glUniformMatrix3ARB(location,transpose,matrices)
    @inline def glUniformMatrix4(location:Int, transpose:Boolean, matrices:java.nio.FloatBuffer) = glUniformMatrix4ARB(location,transpose,matrices)

    @inline def glUniform1i(location:Int, v0:Int) = glUniform1iARB(location, v0)
    @inline def glUniform2i(location:Int, v0:Int, v1:Int) = glUniform2iARB(location, v0, v1)
    @inline def glUniform3i(location:Int, v0:Int, v1:Int, v2:Int) = glUniform3iARB(location, v0, v1, v2)
    @inline def glUniform4i(location:Int, v0:Int, v1:Int, v2:Int, v3:Int) = glUniform4iARB(location, v0, v1, v2, v3)

    @inline def glUniform1f(location:Int, v0:Float) = glUniform1fARB(location, v0)
    @inline def glUniform2f(location:Int, v0:Float, v1:Float) = glUniform2fARB(location, v0, v1)
    @inline def glUniform3f(location:Int, v0:Float, v1:Float, v2:Float) = glUniform3fARB(location, v0, v1, v2)
    @inline def glUniform4f(location:Int, v0:Float, v1:Float, v2:Float, v3:Float) = glUniform4fARB(location, v0, v1, v2, v3)

    @inline def GL_VERTEX_SHADER = GL_VERTEX_SHADER_ARB
    @inline def GL_COMPILE_STATUS = GL_OBJECT_COMPILE_STATUS_ARB
    @inline def GL_LINK_STATUS = GL_OBJECT_LINK_STATUS_ARB
    @inline def GL_ACTIVE_UNIFORMS = GL_OBJECT_ACTIVE_UNIFORMS_ARB
    @inline def GL_ACTIVE_ATTRIBUTES = GL_OBJECT_ACTIVE_ATTRIBUTES_ARB
    @inline def GL_ACTIVE_UNIFORM_MAX_LENGTH = GL_OBJECT_ACTIVE_UNIFORM_MAX_LENGTH_ARB
    @inline def GL_ACTIVE_ATTRIBUTE_MAX_LENGTH = GL_OBJECT_ACTIVE_ATTRIBUTE_MAX_LENGTH_ARB
    @inline def GL_FRAGMENT_SHADER = GL_FRAGMENT_SHADER_ARB
    @inline def GL_INFO_LOG_LENGTH = GL_OBJECT_INFO_LOG_LENGTH_ARB
    @inline def GL_POINT_SPRITE = GL_POINT_SPRITE_ARB
    @inline def GL_VERTEX_PROGRAM_POINT_SIZE = GL_VERTEX_PROGRAM_POINT_SIZE_ARB

    // didn't find any ARB equivalents for:
    @inline def GL_POINT_SPRITE_COORD_ORIGIN:Int = GL20.GL_POINT_SPRITE_COORD_ORIGIN
    @inline def GL_LOWER_LEFT:Int = GL20.GL_LOWER_LEFT
  }
}
