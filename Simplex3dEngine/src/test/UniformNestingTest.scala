/*
 * Simplex3dEngine - Test Package
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

package test

import scala.collection.mutable.ArrayBuffer
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.engine._
import simplex3d.engine.graphics._


object UniformNestingTest extends FullscreenEffectApp with impl.lwjgl.App {
  val title = "Uniform Nesting Test"
  
  def main(args: Array[String]) {
    launch(new Settings(
      verticalSync = true,
      resolution = Some(Vec2i(800, 600))
    ))
  }

  
  sealed abstract class ReadStruct2 extends ReflectStruct[ReadStruct2] {
    type Mutable = Struct2
    protected def mkMutable() = new Mutable
    
    def value2: ReadVec3
    def value2Array: ReadBindingArray[ReadVec3]
    
    def texture2: ReadTextureRef[Texture2d[Vec3]]
  }
  
  final class Struct2 extends ReadStruct2 with Mutable[ReadStruct2] {
    val value2 = Vec3(0)
    val value2Array = new BindingArray[ReadVec3](Vec3.Zero, 2)
    
    val texture2 = new TextureRef[Texture2d[Vec3]]
    
    def :=(r: ReadStruct2) {
      value2 := r.value2
      value2Array := r.value2Array
      texture2 := r.texture2
    }
  }
  
  
  sealed abstract class ReadStruct1 extends ReflectStruct[ReadStruct1] {
    type Mutable = Struct1
    protected def mkMutable() = new Mutable
    
    def value1: ReadVec3
    def value1Array: ReadBindingArray[ReadVec3]
    
    def texture1: ReadTextureRef[Texture2d[Vec3]]
    def texture1Array: ReadBindingArray[ReadTextureRef[Texture2d[Vec3]]]
    
    def struct2: ReadStruct2
    def struct2Array: BindingArray[ReadStruct2]
  }
  
  final class Struct1 extends ReadStruct1 with Mutable[ReadStruct1] {
    val value1 = Vec3(0)
    val value1Array = new BindingArray[ReadVec3](Vec3.Zero, 2)
    
    val texture1 = new TextureRef[Texture2d[Vec3]]
    val texture1Array = new BindingArray[ReadTextureRef[Texture2d[Vec3]]](new TextureRef, 2)
    
    val struct2 = new Struct2
    val struct2Array = new BindingArray[ReadStruct2](new Struct2, 2)
    
    
    def :=(r: ReadStruct1) {
      value1 := r.value1
      value1Array := r.value1Array
      texture1 := r.texture1
      texture1Array := r.texture1Array
      struct2 := r.struct2
      struct2Array := r.struct2Array
    }
  }
  
  
  def mkTexture(color: inVec3) = {
    val dims = ConstVec2i(4)
    val data = DataBuffer[Vec3, UByte](dims.x*dims.y)
    
    var i = 0; while (i < data.size) {
      data(i) = color
      
      i += 1
    }
    
    Texture2d(dims, data)
  }
  

  val effect = new FullscreenEffect("Uniform Test") {
    
    // XXX change XProperty to not require ReadPrefix everywhere, same with BindingArray. Possibly add a WritableFactory interface.
    val value0 = ShaderProperty[ReadVec3](Vec3.Zero)
    val value0Array = ShaderProperty[ReadBindingArray[ReadVec3]](new BindingArrayFactory(Vec3.Zero, 2))
    
    val texture0 = ShaderProperty[ReadTextureRef[Texture2d[Vec3]]](new TextureRef)
    val texture0Array = ShaderProperty[ReadBindingArray[ReadTextureRef[Texture2d[Vec3]]]](
      new BindingArrayFactory(new TextureRef, 2)
    )
    
    val struct1 = ShaderProperty[ReadStruct1](new Struct1)
    val struct1Array = ShaderProperty[ReadBindingArray[ReadStruct1]](new BindingArrayFactory(new Struct1, 2))
    
    
    // Init textures.
    {
      texture0.mutable := mkTexture(Vec3(1, 0, 0))
      texture0Array.mutable(0) := mkTexture(Vec3(1, 0, 0))
      texture0Array.mutable(1) := mkTexture(Vec3(1, 0, 0))
      
      
      struct1.mutable.texture1 := mkTexture(Vec3(0, 1, 0))
      struct1.mutable.texture1Array(0) := mkTexture(Vec3(0, 1, 0))
      struct1.mutable.texture1Array(1) := mkTexture(Vec3(0, 1, 0))
      
      struct1.mutable.struct2.texture2 := mkTexture(Vec3(0, 0, 1))
      struct1.mutable.struct2Array(0).texture2 := mkTexture(Vec3(0, 0, 1))
      struct1.mutable.struct2Array(1).texture2 := mkTexture(Vec3(0, 0, 1))
      
      
      struct1Array.mutable(0).texture1 := mkTexture(Vec3(0, 1, 0))
      struct1Array.mutable(0).texture1Array(0) := mkTexture(Vec3(0, 1, 0))
      struct1Array.mutable(0).texture1Array(1) := mkTexture(Vec3(0, 1, 0))
      
      struct1Array.mutable(0).struct2.texture2 := mkTexture(Vec3(0, 0, 1))
      struct1Array.mutable(0).struct2Array(0).texture2 := mkTexture(Vec3(0, 0, 1))
      struct1Array.mutable(0).struct2Array(1).texture2 := mkTexture(Vec3(0, 0, 1))
      
      
      struct1Array.mutable(1).texture1 := mkTexture(Vec3(0, 1, 0))
      struct1Array.mutable(1).texture1Array(0) := mkTexture(Vec3(0, 1, 0))
      struct1Array.mutable(1).texture1Array(1) := mkTexture(Vec3(0, 1, 0))
      
      struct1Array.mutable(1).struct2.texture2 := mkTexture(Vec3(0, 0, 1))
      struct1Array.mutable(1).struct2Array(0).texture2 := mkTexture(Vec3(0, 0, 1))
      struct1Array.mutable(1).struct2Array(1).texture2 := mkTexture(Vec3(0, 0, 1))
    }
    
    val shaderSrc = """
      struct Struct2 {
        vec3 value2;
        vec3 value2Array[2];
      
        sampler2D texture2;
        sampler2D texture2Array[2];
      };
      
      struct Struct1 {
        vec3 value1;
        vec3 value1Array[2];
      
        sampler2D texture1;
        sampler2D texture1Array[2];
      
        Struct2 struct2;
        Struct2 struct2Array[2];
      };
      
      uniform vec3 value0;
      uniform vec3 value0Array[2];
      uniform sampler2D texture0;
      uniform sampler2D texture0Array[2];
      uniform Struct1 struct1;
      uniform Struct1 struct1Array[2];
      
      
      
      void main() {
        gl_FragColor =
          vec4(value0 + value0Array[0] + value0Array[1], 1) +
          texture2D(texture0, vec2(0)) + texture2D(texture0Array[0], vec2(0)) + texture2D(texture0Array[1], vec2(0)) +
          1.0
          ;
      }
    """
  }
}
