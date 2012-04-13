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

package simplex3d.test.engine

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.algorithm.noise._
import simplex3d.algorithm.mesh._
import simplex3d.engine._
import simplex3d.engine.util._
import simplex3d.engine.graphics._
import simplex3d.engine.bounding._
import simplex3d.engine.input._
import simplex3d.engine.input.handler._
import simplex3d.engine.scenegraph._


/** This test checks environment propagation under different conditions
 * by using color, intensity, and contrast as environmental properties.
 * 
 * This test is meant for debugging rather than a demonstration.
 * 
 * Colors:
 * Colors are used to test propagation by alternating red, green, and blue.
 * Since odd nodes are empty, this pattern gets inverted for visible meshes to red, blue, and green.
 * The last cube is red because its' closest two parents do not defined the color, so the last color gets inherited.
 * 
 * Intensity:
 * Intensity is set at the level of the second and fourth cubes. So when intensity property on the second cube
 * is undefined every 2 seconds, the second and third cubes reset to the default
 * while forth and fifth cubes hold the intensity value.
 * 
 * Contrast:
 * The contrast alternates between an array of one and two factors every 4 seconds.
 * This servers as a test for dynamic binding resolution, which allows the same environmental property to
 * resolve to different structs depending on the circumstances.//XXX
 * Contrast inherits from UpdatableEnvironmentalEffect which can alter its' binding uniquely for each mesh
 * depending on the camera matrices. When the camera is moved further from the meshes,
 * they gradually loose color and become white.
 * 
 * The printout indicates what Meshes have their techniques re-evaluated.
 * Notice that techniques do not change every frame, but only when the properties are altered in a way that requires
 * a different shader. The bindings are resolved only in response to technique changes.
 * These optimizations avoid expensive operations and results in exceptional performance
 * without sacrificing the flexibility. 
 */
object DynamicEnvironment extends App with scala.App {
  val title = "Dynamic Environment"

  val config = new Config
  
  lazy val settings = new Settings(
    fullscreen = false,
    verticalSync = true,
    antiAliasingSamples = 4,
    resolution = Some(Vec2i(800, 600))
  )

  addInputListener(new InputListener {
    override val keyboardListener = new KeyboardListener {
      override def keyTyped(input: Input, e: KeyEvent) {
        if (KeyCode.K_Escape == e.keyCode) dispose()
      }
    }
  })
  
  
  type TestT = simplex3d.engine.transformation.ComponentTransformation3dContext
  type TestG = testenv.GraphicsContext
  
  implicit final val TransformationContext = new TestT
  implicit final val GraphicsContext = new TestG
  
  
  val scene = new SceneGraph("World", new SceneGraphSettings, new Camera("World Camera"), new testenv.TechniqueManager)

  
  var nodes: Array[Node[TestT, TestG]] = _
  
  def init() {
    scene.camera.transformation.mutable.translation := Vec3(-25, 25, 100)
    
    val camControls = new FirstPersonHandler(scene.camera.transformation)
    addInputListener(camControls)
    addInputListener(new MouseGrabber(true)(KeyCode.Num_Enter, KeyCode.K_Enter)(camControls)())
    
    
    val (indices, vertices, _, _) = Shapes.makeBox()
    val lineIndices = DataBuffer[SInt, UByte](indices.size/3 * 6)
    MeshConversion.linesFromTriangles(indices, vertices, lineIndices)
    
    var node = new Node("Root")
    
    nodes = (for (i <- 0 until 9) yield {
      val newnode = new Node("Node Level " + i)
      node.appendChild(newnode)
      node = newnode
      
      if (i % 2 == 0) {
        val mesh = new Mesh("Cube Level " + i)
        
        mesh.geometry.mode = Lines(3)
        
        mesh.geometry.indices.defineAs(Attributes(lineIndices))
        mesh.geometry.vertices.defineAs(Attributes(vertices))
        
        val scale = 50 - i/2*10
        mesh.transformation.mutable.translation := Vec3(-0.5*scale, 0.5*scale, 0.5*scale)*0.9999
        mesh.transformation.mutable.scale := scale
        
        node.appendChild(mesh)
      }
      
      node
    }).toArray

    
    nodes(6).environment.intensity.mutable.value := 0.75
    for (i <- 0 until 7) { nodes(i).environment.nodeColor.mutable.color := Vec3(1, 0, 0) }
    nodes(0).environment.contrast.mutable.factor := 0.1
    
    scene.attach(nodes(0))
  } 
  
  def update(time: TimeStamp) {
    if (time.total.toInt %2 == 0) nodes(1).environment.intensity.mutable.value := 0.75
    else nodes(1).environment.intensity.undefine()
    
    if (time.total.toInt %4 == 0) nodes(0).environment.contrast.mutable.secondary := true
    else nodes(0).environment.contrast.mutable.secondary := false
  }
  
  
  // App methods.
  import SceneAccess._
  
  override def preUpdate(time: TimeStamp) {
    scene.update(time)
  }
  
  override def render(time: TimeStamp) {
    scene.render(renderManager, time)
  }
  
  override def manage() {
    scene.manage(renderManager.renderContext, timer.frameTimer, 0.01)
  }
  
  override def reshape(position: inVec2i, contrastensions: inVec2i) {
    val aspect = contrastensions.x.toDouble/contrastensions.y
    scene.camera.projection := perspectiveProj(radians(60), aspect, 10, 500)
  }
  
  override def main(args: Array[String]) = {
    super.main(args)
    launch()
  }
}

package testenv {
  
  sealed abstract class ReadIntensity extends ReadStruct[Intensity] {
    def value: ReadDoubleRef
  }
  
  final class Intensity extends ReadIntensity with EnvironmentalEffect[Intensity] {
    type Read = ReadIntensity
    protected def mkMutable() = new Intensity
    
    val value = new DoubleRef(1)
  
    def :=(r: ReadStruct[Intensity]) {
      val t = r.asInstanceOf[Intensity]
      value := t.value
    }
    
    def propagate(parentVal: ReadIntensity, result: Intensity) {
      result.value := value
    }
    
    protected def resolveBinding() = value
  }
  
  
  sealed abstract class ReadNodeColor extends ReadStruct[NodeColor] {
    def color: ReadVec3
  }
  
  final class NodeColor extends ReadNodeColor with EnvironmentalEffect[NodeColor] {
    type Read = ReadNodeColor
    protected def mkMutable() = new NodeColor
    
    val color = Vec3(1)
  
    def :=(r: ReadStruct[NodeColor]) {
      val t = r.asInstanceOf[NodeColor]
      color := t.color
    }
    
    def propagate(parentVal: ReadNodeColor, result: NodeColor) {
      if (parentVal.color == Vec3.UnitX) result.color := Vec3.UnitY
      else if (parentVal.color == Vec3.UnitY) result.color := Vec3.UnitZ
      else if (parentVal.color == Vec3.UnitZ) result.color := Vec3.UnitX
      else result.color := Vec3.One
    }
    
    protected def resolveBinding() = color
  }
  
  
  sealed abstract class ReadContrast extends ReadStruct[Contrast] {
    def factor: ReadDoubleRef
    def secondary: ReadBooleanRef
  }
  
  final class Contrast extends ReadContrast with UpdatableEnvironmentalEffect[Contrast] {
    type Read = ReadContrast
    protected def mkMutable() = new Contrast
    
    val factor = new DoubleRef(0)
    val secondary = new BooleanRef(false)
  
    def :=(r: ReadStruct[Contrast]) {
      val t = r.asInstanceOf[Contrast]
      
      factor := t.factor
      if (secondary != t.secondary) signalBindingChanges()
      secondary := t.secondary
    }
    
    def propagate(parentVal: ReadContrast, result: Contrast) {
      result.factor := factor
      if (result.secondary != secondary) signalBindingChanges()
      result.secondary := secondary
    }
    
    protected def resolveBinding() = {
      println("Resolving contrast binding.")
            
      val binding = 
        if (secondary) {
          val b = new ContrastBinding2
          b.factor1 := factor
          b.factor2 := factor
          b
        }
        else {
          val b = new ContrastBinding1
          b.factor1 := factor
          b
        }
      
      binding.asInstanceOf[TechniqueBinding]
    }
    
    def updateBinding(predefinedUniforms: ReadPredefinedUniforms) {
      val factorRef = binding match {
        case b: ContrastBinding1 => b.factor1
        case b: ContrastBinding2 => b.factor1
      }
      factorRef := length(predefinedUniforms.se_modelViewMatrix(3))*0.004
    }
  }
  
  final class ContrastBinding1 extends prototype.Struct[ContrastBinding1] {
    type Read = ContrastBinding1
    protected def mkMutable() = new ContrastBinding1
    
    val factor1 = new DoubleRef(0)
    
    init(classOf[ContrastBinding1])
  }
  
  final class ContrastBinding2 extends prototype.Struct[ContrastBinding2] {
    type Read = ContrastBinding2
    protected def mkMutable() = new ContrastBinding2
    
    val factor1 = new DoubleRef(0)
    val factor2 = new DoubleRef(0)
    
    init(classOf[ContrastBinding2])
  }
  
  
  class Environment extends prototype.Environment {
    val intensity = Optional[Intensity](new Intensity)
    val nodeColor = Optional[NodeColor](new NodeColor)
    val contrast = Optional[Contrast](new Contrast)
    
    init(classOf[Environment])
  }
  
  
  final class GraphicsContext extends graphics.GraphicsContext {
    type Geometry = renderer.Geometry
    type Material = renderer.Material
    type Environment = testenv.Environment
    
    def mkGeometry() = new Geometry
    def mkMaterial() = new Material
    def mkEnvironment() = new Environment
    
    init()
  }
  
  
  final class TechniqueManager[G <: GraphicsContext](implicit val graphicsContext: G) extends graphics.TechniqueManager[G] {
    val passManager = new graphics.pluggable.PassManager[G]
    
    val vertexShader = new Shader(Shader.Vertex, """
      uniform mat4 se_modelViewProjectionMatrix;
      attribute vec3 vertices;
      
      void main() {
        gl_Position = se_modelViewProjectionMatrix*vec4(vertices, 1.0);
      }
      """
    )
    
    val fragmentShader = new Shader(Shader.Fragment, """
        uniform vec3 nodeColor;
        
        float resolveIntensity();
        float resolveContrastFactor();
        
        void main() {
          vec3 color = nodeColor * resolveIntensity() + vec3(resolveContrastFactor());
          gl_FragColor = vec4(color, 1);
        }
      """
    )
      
    val withoutIntensity = new Shader(Shader.Fragment, """
        float resolveIntensity() {
          return 0.25;
        }
      """
    )
    
    val withIntensity = new Shader(Shader.Fragment, """
        uniform float intensity;
        
        float resolveIntensity() {
          return intensity;
        }
      """
    )
    
    val contrast1 = new Shader(Shader.Fragment, """
        struct Contrast1 {
          float factor1;
        };
        
        uniform Contrast1 contrast;
        
        float resolveContrastFactor() {
          return contrast.factor1;
        }
      """
    )
    
    val contrast2 = new Shader(Shader.Fragment, """
        struct Contrast2 {
          float factor1;
          float factor2;
        };
        
        uniform Contrast2 contrast;
        
        float resolveContrastFactor() {
          return contrast.factor1 + contrast.factor2;
        }
      """
    )
  
    
    private val cache = new java.util.HashMap[(Shader, Shader), Technique]
    def getTechnique(intensityShader: Shader, contrastShader: Shader) :Technique = {
      val key = (intensityShader, contrastShader)
      var technique = cache.get(key)
      if (technique == null) {
        technique = new Technique(
          graphicsContext,
          Set(vertexShader, fragmentShader, intensityShader, contrastShader)
        )
        cache.put(key, technique)
      }
      technique
    }
    
    def resolveTechnique(
      meshName: String,
      geometry: G#Geometry, material: G#Material, worldEnvironment: G#Environment
    )
    :Technique = {
      
      assert(worldEnvironment.nodeColor.isDefined)
      assert(worldEnvironment.contrast.isDefined)
      
      
      print("Resolving technique for mesh '" + meshName + "': ")
      
      val intensityShader =
        if (worldEnvironment.intensity.isDefined) {
          print("with intensity, ")
          withIntensity
        }
        else {
          print("without intensity, ")
          withoutIntensity
        }
      
      val contrastShader =
        if (worldEnvironment.contrast.defined.secondary) {
          println("with contrast2.")
          contrast2
        }
        else {
          println("with contrast1.")
          contrast1
        }
      
      getTechnique(intensityShader, contrastShader)
    }
  }
}
