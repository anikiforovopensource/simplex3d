package simplex3d.example.engine

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.algorithm.noise._
import simplex3d.algorithm.mesh._
import simplex3d.engine._
import simplex3d.engine.util._
import simplex3d.engine.transformation._
import simplex3d.engine.graphics._
import simplex3d.engine.graphics.pluggable._
import simplex3d.engine.scene._
import simplex3d.engine.input._
import simplex3d.engine.input.handler._
import simplex3d.scenegraph._


object CustomRenderer extends simplex3d.vanilla.BaseApp {
  
  def main(args: Array[String]) {
    launch()
  }
  
  val title = "Custom Renderer"
  
  // Configure application settings.
  override lazy val settings = new Settings(
    fullscreen = false,
    verticalSync = true,
    logPerformance = true,
    resolution = Some(Vec2i(800, 600))
  )
  
  
  // Custom Transformation and Graphics contexts.
  implicit val TransformationContext = new Transformation
  implicit val GraphicsContext = new GraphicsContext
  
  
  // Initialize the application.
  def init() {
    
    // Add exit on Esc.
    addInputListener(new InputListener {
      override val keyboardListener = new KeyboardListener {
        override def keyTyped(input: Input, e: KeyEvent) {
          if (KeyCode.K_Escape == e.keyCode) dispose()
        }
      }
    })
    
    // Position the camera.
    world.camera.transformation.update.translation := Vec3(-10, 25, 100)
    world.camera.transformation.update.lookAt(Vec3.Zero, Vec3.UnitY, true)
    
    // Init camera controls.
    addInputListener(new MouseGrabber(false)(KeyCode.Num_Enter, KeyCode.K_Enter))
    addInputListener(new FirstPersonHandler(world.camera.transformation))
    
    // Init the mesh.
    val mesh = new Mesh("Cube")
    
    // Generate box and attach attributes to the mesh.
    val (indices, vertices, normals, texCoords) = Shapes.makeBox()
    mesh.geometry.indices := Attributes.fromData(indices)
    mesh.geometry.vertices := Attributes.fromData(vertices)
    mesh.geometry.normals := Attributes.fromData(normals)
    
    mesh.geometry.texCoords := Attributes.fromData(texCoords)
    
    // Generate and attach textures.
    val noise = ClassicalGradientNoise
    
    val tileSize = 256
    val tiledNoise = new TiledNoiseSum(
      source = new ClassicalGradientNoise(System.currentTimeMillis.toInt),
      tile = ConstVec4(tileSize),
      frequency = 0.2,
      octaves = 1
    )
    
    val objectTexture = Texture2d[Vec3](Vec2i(128)).fillWith { p =>
      val intensity = (noise(p.x*0.06, p.y*0.06) + 1)*0.5
      Vec3(0, intensity, intensity) + 0.2
    }
    
    val detailTexture = Texture2d[Vec3](Vec2i(tileSize)).fillWith { p =>
      val intensity = abs(tiledNoise(p.x, p.y) + 0.3) + 0.3
      Vec3(intensity, intensity, intensity)
    }
    
    
    mesh.material.textureUnits.update += new TextureUnit(
      objectTexture, Mat3x2.Identity
    )
    mesh.material.textureUnits.update += new TextureUnit(
      detailTexture, Mat3x2.scale(3).rotate(radians(30))
    )
    
    // Position the mesh.
    mesh.transformation.update.rotation :=
      Quat4 rotateX(radians(25)) rotateY(radians(-30))
      
    mesh.transformation.update.scale := 40
    
    mesh.shaderDebugging.logAccepted = true
    mesh.shaderDebugging.logRejected = true
    mesh.shaderDebugging.logGeneratedSource = true
    
    // Attach the mesh to the scenegraph.
    world.attach(mesh)
    
    
    // Attach lights.
    world.environment.lighting.update.lights += new PointLight(Vec3(4), 0.1, 0)
    world.environment.lighting.update.lights += new PointLight(Vec3(6), 0.1, 0)
    
    // Init and attach light indicators.
    lightMesh.geometry.vertices := Attributes[Vec3, RFloat](maxLightCount)
    lightMesh.geometry.primitive.update.mode := VertexMode.Points
    lightMesh.geometry.primitive.update.pointSize := 5
    
    // Reuse texture rendering for lights.
    lightMesh.geometry.texCoords := Attributes[Vec2, RFloat](maxLightCount)
    val lightTexture = Texture2d[Vec3](Vec2i(4)).fillWith { p => Vec3(1) }
    lightMesh.material.textureUnits.update += new TextureUnit(lightTexture, Mat3x2.Identity)
    
    // Set vertex coordinates that will be used to position lights.
    lightMesh.elementRange.update.count := 2
    lightMesh.geometry.vertices.write(2) = Vec3(0, 40, 0)
    lightMesh.geometry.vertices.write(3) = Vec3(-40, 0, 40)
    
    world.attach(lightMesh)
  }
  
  
  // Setup light representation.
  val maxLightCount = 4
  val lightMesh = new Mesh("Lights")
  
  val lightPositions = Array(Vec3(1, 0.5, 0)*40, Vec3(1, -0.5, 0)*60)
  
  val period = 15
  val rotationSpeeds = Array(radians(2*360/period), radians(1.5*360/period))
  
  var lightsOn = false
  

  def update(time: TimeStamp) {
    
    val lights = world.environment.lighting.update.lights
    val lightVertices = lightMesh.geometry.vertices.write
    
    // Turn extra lights on and off periodically.
    if (mod(time.total, period) > period*0.5) {
      if (!lightsOn) {
        lightsOn = true
        println("Extra lights on.")
        
        for (i <- 2 until maxLightCount) {
          val light = new PointLight(Vec3(4), 0.1, 0)
          light.position := lightVertices(i)
          lights += light
        }
        
        lightMesh.elementRange.update.count := maxLightCount
      }
    }
    else {
      if (lightsOn) {
        lightsOn = false
        println("Extra lights off.")
        
        lights.take(lightPositions.size)
        lightMesh.elementRange.update.count := lightPositions.size
      }
    }
    
    // Animate moving lights.
    for (i <- 0 until 2) {
      val transformation = Mat4x3.rotateY(time.total*rotationSpeeds(i))
      val position = transformation.transformPoint(lightPositions(i))
      lights(i).position := position
      lightVertices(i) = position
    }
  }
  
  
  // Declare TextureUnit Struct.
  sealed abstract class ReadTextureUnit extends prototype.ReadStruct {
    type Read = ReadTextureUnit
    type Mutable = TextureUnit
    
    def texture: ReadTextureBinding[Texture2d[_]]
    def transformation: ReadMat3x2
  }
  
  final class TextureUnit extends ReadTextureUnit with prototype.Struct {
    protected def mkMutable() = new TextureUnit
    
    def this(texture: Texture2d[_], transformation: inMat3x2) {
      this()
      
      this.texture := texture
      this.transformation := transformation
    }
    
    val texture = new TextureBinding[Texture2d[_]]
    val transformation = Mat3x2(1)
    
    init(classOf[TextureUnit])
  }
  
  
  // Replace default geometry, material, and environment.
  class Geometry extends prototype.Geometry {
    val texCoords = AttributeBinding[Vec2, RFloat]
    
    init(classOf[Geometry])
  }
  class Material(controllerContext: ControllerContext) extends prototype.Material(controllerContext) {
    val textureUnits = Property(() => new BindingList[TextureUnit])
    
    init(classOf[Material])
  }
  
  class Environment(controllerContext: ControllerContext) extends prototype.Environment(controllerContext) {
    val lighting = Property(() => new Lighting)
    
    init(classOf[Environment])
  }
  
  
  // Define custom contexts. This allows to inject custom material and geometry implementations.
  type Transformation = ComponentTransformation3dContext
  
  class GraphicsContext extends graphics.GraphicsContext {
    type Geometry = CustomRenderer.Geometry
    type Material = CustomRenderer.Material
    type Environment = CustomRenderer.Environment
    
    def mkGeometry() = new Geometry
    def mkMaterial(controllerContext: ControllerContext) = new Material(controllerContext)
    def mkEnvironment(controllerContext: ControllerContext) = new Environment(controllerContext)
    
    init()
  }
  type Graphics = GraphicsContext
  
  
  // Declare a new technique manager and attach it to the scenegraph.
  val techniqueManager = new pluggable.TechniqueManager
  
  protected val world = new SceneGraph(
    "World",
    sceneGraphSettings,
    new Camera("Main Camera"),
    techniqueManager
  )
  
  
  // Rebuild the Technique Manager from scratch.
  techniqueManager.push(new FragmentShader {
    use("vec4 texturingColor()")
    use("vec4 lightIntensity()")
    
    in("transformationCtx") {
      declare[Vec4]("gl_FragCoord")
    }
    
    main("resolveColor")(){"""
      gl_FragColor = texturingColor() * lightIntensity();
    """}
  }.toPrototype(Profile.Gl2))
  
  techniqueManager.push(new FragmentShader {
    function("vec4 lightIntensity()"){"""
      return vec4(1.0);
    """}
  }.toPrototype(Profile.Gl2))
  
  techniqueManager.push(new FragmentShader {
    uniform {
      declare[BindingList[TextureUnit]]("textureUnits")
    }
    
    in("texturingCtx") {
      declare[BindingList[Vec2]]("ecTexCoords").size("se_sizeOf_textureUnits")
    }
    
    function("vec4 texturingColor()"){"""
      vec4 color = vec4(1.0);
      for (int i = 0; i < se_sizeOf_textureUnits; i++) {
        color *= texture2D(texture[i], texturingCtx.ecTexCoords[i]);
      }
      return color;
    """}
  }.toPrototype(Profile.Gl2))
  
  techniqueManager.push(new VertexShader {
    uniform {
      declare[Mat4]("se_modelViewProjectionMatrix")
    }
    
    attributes {
      declare[Vec3]("vertices")
    }
    
    out("transformationCtx") {
      declare[Vec4]("gl_Position")
    }
    
    main("transformVertices")(){"""
      gl_Position = se_modelViewProjectionMatrix*vec4(vertices, 1.0);
    """}
  }.toPrototype(Profile.Gl2))
    
  techniqueManager.push(new VertexShader {
    uniform {
      declare[BindingList[TextureUnit]]("textureUnits")
    }
    
    attributes {
      declare[Vec2]("texCoords")
    }
    
    out("texturingCtx") {
      declare[BindingList[Vec2]]("ecTexCoords").size("se_sizeOf_textureUnits")
    }
    
    main("propagateTexturingValues")(){"""
      for (int i = 0; i < se_sizeOf_textureUnits; i++) {
        vec3 transformed = textureUnits[i].transformation*vec3(texCoords, 1);
        texturingCtx.ecTexCoords[i] = transformed.xy;
      }
    """}
  }.toPrototype(Profile.Gl2))
  
  
  // Declare PointLight Struct.
  sealed abstract class ReadPointLight extends prototype.ReadStruct {
    type Read = ReadPointLight
    type Mutable = PointLight
    
    def position: ReadVec3
    def intensity: ReadVec3
    def linearAttenuation: ReadDoubleRef
    def quadraticAttenuation: ReadDoubleRef
  }
  
  final class PointLight extends ReadPointLight with prototype.Struct {
    protected def mkMutable() = new PointLight
    
    def this(
      intensity: inVec3,
      linearAttenuation: Double,
      quadraticAttenuation: Double
    ) {
      this()
      
      this.intensity := intensity
      this.linearAttenuation := linearAttenuation
      this.quadraticAttenuation := quadraticAttenuation
    }
    
    val position = Vec3(0)
    val intensity = Vec3(1)
    val linearAttenuation = new DoubleRef(0)
    val quadraticAttenuation = new DoubleRef(0)
    
    private[CustomRenderer] val ecPosition = Vec3(0)
    
    init(classOf[PointLight])
  }
  
  
  // Declare Lighting Environment.
  sealed abstract class ReadLighting extends ReadUpdatableEnvironmentalEffect {
    type Read = ReadLighting
    type Mutable = Lighting
    
    def lights: BindingSeq[PointLight]
  }
  
  final class Lighting
  extends ReadLighting with UpdatableEnvironmentalEffect 
  {
    protected def mkMutable() = new Lighting
    
    val lights = new BindingList[PointLight]
    
    def :=(e: ReadLighting) {
      if (lights.size != e.lights.size) signalStructuralChanges()
      lights := e.lights
    }
    
    def propagate(parentVal: ReadLighting, result: Lighting) {
      val oldSize = result.lights.size
      result.lights.clear()
      result.lights ++= lights
      result.lights ++= parentVal.lights
      
      if (result.lights.size > maxLightCount) result.lights.take(maxLightCount)
      if (result.lights.size != oldSize) result.signalStructuralChanges()
    }
    
    protected def resolveBinding() = lights
    
    def updateBinding(predefinedUniforms: ReadPredefinedUniforms) {
      val s = lights.size; var i = 0; while (i < s) {
        val t = predefinedUniforms.se_viewMatrix.transformPoint(lights(i).position)
        lights(i).ecPosition := t
        
        i += 1
      }
    }
  }
  
  
  // Declare lighting shaders.
  techniqueManager.push(new FragmentShader {
    debugging.logRejected = true
    debugging.logGeneratedSource = true
    
    uniform {
      declare[BindingList[PointLight]]("lighting")
    }
    
    in("lightingCtx") {
      declare[Vec3]("normal")
      declare[Vec3]("ecPosition")
    }
    
    function("vec4 lightIntensity()"){"""
      vec3 intensity = vec3(0.0);
      for (int i = 0; i < se_sizeOf_lighting; i++) {
    
        vec3 lightDir = lighting[i].ecPosition - lightingCtx.ecPosition;
        float dist = length(lightDir);
        float attenuation = 1.0 / (1.0 +
          lighting[i].linearAttenuation * dist +
          lighting[i].quadraticAttenuation * dist*dist
        );
      
        lightDir = lightDir/dist;
        float diffuseFactor = max(0.0, dot(lightingCtx.normal, lightDir));
        intensity += lighting[i].intensity * diffuseFactor * attenuation;
      }
      return vec4(intensity + 0.2, 1.0);
    """}
  }.toPrototype(Profile.Gl2))
  
  
  techniqueManager.push(new VertexShader {
    uniform {
      declare[Mat4]("se_modelViewMatrix")
      declare[Mat3]("se_normalMatrix")
    }
    
    attributes {
      declare[Vec3]("vertices")
      declare[Vec3]("normals")
    }
    
    out("lightingCtx") {
      declare[Vec3]("ecPosition")
      declare[Vec3]("normal")
    }
    
    main("propagateLightingValues")(){"""
      lightingCtx.ecPosition = (se_modelViewMatrix*vec4(vertices, 1.0)).xyz;
      lightingCtx.normal = normalize(se_normalMatrix*normals);
    """}
  }.toPrototype(Profile.Gl2))
}
