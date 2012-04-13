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
import simplex3d.engine.scenegraph._
import simplex3d.engine.default._
import simplex3d.engine.input._
import simplex3d.engine.input.handler._


object CustomRenderer extends BasicApp {
  val title = "Custom Renderer"
  
  // Configure application settings.
  override lazy val settings = new Settings(
    fullscreen = false,
    verticalSync = true,
    logPerformance = true,
    antiAliasingSamples = 4,
    resolution = Some(Vec2i(800, 600))
  )
  
  
  // Declare TextureUnit Struct.
  sealed abstract class ReadTextureUnit extends ReadStruct[TextureUnit] {
    def texture: ReadTextureBinding[Texture2d[_]]
    def scale: ReadDoubleRef
  }

  final class TextureUnit extends ReadTextureUnit with prototype.Struct[TextureUnit] {
    def this(texture: Texture2d[_], scale: Double) {
      this()
      
      this.texture := texture
      this.scale := scale
    }
    
    type Read = ReadTextureUnit
    protected def mkMutable() = new TextureUnit
    
    val texture = new TextureBinding[Texture2d[_]]
    val scale = new DoubleRef(1)
    
    init(classOf[TextureUnit])
  }
  
  
  // Declare PointLight Struct.
  sealed abstract class ReadPointLight extends ReadStruct[PointLight] {
    def position: ReadVec3
    def intensity: ReadVec3
    def linearAttenuation: ReadDoubleRef
    def quadraticAttenuation: ReadDoubleRef
  }

  final class PointLight extends ReadPointLight with prototype.Struct[PointLight] {
    def this(intensity: inVec3, linearAttenuation: Double, quadraticAttenuation: Double) {
      this()
      
      this.intensity := intensity
      this.linearAttenuation := linearAttenuation
      this.quadraticAttenuation := quadraticAttenuation
    }
    
    type Read = ReadPointLight
    protected def mkMutable() = new PointLight
    
    val position = Vec3(0)
    val intensity = Vec3(1)
    val linearAttenuation = new DoubleRef(0)
    val quadraticAttenuation = new DoubleRef(0)
    
    private[CustomRenderer] val ecPosition = Vec3(0)
    
    init(classOf[PointLight])
  }
  
  
  // Declare Lighting Environment.
  sealed abstract class ReadLighting extends ReadStruct[Lighting] {
    def lights: ReadBindingList[PointLight]
  }
  
  final class Lighting(implicit listener: StructuralChangeListener)
  extends ReadLighting with UpdatableEnvironmentalEffect[Lighting]
  {
    type Read = ReadLighting
    protected def mkMutable() = new Lighting
    
    val lights = new BindingList[PointLight]
  
    def :=(r: ReadStruct[Lighting]) {
      val e = r.asInstanceOf[Lighting]
      
      if (lights.size != e.lights.size) signalBindingChanges()
      lights := e.lights
    }
    
    def propagate(parentVal: ReadLighting, result: Lighting) {
      val oldSize = result.lights.size
      result.lights.clear()
      result.lights ++= lights
      result.lights ++= parentVal.lights
      
      if (result.lights.size > maxLightCount) result.lights.take(maxLightCount)
      if (result.lights.size != oldSize) result.signalBindingChanges()
    }
    
    protected def resolveBinding() = lights
    def updateBinding(predefinedUniforms: ReadPredefinedUniforms) {
      val s = lights.size; var i = 0; while (i < s) {
        lights(i).ecPosition := predefinedUniforms.se_viewMatrix.transformPoint(lights(i).position)
        
        i += 1
      }
    }
  }
  

  // Replace default material, environment, and geometry.
  class Material extends prototype.Material {
    val textures = Optional[BindingList[TextureUnit]](new BindingList)
    
    init(classOf[Material])
  }
  
  class Environment extends prototype.Environment {
    val lighting = Optional[Lighting](new Lighting)
    
    init(classOf[Environment])
  }
  
  class Geometry extends prototype.Geometry {
    val texCoords = SharedAttributes[Vec2, RFloat]
    
    init(classOf[Geometry])
  }
  
  
  // Replace default contexts. This allows to inject custom material and geometry implementations.
  type Transformation = ComponentTransformation3dContext
  implicit val TransformationContext = new Transformation
  
  class GraphicsContext extends graphics.GraphicsContext {
    type Geometry = CustomRenderer.Geometry
    type Material = CustomRenderer.Material
    type Environment = CustomRenderer.Environment
    
    def mkGeometry() = new Geometry
    def mkMaterial() = new Material
    def mkEnvironment() = new Environment
    
    init()
  }
  type Graphics = GraphicsContext
  implicit val GraphicsContext = new GraphicsContext
  
  
  // Initialize the Technique Manager and the Scenegraph.
  val techniqueManager = new pluggable.TechniqueManager
  
  protected val world = new SceneGraph(
    "World",
    sceneGraphSettings,
    new Camera("World Camera"),
    techniqueManager
  )
  
  
  // Rebuild our techniques from scratch.
  techniqueManager.register(new FragmentShader {
    use("vec4 texturingColor()")
    use("vec4 lightingColor()")
    
    src {"""
      void resolveColor() {
        gl_FragColor = texturingColor() * lightingColor();
      }
    """}
    
    entryPoint("resolveColor")
  })
  
  techniqueManager.register(new FragmentShader {
    uniform {
      declare[BindingList[TextureUnit]]("textures")
    }
    
    in("rasterisation") {
      declare[Vec4]("gl_FragCoord")
    }
    in("texturing") {
      declare[BindingList[Vec2]]("ecTexCoords").size("se_sizeOf_textures")
    }
    
    src {"""
      vec4 texturingColor() {
        vec4 color = vec4(1);
        for (int i = 0; i < se_sizeOf_textures; i++) {
          color *= texture2D(textures[i].texture, texturing.ecTexCoords[i]);
        }
        //return color;
        return vec4(1);//XXX
      }
    """}
    
    export("vec4 texturingColor()")
  })
  
  techniqueManager.register(new FragmentShader {
    src {"""
      vec4 lightingColor() {
        return vec4(1);
      }
    """}
    
    export("vec4 lightingColor()")
  })
  
  techniqueManager.register(new FragmentShader {
    uniform {
      declare[BindingList[PointLight]]("lighting")
    }
    
    in("lightingCtx") {
      declare[Vec3]("normal")
      declare[Vec3]("ecPosition")
    }
    
    src {"""
      vec4 lightingColor() {
        vec3 intensity = vec3(0);
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
        //return vec4(intensity + 0.2, 1);
        return vec4(lightingCtx.normal, 1);//XXX
      }
    """}
    
    export("vec4 lightingColor()")
  })
  
  
  techniqueManager.register(new VertexShader {
    uniform {
      declare[Mat4]("se_modelViewProjectionMatrix")
    }
    
    attributes {
      declare[Vec3]("vertices")
    }
    
    out("rasterisation") {
      declare[Vec4]("gl_Position")
    }
    
    src {"""
      void transformVertices() {
        rasterisation.gl_Position = se_modelViewProjectionMatrix*vec4(vertices, 1.0);
      }
    """}
    
    entryPoint("transformVertices")
  })
    
  techniqueManager.register(new VertexShader {
    uniform {
      declare[BindingList[TextureUnit]]("textures")
    }
    
    attributes {
      declare[Vec2]("texCoords")
    }
    
    out("texturing") {
      declare[BindingList[Vec2]]("ecTexCoords").size("se_sizeOf_textures")
    }
    
    src {"""
      void computeEcTexCoords() {
        for (int i = 0; i < se_sizeOf_textures; i++) {
          texturing.ecTexCoords[i] = texCoords*textures[i].scale;
        }
      }
    """}
    
    entryPoint("computeEcTexCoords")
  })
  
  techniqueManager.register(new VertexShader {
    useSquareMatrices
    
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
    
    src {"""
      void computeLighting() {
        lightingCtx.ecPosition = (se_modelViewMatrix*vec4(vertices, 1)).xyz;
        lightingCtx.normal = normalize(se_normalMatrix*normals);
      }
    """}
    
    entryPoint("computeLighting")
  })
  
  
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
    world.camera.transformation.mutable.translation := Vec3(-10, 25, 100)
    world.camera.transformation.mutable.lookAt(Vec3.Zero, Vec3.UnitY)
    
    // Init camera controls.
    val camControls = new FirstPersonHandler(world.camera.transformation)
    addInputListener(camControls)
    addInputListener(new MouseGrabber(true)(KeyCode.Num_Enter, KeyCode.K_Enter)(camControls)())
    
    // Init the mesh.
    val mesh = new Mesh("Cube")
    
    // Generate box and attach attributes to the mesh.
    val (indices, vertices, normals, texCoords) = Shapes.makeBox()
    mesh.geometry.indices.defineAs(Attributes(indices))
    mesh.geometry.vertices.defineAs(Attributes(vertices))
    mesh.geometry.normals.defineAs(Attributes(normals))
    
    mesh.geometry.texCoords.defineAs(Attributes(texCoords))
    
    // Generate and attach textures.
    val noise = ClassicalGradientNoise
    
    val tileSize = 256
    val tiledNoise = new TiledNoiseSum(
      source = new ClassicalGradientNoise(System.currentTimeMillis.toInt),
      tile = ConstVec4(tileSize),
      frequency = 0.2,
      octaves = 1
    )
    
    val objectTexture = Texture2d[Vec3](Vec2i(128))
    objectTexture.fillWith { p =>
      val intensity = (noise(p.x*0.06, p.y*0.06) + 1)*0.5
      Vec3(intensity, intensity, intensity) + 0.2
    }
    
    val detailTexture = Texture2d[Vec3](Vec2i(tileSize))
    detailTexture.fillWith { p =>
      val intensity = abs(tiledNoise(p.x, p.y) + 0.3) + 0.3
      Vec3(0, intensity, intensity)
    }
    
    mesh.material.textures.mutable += new TextureUnit(objectTexture, 1)
    mesh.material.textures.mutable += new TextureUnit(detailTexture, 4)

    // Position the mesh.
    mesh.transformation.mutable.rotation := Quat4 rotateX(radians(25)) rotateY(radians(-30))
    mesh.transformation.mutable.scale := 40
    
    // Attach the mesh to the scenegraph.
    world.attach(mesh)
    
    
    // Attach lights.
    world.environment.lighting.mutable.lights += new PointLight(Vec3(2), 0.001, 0)
    //world.environment.lighting.mutable.lights += new PointLight(Vec3(3), 0.001, 0)
    
    // Init and attach light indicators.
    lightMesh.geometry.vertices.defineAs(Attributes(DataBuffer[Vec3, RFloat](maxLightCount)))
    lightMesh.geometry.mode = Points(5)
    
    // Reuse texture rendering for lights.
    lightMesh.geometry.texCoords.defineAs(Attributes(DataBuffer[Vec2, RFloat](maxLightCount)))
    val lightTexture = Texture2d[Vec3](Vec2i(4))
    lightTexture.fillWith { p => Vec3(1) }
    lightMesh.material.textures.mutable += new TextureUnit(lightTexture, 1)
    
    // Set vertex coordinates that will later be used as to position lights.
    lightMesh.elementRange.mutable.count := 2
    lightMesh.geometry.vertices.write(2) = Vec3(0, 40, 0)
    lightMesh.geometry.vertices.write(3) = Vec3(0, -40, 0)
    
    world.attach(lightMesh)
  }
  
  
  // Setup light representation.
  val maxLightCount = 4
  val lightMesh = new Mesh("Lights")
  
  val lightPositions = Array(Vec3(1, 0.5, 0)*40, Vec3(1, -0.5, 0)*60)
  val rotationSpeeds = Array(radians(40), radians(60))
  
  var lightsOn = false
  
  
  def update(time: TimeStamp) {
    
    val lights = world.environment.lighting.mutable.lights
    val lightVertices = lightMesh.geometry.vertices.write
    val movingLights = min(lightPositions.size, lights.size)
    
//    if (mod(time.total, 10) > 5) {
//      if (!lightsOn) {
//        lightsOn = true
//        println("Extra lights on.")
//        
//        for (i <- movingLights until maxLightCount) {
//          val light = new PointLight(Vec3(2), 0.001, 0)
//          light.position := lightVertices(i)
//          lights += light
//        }
//        
//        lightMesh.elementRange.mutable.count := maxLightCount
//      }
//    }
//    else {
//      if (lightsOn) {
//        lightsOn = false
//        println("Extra lights off.")
//        
//        lights.take(lightPositions.size)
//        lightMesh.elementRange.mutable.count := lightPositions.size
//      }
//    }
    
    for (i <- 0 until movingLights) {
      val transformation = Mat4x3.rotateY(time.total*rotationSpeeds(i))
      val position = transformation.transformPoint(lightPositions(i))
      lights(i).position := position
      lightVertices(i) = position
    }
  }
}
