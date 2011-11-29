Project documentation is available at:
http://www.simplex3d.org/project/documentation/


Simplex3d Project is using a single SBT build script to manage all of its' sub-projects.



***** SBT sub-project layout *****

root

  math
    math-core
    math-double
    
  data
    data-core
    data-double
    data-format
    
  algorithm
    algorithm-intersection
    algorithm-mesh
    algorithm-noise
    
  engine
    engine-core
    engine-scenegraph
    engine-renderer
    engine-backend-opengl
    engine-backend-lwjgl
    engine-default
    
  script
  
  console
    console-webstart
  
  
Additionally there are aggregated doc and test projects:

root-doc
  math-doc
  data-doc
  algorithm-doc
  engine-doc
  
root-example
  math-example
  data-example
  algorithm-example
  engine-example
  script-example


Also there are some test projects:

  math-test
  data-test
  engine-test


And two optional sub-projects:

  math-float
  data-float

  
  
***** Using SBT *****
  
You can use SBT to compile these projects and run tests.

Assuming you have checked out "trunk" from the SVN and renamed it to "Simplex3d":
  cd Simplex3d
  sbt ";project engine-example; run-main simplex3d.example.engine.DynamicTexture"


  
***** Setting up projects in Netbeans *****

Simply open desired project in Netbeans.
You can use clean, build, and run commands from the IDE.



***** Setting up Simplex3dEngine project in Eclipse *****

Before you start, install ScalaIDE and IvyDE plugins for Eclipse.
This guide assumes you have checked out "trunk" from the SVN and renamed it to "Simplex3d".

1) Publish engine dependencies into local repository:
     sbt ";project root;publish-local"
2) Make a new Scala project in Eclipse.
3) Add source directories to Eclipse.
4) Setup IvyDE using provided ivy.xml and ivysettings.xml.
5) Run any of the Simplex3dEngine examples to setup native libs:
     sbt ";project engine-example; run-main simplex3d.example.engine.DynamicTexture"
6) Set "Native library location" for lwjgl.jar to "Simplex3d/target/engine/natives"

After completing these steps you should be able to compile Simplex3dEngine and run any of the tests and examples from Eclipse.
