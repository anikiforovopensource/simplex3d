Project documentation is available at:
http://www.simplex3d.org/project/documentation/


Simplex3d Project is using a single SBT build script to manage all of its' sub-projects.



***** SBT sub-project layout *****

  math
    math-core
    math-double
    
  data
    data-core
    data-double
    
  algorithm
    data-format
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
  
  
Additionally there are doc and test sub-project:

  doc-math
  doc-data
  doc-algorithm
  doc-engine
  
  test-math
  test-data
  test-engine
  
  
And two optional sub-projects:

  math-float
  data-float
  

  
***** Using SBT *****
  
You can use SBT to compile these projects and run tests.

Assuming you have checked out "trunk" from the SVN and renamed it to "Simplex3d":
  cd Simplex3d
  sbt ";project test-engine; run-main test.DynamicTexture"


  
***** Setting up projects in Netbeans *****

Simply open desired project in Netbeans.
You can use clean, build, and run commands from the IDE.



***** Setting up Simplex3dEngine project in Eclipse *****

Before you start, install ScalaIDE and IvyDE plugins.
This guide assumes you have checked out "trunk" from the SVN and renamed it to "Simplex3d".

1) Publish engine dependencies into local repository:
     sbt ";project root; publish-local"
2) Make a new Scala project in Eclipse.
3) Set the src directory of the Eclipse project to point to "Simplex3d/Simplex3dEngine/src".
4) Setup IvyDE using provided ivy.xml and ivysettings.xml.
5) Run any of the Simplex3dEngine tests to setup native libs:
     sbt ";project test-engine; run-main test.DynamicTexture"
6) Set "Native library location" for lwjgl.jar to "Simplex3d/target/engine/natives"

After completing these steps you should be able to compile Simplex3dEngine and run any of the tests from Eclipse.



***** Known issues *****

There is a bug in SBT that causes it to run an extra doc task on the root folder
when running the doc task on math-core. This will result in error messages being printed to the
console. However these errors do not affect the build and can be simply ignored.
