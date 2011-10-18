Project documentation is available at:
http://www.simplex3d.org/project/documentation/


Runtime requirements:
  - Scala 2.9.1.final or higher.
  - Simplex3dMath.
  - Simplex3dData.
  - Simplex3dAlgorithm.

Build requirements:
  - Ant 1.7.1 or higher.
  - SCALA_HOME set to Scala 2.9.0.final or higher.
  - Simplex3dMath (built with ant) in the same folder as this project folder.
  - Simplex3dData (built with ant) in the same folder as this project folder.
  - Simplex3dAlgorithm (built with ant) in the same folder as this project folder.
  
See SVN trunk for correct directory layout: http://code.google.com/p/simplex3d/source/browse/#svn%2Ftrunk


To build the project:
  1) open a console,
  2) cd into the project directory,
  3) type: ant


simplex3d-engine-all.jar includes all the necessary simplex3d dependencies.
LWJGL and native libs can be found in the lib directory.
