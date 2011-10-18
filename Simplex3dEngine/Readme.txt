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


EXAMPLE

To run an example (on GNU/Linux 32-bit from Simplex3dEngine directory):
java -cp $SCALA_HOME/lib/scala-library.jar:lib/lwjgl/lwjgl.jar:lib/lwjgl/lwjgl_util.jar:release/jars/simplex3d-engine-all.jar:dist/Simplex3dEngine.jar -Djava.library.path="lib/lwjgl/native/linux64" test.InstancingTest


KNOWN ISSUES

There is a bug with lwjgl detection of 64 bit linux. Set your native library path to linux64 bit folder. You may have to append 64 to the name of the libs to make lwjgl work: libjinput-linux64.so, liblwjgl64.so, libopenal64.so.
