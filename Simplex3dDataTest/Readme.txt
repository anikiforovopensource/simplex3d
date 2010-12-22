Project documentation is available at:
http://code.google.com/p/simplex3d/wiki/


Runtime requirements:
  - Scala 2.8.0.final or higher.
  - Simplex3dMath of matching version.
  - Simplex3dBuffer of matching version.

Build requirements:
  - Ant 1.7 or higher.
  - SCALA_HOME set to Scala 2.8.0.final or higher.
  - Simplex3dMath (built with ant) in the same folder as this project folder
  - Simplex3dBuffer (built with ant) in the same folder as this project folder

To build the project:
  1) open a console,
  2) cd into the project directory,
  3) type: ant release-bin

To run all test:
Put scalatest-1.2.jar, simplex3d-math-core.jar, simplex3d-math-int.jar,
simplex3d-math-float.jar, simplex3d-math-double.jar,
simplex3d-buffer-core.jar, simplex3d-buffer-int.jar,
simplex3d-buffer-float.jar, simplex3d-buffer-double.jar,
and simplex3d-buffer-test.jar on the classpath.
Execute the main class "test.JarLauncher".

Example (on GNU/Linux from Simplex3dBufferTest directory):
java -cp $SCALA_HOME/lib/scala-library.jar:../Simplex3dMathTest/lib/scalatest-1.2.jar:../Simplex3dMath/release/jars/simplex3d-math-core.jar:../Simplex3dMath/release/jars/simplex3d-math-int.jar:../Simplex3dMath/release/jars/simplex3d-math-float.jar:../Simplex3dMath/release/jars/simplex3d-math-double.jar:../Simplex3dBuffer/release/jars/simplex3d-buffer-core.jar:../Simplex3dBuffer/release/jars/simplex3d-buffer-int.jar:../Simplex3dBuffer/release/jars/simplex3d-buffer-float.jar:../Simplex3dBuffer/release/jars/simplex3d-buffer-double.jar:release/jars/simplex3d-buffer-test.jar test.JarLauncher
