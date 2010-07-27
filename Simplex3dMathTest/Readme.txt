Project documentation is available at:
http://code.google.com/p/simplex3d/wiki/


Runtime requirements:
  - Scala 2.8.0.final or higher.
  - Simplex3dMath of matching version.

Build requirements:
  - Ant 1.7 or higher.
  - SCALA_HOME set to Scala 2.8.0.final or higher.
  - Simplex3dMath (built with ant) in the same folder as this project folder.

NOTE: you need at least 4.0Gb RAM to build with Scala 2.8.0.final.
To build the project:
  1) open a console,
  2) cd into the project directory,
  3) type: ant release-bin

Alternative GNU/Linux-only build script that works with 2.0Gb RAM (until scalac is fixed):
  1) open a console,
  2) cd into the project directory,
  3) type: ./release.sh

To run all test:
Put simplex3d-math-core.jar, simplex3d-math-intm.jar, simplex3d-math-floatm.jar,
simplex3d-math-doublem.jar, scalatest-1.2.jar, and simplex3d-math-test.jar
on the classpath.
Execute the main class "test.JarLauncher".

Example (on GNU/Linux from Simplex3dMathTest directory):
java -cp $SCALA_HOME/lib/scala-library.jar:lib/scalatest-1.2.jar:../Simplex3dMath/release/jars/simplex3d-math-core.jar:../Simplex3dMath/release/jars/simplex3d-math-int.jar:../Simplex3dMath/release/jars/simplex3d-math-float.jar:../Simplex3dMath/release/jars/simplex3d-math-double.jar:release/jars/simplex3d-math-test.jar test.JarLauncher
