Project documentation is available at:
http://code.google.com/p/simplex3d/wiki/


Runtime requirements:
  - Scala 2.8.0.RC3 or higher.
  - Simplex3dMath of matching version.

Build requirements:
  - Ant 1.7 or higher.
  - SCALA_HOME set to Scala 2.8.0.RC3 or higher.
  - Simplex3dMath (built with "ant jar") in the same folder as this project folder

To build the project:
  1) open a console,
  2) cd into the project directory,
  3) type: ant release-bin

To run all test:
Put simplex3d-math-core.jar, simplex3d-math-intm.jar, simplex3d-math-floatm.jar,
simplex3d-math-doublem.jar, scalatest.jar, and simplex3d-math-test.jar
on the classpath. Execute the main class "test.Launcher".
