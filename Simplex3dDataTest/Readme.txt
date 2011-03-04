Project documentation is available at:
http://www.simplex3d.org/project/documentation/


Runtime requirements:
  - Scala 2.8.1.final or higher.
  - Simplex3dMath of matching version.
  - Simplex3dData of matching version.

Build requirements:
  - Ant 1.7 or higher.
  - SCALA_HOME set to Scala 2.8.1.final or higher.
  - Simplex3dMath (built with ant) in the same folder as this project folder
  - Simplex3dData (built with ant) in the same folder as this project folder

To build the project:
  1) open a console,
  2) cd into the project directory,
  3) type: ant

To run all test:
Put
  scalatest-1.2.jar,
  simplex3d-math-core.jar, simplex3d-math-float.jar, simplex3d-math-double.jar,
  simplex3d-data-core.jar, simplex3d-data-float.jar, simplex3d-data-double.jar,
  and simplex3d-data-test.jar
on the classpath.
Execute the main class "test.JarLauncher".

Example (on GNU/Linux from Simplex3dDataTest directory):
java -cp $SCALA_HOME/lib/scala-library.jar:../Simplex3dMathTest/lib/scalatest-1.2.jar:../Simplex3dMath/release/jars/simplex3d-math-core.jar:../Simplex3dMath/release/jars/simplex3d-math-float.jar:../Simplex3dMath/release/jars/simplex3d-math-double.jar:../Simplex3dData/release/jars/simplex3d-data-core.jar:../Simplex3dData/release/jars/simplex3d-data-float.jar:../Simplex3dData/release/jars/simplex3d-data-double.jar:release/jars/simplex3d-data-test.jar test.JarLauncher
