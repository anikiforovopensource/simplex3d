Project documentation is available at:
http://www.simplex3d.org/project/documentation/


Runtime requirements:
  - Scala 2.9.0.final or higher.

Build requirements:
  - Ant 1.7.1 or higher.
  - SCALA_HOME set to Scala 2.9.0.final or higher.


To build the project:
  1) open a console,
  2) cd into the project directory,
  3) type: ant


To run:
Put
  scala-library.jar, scala-compiler.jar,
  find-replace-panel.jar, rsyntaxtextarea.jar,
  simplex3d-math-core.jar, simplex3d-math-double.jar,
  simplex3d-data-core.jar, simplex3d-data-double.jar,
  and simplex3d-console.jar
on the classpath.
Execute the main class "simplex3d.console.ConsoleFrame".

Example (on GNU/Linux from Simplex3dDataTest directory):
java -cp $SCALA_HOME/lib/scala-library.jar:$SCALA_HOME/lib/scala-compiler.jar:lib/find-replace-panel.jar:lib/rsyntaxtextarea.jar:../Simplex3dMath/release/jars/simplex3d-math-core.jar:../Simplex3dMath/release/jars/simplex3d-math-double.jar:../Simplex3dData/release/jars/simplex3d-data-core.jar:../Simplex3dData/release/jars/simplex3d-data-double.jar:release/jars/simplex3d-console.jar simplex3d.console.ConsoleFrame
