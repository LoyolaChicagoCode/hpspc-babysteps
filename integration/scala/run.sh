mkdir build
scalac -d build integration.scala
scala -classpath build integration
