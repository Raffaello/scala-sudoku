name := "scala-sudoku"

version := "1.0"

scalaVersion := "2.12.4"

logBuffered in Test := false

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
//libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"
