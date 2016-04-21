name := "cats-stuff"

version := "0.0.1"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "com.github.mpilquist" %% "simulacrum" % "0.7.0",

  "org.spire-math" %% "algebra" % "0.3.1",
  "org.typelevel" %% "cats" % "0.4.1",

  "org.scalatest" %% "scalatest" % "3.0.0-M16-SNAP3" % Test
)

addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.6.3")