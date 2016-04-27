// build.sbt for projec structures
// git clone https://github.com/mpilquist/Structures.git
// sbt handle build from command line but IntelliJ don't import project properly

val scalaTestVersion = "2.2.5"
val akkaVersion = "2.3.14"

lazy val commonSettings = Seq(
  organization := "org.castspells",
  version := "0.1.0",
  scalaVersion := "2.11.8",
  libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.7",
  libraryDependencies += "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.4",
  libraryDependencies += "org.scalatest" % "scalatest_2.11" % scalaTestVersion % "test"
)


lazy val core = (project in file("core")).
  settings(commonSettings: _*).
  settings(
    // other settings
  )

lazy val discipline = (project in file("discipline")).
  settings(commonSettings: _*).
  settings(
    // other settings
  )


lazy val examples = (project in file("examples")).
  settings(commonSettings: _*).
  settings(
    // other settings
  )


lazy val laws = (project in file("laws")).
  settings(commonSettings: _*).
  settings(
    // other settings
  )
