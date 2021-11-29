val scala3Version = "3.0.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Advent-of-code-2021",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += "com.monovore" %% "decline" % "2.1.0",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.3.0",
    libraryDependencies += "com.monovore" %% "decline-effect" % "2.1.0"


)
