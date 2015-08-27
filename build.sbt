organization  := "feh.tec"

name := "agents-comm"

version := "0.2-SNAPSHOT"

scalaVersion  := "2.11.5"

resolvers += "Fehu's github repo" at "http://fehu.github.io/repo"

libraryDependencies += "feh.util" %% "util" % "1.0.9-SNAPSHOT"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.9"

initialCommands += "import feh.tec.agents.comm._"

scalacOptions in (Compile, doc) ++= Seq("-diagrams", "-diagrams-max-classes", "50", "-diagrams-max-implicits", "20")