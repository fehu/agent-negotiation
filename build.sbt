organization  := "feh.tec"

name := "agents-comm"

version := "0.2-SNAPSHOT"

scalaVersion  := "2.11.7"

resolvers += "Fehu's github repo" at "http://fehu.github.io/repo"

libraryDependencies += "feh.util" %% "util" % "1.1.0-SNAPSHOT"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.4.0"

initialCommands += "import feh.tec.agents.comm._"

scalacOptions in (Compile, doc) ++= Seq("-diagrams", "-diagrams-max-classes", "50", "-diagrams-max-implicits", "20")