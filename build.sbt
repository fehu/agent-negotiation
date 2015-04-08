organization  := "feh.tec"

name := "agents-comm"

version := "0.2-SNAPSHOT"

scalaVersion  := "2.11.5"

resolvers += "Fehu's github repo" at "http://fehu.github.io/repo"

libraryDependencies += "feh.util" %% "util" % "1.0.8"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.9"

initialCommands += "import feh.tec.agents.comm._"