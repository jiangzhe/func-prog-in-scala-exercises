lazy val root = project.enablePlugins(ScalaJSPlugin)

name := "func-prog-in-scala-exercises"

version := "1.0"

scalaVersion := "2.11.8"

resolvers += Resolver.mavenLocal

resolvers += Resolver.sonatypeRepo("snapshots")

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies += "com.typesafe.akka" % "akka-actor_2.11" % "2.3.13"

libraryDependencies += "org.scala-js" % "scalajs-library_2.11" % "0.6.12"

libraryDependencies += "com.thoughtworks.binding" % "dom_sjs0.6_2.11" % "9.0.2"

//libraryDependencies += "org.typelevel" %% "cats" % "0.7.2"

addCompilerPlugin("org.scalamacros" % "paradise_2.11.8" % "2.1.0")

