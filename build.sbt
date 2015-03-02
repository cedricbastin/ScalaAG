name := "ScalaAG"

version := "0.1-alpha"

// Adding Sonatype snapshots for experimental tools (Obey, TQL) to resolvers.
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

scalaVersion := "2.11.5"

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.5"

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.11.5"

libraryDependencies += "com.github.begeric" % "tqlscalameta_2.11" % "0.1-SNAPSHOT"


scalacOptions += "-feature"
