name := "ScalaAG"

version := "0.1-alpha"

// Adding Sonatype snapshots for experimental tools (Obey, TQL) to resolvers.
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

scalaVersion := "2.11.2" //this exact version is needed for Scala virtualized

scalaOrganization := "org.scala-lang.virtualized" //somehow adds the library dependency for scala-virtualized

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.5"

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.11.5"

libraryDependencies += "com.github.begeric" % "tqlscalameta_2.11" % "0.1-SNAPSHOT"

libraryDependencies += "EPFL" % "lms_2.11" % "0.3-SNAPSHOT"

libraryDependencies += "default" % "functadelic_2.11" % "0.1-SNAPSHOT"

scalacOptions ++= Seq(
  "-feature",
  "-Yvirtualize" //needed for virtualization of if-then-else with lms
)
