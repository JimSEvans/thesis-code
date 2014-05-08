name := "thesis-code"

version := "0.1.0"

organization := "edu.utexas"

scalaVersion := "2.10.3"

retrieveManaged := true

crossPaths := false

resolvers ++= Seq(
// Opts.resolver.sonatypeSnapshots (for latest snapshot of scalalogging),
  "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

libraryDependencies ++= Seq(
  // "com.typesafe" %% "scalalogging-slf4j" % "1.0.1",
  // "org.clapper" % "argot_2.9.1" % "0.3.8",
  // "gov.nist.math" % "jama" % "1.0.2",
  "org.scalanlp" % "chalk" % "1.3.0",
  "com.typesafe.play" %% "play-json" % "2.2.1",
  "org.scalanlp" % "nak" % "1.2.0",
  "org.twitter4j" % "twitter4j-core" % "4.0.1",
  "org.twitter4j" % "twitter4j-stream" % "4.0.1"
)