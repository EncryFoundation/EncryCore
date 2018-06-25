import sbt.Keys._
import sbt._

name := "EncryCore"

version := "0.3.1"

organization := "org.encryfoundation"

scalaVersion := "2.12.6"

resolvers ++= Seq("Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "SonaType" at "https://oss.sonatype.org/content/groups/public",
  "Typesafe maven releases" at "http://repo.typesafe.com/typesafe/maven-releases/",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/")

val akkaVersion = "10.0.9"
val circeVersion = "0.9.3"

val networkDependencies = Seq(
  "org.bitlet" % "weupnp" % "0.1.+",
  "commons-net" % "commons-net" % "3.+"
)

val apiDependencies = Seq(
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "io.swagger" %% "swagger-scala-module" % "1.0.3",
  "com.github.swagger-akka-http" %% "swagger-akka-http" % "0.10.0",
  "com.typesafe.akka" %% "akka-http" % akkaVersion
)

val loggingDependencies = Seq(
  "com.typesafe.scala-logging" %% "scala-logging" % "3.+",
  "ch.qos.logback" % "logback-classic" % "1.+",
  "ch.qos.logback" % "logback-core" % "1.+"
)

val testingDependencies = Seq(
  "com.typesafe.akka" %% "akka-testkit" % "2.4.+" % "test",
  "com.typesafe.akka" %% "akka-http-testkit" % akkaVersion % "test",
  "org.scalatest" %% "scalatest" % "3.0.3" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.+" % "test"
)

libraryDependencies ++= Seq(
  "javax.xml.bind" % "jaxb-api" % "2.+",
  "com.lihaoyi" %% "fastparse" % "1.0.0",
  "com.iheart" %% "ficus" % "1.4.2",
  "com.google.guava" % "guava" % "21.+",
  "org.slf4j" % "slf4j-api" % "1.7.+",
  "org.bouncycastle" % "bcprov-jdk15on" % "1.58",
  "org.whispersystems" % "curve25519-java" % "+",
  "org.rudogma" %% "supertagged" % "1.+",
  "org.scorexfoundation" %% "scrypto" % "2.1.1",
  "org.scorexfoundation" %% "iodb" % "0.3.2",
  "io.spray" %%  "spray-json" % "1.3.3",
  "io.monix" %% "monix" % "2.3.3",
  "com.github.oskin1" %% "prism" % "0.2.1",
  "de.heikoseeberger" %% "akka-http-circe" % "1.20.1",
  "org.influxdb" % "influxdb-java" % "2.10",
  "org.apache.commons" % "commons-io" % "1.3.2"
) ++ networkDependencies ++ apiDependencies ++ loggingDependencies ++ testingDependencies

fork := true

fork in run := true

outputStrategy := Some(StdoutOutput)

connectInput in run := true

evictionWarningOptions in update := EvictionWarningOptions.default
  .withWarnTransitiveEvictions(false)
  .withWarnDirectEvictions(false)
  .withWarnScalaVersionEviction(false)

logLevel := Level.Error

val opts = Seq(
  "-server",
  "-Xms4G",
  "-Xmx4G",
  "-XX:+ExitOnOutOfMemoryError",
  "-XX:+IgnoreUnrecognizedVMOptions",
  "--add-modules=java.xml.bind",

  // from https://groups.google.com/d/msg/akka-user/9s4Yl7aEz3E/zfxmdc0cGQAJ
  "-XX:+UseG1GC",
  "-XX:+UseNUMA",
  "-XX:+AlwaysPreTouch",

  // probably can't use these with jstack and others tools
  "-XX:+PerfDisableSharedMem",
  "-XX:+ParallelRefProcEnabled",
  "-XX:+UseStringDeduplication")

// todo after adding sbt-native-packager
//javaOptions in Universal ++= opts.map(opt => "-J" + opt)

// -J prefix is required by the bash script
javaOptions in run ++= opts

mainClass in assembly := Some("encry.EncryApp")

test in assembly := {}

assemblyMergeStrategy in assembly := {
  case "logback.xml" => MergeStrategy.first
  case "module-info.class" => MergeStrategy.discard
  case "META-INF/MANIFEST.MF" => MergeStrategy.discard
  case "META-INF/*.DSA" => MergeStrategy.discard
  case "META-INF/*.RSA" => MergeStrategy.discard
  case "META-INF/*.SF" => MergeStrategy.discard
  case PathList("reference.conf") => MergeStrategy.concat
  case _ => MergeStrategy.first
}

sourceGenerators in Compile += Def.task {
  val versionFile = (sourceManaged in Compile).value / "encry" / "Version.scala"
  val versionExtractor = """(\d+)\.(\d+)\.(\d+).*""".r
  val versionExtractor(major, minor, bugfix) = version.value
  IO.write(versionFile,
    s"""package encry
       |
       |object Version {
       |  val VersionString = "${version.value}"
       |  val VersionTuple = ($major, $minor, $bugfix)
       |}
       |""".stripMargin)
  Seq(versionFile)
}