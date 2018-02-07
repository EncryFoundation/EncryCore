import sbt.Keys._
import sbt._

name := "Encry"

version := "0.1.9"

scalaVersion := "2.12.4"

resolvers ++= Seq("Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "SonaType" at "https://oss.sonatype.org/content/groups/public",
  "Typesafe maven releases" at "http://repo.typesafe.com/typesafe/maven-releases/",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/")

val scorexVersion = "e56b893b-SNAPSHOT"

val circeVersion = "0.8.0"

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
  "com.typesafe.akka" %% "akka-http" % "10.0.9"
)

val loggingDependencies = Seq(
  "ch.qos.logback" % "logback-classic" % "1.+",
  "ch.qos.logback" % "logback-core" % "1.+"
)

val testingDependencies = Seq(
  "com.typesafe.akka" %% "akka-testkit" % "2.4.+" % "test",
  "com.typesafe.akka" %% "akka-http-testkit" % "10.0.9" % "test",
  "org.scalactic" %% "scalactic" % "3.0.3" % "test",
  "org.scalatest" %% "scalatest" % "3.0.3" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.+" % "test",
  "org.scorexfoundation" %% "scorex-testkit" % scorexVersion % "test",
  "net.databinder.dispatch" %% "dispatch-core" % "+" % "test"
)

libraryDependencies ++= Seq(
  "com.iheart" %% "ficus" % "1.4.2",
  "com.google.guava" % "guava" % "19.+",
  "org.slf4j" % "slf4j-api" % "1.7.+",
  "org.bouncycastle" % "bcprov-jdk15on" % "1.58",
  "org.whispersystems" % "curve25519-java" % "+",
  "org.rudogma" %% "supertagged" % "1.+",
  "org.scorexfoundation" %% "scorex-core" % scorexVersion,
  "org.scorexfoundation" %% "iodb" % "0.3.2",
  "org.scorexfoundation" %% "avl-iodb" % "0.2.11",
  "org.scorexfoundation" %% "scrypto" % "2.0.3",
  "com.storm-enroute" %% "scalameter" % "0.8.+",
  "io.spray" %%  "spray-json" % "1.3.3",
  "io.monix" %% "monix" % "2.3.3"
) ++ networkDependencies ++ apiDependencies ++ loggingDependencies ++ testingDependencies

val opts = Seq(
  "-server",
  // JVM memory tuning for 2g ram
  "-Xms128m",
  "-Xmx2G",
  "-XX:+ExitOnOutOfMemoryError",
  // Java 9 support
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
