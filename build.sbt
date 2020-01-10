import sbt.Def
import sbt.Keys.version

val settings: Seq[Def.Setting[String]] = Seq(
  name := "EncryCore",
  version := "0.9.3",
  organization := "org.encryfoundation",
  scalaVersion := "2.12.6"
)

val encry = (project in file(".")).settings(settings: _*)

val akkaVersion = "2.5.23"
val akkaHttpVersion = "10.1.10"
val doobieVersion = "0.5.2"
val logbackVersion = "1.2.3"
val kamonVersion = "1.1.0"

val databaseDependencies = Seq(
  "org.tpolecat" %% "doobie-core" % doobieVersion,
  "org.tpolecat" %% "doobie-postgres" % doobieVersion,
  "org.tpolecat" %% "doobie-specs2" % doobieVersion,
  "org.tpolecat" %% "doobie-hikari" % doobieVersion
)

val apiDependencies = Seq(
  "io.swagger" %% "swagger-scala-module" % "1.0.3",
  "com.github.swagger-akka-http" %% "swagger-akka-http" % "0.10.0",
  "com.typesafe.akka" %% "akka-http" % akkaHttpVersion
)

val loggingDependencies = Seq(
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0",
  "ch.qos.logback" % "logback-classic" % logbackVersion,
  "ch.qos.logback" % "logback-core" % logbackVersion,
  "net.logstash.logback" % "logstash-logback-encoder" % "1.0",
  "com.typesafe.akka" %% "akka-slf4j" % akkaVersion
)

val testingDependencies = Seq(
  "com.typesafe.akka" %% "akka-testkit" % "2.4.+" % Test,
  "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpVersion % Test,
  "org.scalacheck" %% "scalacheck" % "1.13.+" % Test,
  "org.mockito" % "mockito-core" % "2.19.1" % Test,
  "org.openjdk.jmh" % "jmh-generator-annprocess" % "1.21" % Test
)

lazy val monitoringDependencies = Seq(
  "io.kamon" %% "kamon-core" % kamonVersion,
  "io.kamon" %% "kamon-akka-2.5" % kamonVersion,
  "io.kamon" %% "kamon-system-metrics" % "1.0.0",
  "io.kamon" %% "kamon-influxdb" % "1.0.1"
)

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-stream" % akkaVersion,
  "com.typesafe.akka" %% "akka-persistence" % akkaVersion,
  "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpVersion,
  "org.fusesource.leveldbjni" % "leveldbjni-all" % "1.8",
  "org.iq80.leveldb" % "leveldb" % "0.9",
  "javax.xml.bind" % "jaxb-api" % "2.3.0",
  "com.iheart" %% "ficus" % "1.4.2",
  "com.typesafe" % "config" % "1.3.3",
//  "org.bouncycastle" % "bcprov-jdk15on" % "1.60" % Provided,
  "org.whispersystems" % "curve25519-java" % "0.5.0",
  "org.rudogma" %% "supertagged" % "1.4",
  "org.scorexfoundation" %% "iodb" % "0.3.2",
  "io.spray" %% "spray-json" % "1.3.3",
  "com.lihaoyi" %% "scalatags" % "0.7.0",
  // JWT
  "com.pauldijou" %% "jwt-spray-json" % "4.2.0",
  "org.bouncycastle" % "bcpkix-jdk15on" % "1.60",
  "org.encry" %% "encry-common" % "0.9.3",
  "org.scalatest" %% "scalatest" % "3.0.5",
  "org.scalactic" %% "scalactic" % "3.0.5",
  "de.heikoseeberger" %% "akka-http-circe" % "1.20.1",
  "org.influxdb" % "influxdb-java" % "2.10",
  "org.apache.commons" % "commons-io" % "1.3.2",
  "org.apache.kafka" % "kafka-clients" % "2.0.0",
  "commons-net" % "commons-net" % "3.6",
  "com.spotify" % "docker-client" % "8.11.0" % "test" classifier "shaded",
  "com.fasterxml.jackson.dataformat" % "jackson-dataformat-properties" % "2.9.6",
  "com.fasterxml.jackson.core" % "jackson-databind" % "2.9.6",
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.9.6",
  "org.asynchttpclient" % "async-http-client" % "2.4.7",
  "com.spotify" % "docker-client" % "8.11.3",
  "io.monix" %% "monix" % "3.0.0-RC1",
  "com.sun.jersey" % "jersey-client" % "1.19",
  "commons-net" % "commons-net" % "3.6",
  "org.aspectj" % "aspectjweaver" % "1.9.2",
  "org.typelevel" % "cats-core_2.12" % "2.0.0",
  "org.typelevel" % "cats-kernel_2.12" % "2.0.0",
  "org.typelevel" % "cats-macros_2.12" % "2.0.0",
  "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf",
  "com.google.guava" % "guava" % "27.1-jre"
) ++ databaseDependencies ++ apiDependencies ++ loggingDependencies ++ testingDependencies ++ monitoringDependencies

resolvers ++= Seq("Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "SonaType" at "https://oss.sonatype.org/content/groups/public",
  "Typesafe maven releases" at "http://repo.typesafe.com/typesafe/maven-releases/",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/")

evictionWarningOptions in update := EvictionWarningOptions.default
  .withWarnTransitiveEvictions(false)
  .withWarnDirectEvictions(false)
  .withWarnScalaVersionEviction(false)

logLevel := Level.Info
val opts = Seq(
  "-server",
  "-Xms3G",
  "-Xmx6G",
  "-XX:+ExitOnOutOfMemoryError",
  "-XX:+IgnoreUnrecognizedVMOptions",
  "--add-modules=java.xml.bind",
  "-XX:+UseConcMarkSweepGC",
  "-XX:+UseNUMA",
  "-XX:+AlwaysPreTouch",
  "-XX:+PerfDisableSharedMem",
  "-XX:+ParallelRefProcEnabled",
  "-XX:+UseStringDeduplication",
  "-XX:MaxMetaspaceSize=512m")

javaOptions in run ++= opts

fork := true

outputStrategy := Some(StdoutOutput)

connectInput in run := true

assemblyJarName in assembly := "Encry.jar"

mainClass in assembly := Some("encry.EncryApp")

scalacOptions += "-Ypartial-unification"

test in assembly := {}

assemblyMergeStrategy in assembly := {
  case "logback.xml" => MergeStrategy.first
  case "module-info.class" => MergeStrategy.discard
  case "META-INF/MANIFEST.MF" => MergeStrategy.discard
  case "META-INF/BC1024KE.SF" => MergeStrategy.discard
  case "META-INF/BC2048KE.SF" => MergeStrategy.discard
  case PathList("reference.conf") => MergeStrategy.concat
  case _ => MergeStrategy.first
}

unmanagedResourceDirectories in Compile += { baseDirectory.value / "src/main/resources" }

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

lazy val it = project.dependsOn(encry)
lazy val benchmarks = (project in file("benchmarks"))
  .dependsOn(encry % "compile->compile;test->test")
  .enablePlugins(JmhPlugin)

PB.targets in Compile := Seq(
  scalapb.gen() -> (sourceManaged in Compile).value / "protobuf"
)