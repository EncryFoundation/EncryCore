name := "EncryCore"
version := "0.9.0"
organization := "org.encryfoundation"
scalaVersion := "2.12.6"

val akkaVersion = "2.5.13"
val akkaHttpVersion = "10.0.9"
val doobieVersion = "0.5.2"
val logbackVersion = "1.2.3"

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
  "ch.qos.logback" % "logback-core" % logbackVersion
)

val testingDependencies = Seq(
  "com.typesafe.akka" %% "akka-testkit" % "2.4.+" % Test,
  "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpVersion % Test,
  "org.scalatest" %% "scalatest" % "3.0.3" % Test,
  "org.scalacheck" %% "scalacheck" % "1.13.+" % Test,
  "org.mockito" % "mockito-core" % "2.19.1" % Test
)

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-stream" % akkaVersion,
  "com.typesafe.akka" %% "akka-persistence" % akkaVersion,
  "org.fusesource.leveldbjni" % "leveldbjni-all" % "1.8",
  "org.iq80.leveldb" % "leveldb" % "0.7",
  "javax.xml.bind" % "jaxb-api" % "2.3.0",
  "com.iheart" %% "ficus" % "1.4.2",
  "org.slf4j" % "slf4j-api" % "1.7.25",
  "org.bouncycastle" % "bcprov-jdk15on" % "1.58",
  "org.whispersystems" % "curve25519-java" % "0.5.0",
  "org.rudogma" %% "supertagged" % "1.4",
  "org.scorexfoundation" %% "iodb" % "0.3.2",
  "io.spray" %% "spray-json" % "1.3.3",
  "org.encry" %% "encry-common" % "0.8.3",
  "de.heikoseeberger" %% "akka-http-circe" % "1.20.1",
  "org.influxdb" % "influxdb-java" % "2.10",
  "org.apache.commons" % "commons-io" % "1.3.2",
  "org.apache.kafka" % "kafka-clients" % "2.0.0",
  "commons-net" % "commons-net" % "3.6"
) ++ databaseDependencies ++ apiDependencies ++ loggingDependencies ++ testingDependencies

resolvers ++= Seq("Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "SonaType" at "https://oss.sonatype.org/content/groups/public",
  "Typesafe maven releases" at "http://repo.typesafe.com/typesafe/maven-releases/",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/")

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
  "-XX:+UseG1GC",
  "-XX:+UseNUMA",
  "-XX:+AlwaysPreTouch",
  "-XX:+PerfDisableSharedMem",
  "-XX:+ParallelRefProcEnabled",
  "-XX:+UseStringDeduplication")

javaOptions in run ++= opts

assemblyJarName in assembly := "core.jar"

mainClass in assembly := Some("encry.EncryApp")

test in assembly := { }

assemblyMergeStrategy in assembly := {
  case "logback.xml" => MergeStrategy.first
  case "module-info.class" => MergeStrategy.discard
  case "META-INF/MANIFEST.MF" => MergeStrategy.discard
  case "META-INF/BC1024KE.SF" => MergeStrategy.discard
  case "META-INF/BC2048KE.SF" => MergeStrategy.discard
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