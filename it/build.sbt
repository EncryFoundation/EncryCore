enablePlugins(DockerPlugin)

inTask(docker)(
  Seq(
    dockerfile := {
      val configTemplate = (Compile / resourceDirectory).value / "template.conf"
      val startEncry     = sourceDirectory.value / "container" / "startNode.sh"

      new Dockerfile {
        from("anapsix/alpine-java:8_server-jre")
        runRaw("mkdir -p /opt/encry")
        add((assembly in LocalProject("encry")).value, "/opt/encry/EncryCore.jar")
        add(Seq(configTemplate, startEncry), "/opt/encry/")
        toRepositoryName("org.encryfoundation")
        runShell("chmod", "+x", "/opt/encry/startNode.sh")
        entryPoint("/opt/encry/startNode.sh")
        expose(10001)
      }
    },
    buildOptions := BuildOptions(removeIntermediateContainers = BuildOptions.Remove.OnSuccess)
  )
)
inConfig(Test)(Seq(
  parallelExecution := false,
  test := (test dependsOn docker).value,
))

val opts = Seq(
  "-J-server",
  "-J-Xms128M",
  "-J-Xmx4G",
  "-XX:+ExitOnOutOfMemoryError",
  "-XX:+IgnoreUnrecognizedVMOptions",
  "--add-modules=java.xml.bind",
  "-XX:+UseG1GC",
  "-XX:+UseNUMA",
  "-XX:+AlwaysPreTouch",
  "-XX:+PerfDisableSharedMem",
  "-XX:+ParallelRefProcEnabled",
  "-XX:+UseStringDeduplication",
  "-XX:MaxMetaspaceSize=512m")

javaOptions in run ++= opts

javaOptions in run ++= Seq(
  "-XX:+IgnoreUnrecognizedVMOptions",
  "--add-modules=java.xml.bind"
)

scalacOptions ++= Seq("-J-Xss8m")