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
        add(Seq(configTemplate, startEncry), "/opt/encry/core")
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