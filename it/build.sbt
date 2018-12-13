enablePlugins(DockerPlugin)

inTask(docker)(
  Seq(
    dockerfile := {
      val configTemplate = (Compile / resourceDirectory).value / "application.conf"
      val startEncry     = sourceDirectory.value / "container" / "startDocker.sh"

      new Dockerfile {
        from("anapsix/alpine-java:8_server-jre")
        runRaw("mkdir -p /opt/encry")

        add((assembly in LocalProject("encry")).value, "/opt/encry/encry.jar")
        add(Seq(configTemplate, startEncry), "/opt/encry/")
        runShell("chmod", "+x", "/opt/encry/startDocker.sh")
        entryPoint("/opt/encry/startDocker.sh")
        expose(10001)
      }
    },
    buildOptions := BuildOptions(removeIntermediateContainers = BuildOptions.Remove.OnSuccess)
  ))

Defaults.itSettings
configs(IntegrationTest extend (Test))
inConfig(IntegrationTest)(Seq(
  parallelExecution := false,
  test := (test dependsOn docker).value,
))

buildOptions in docker := BuildOptions(
  removeIntermediateContainers = BuildOptions.Remove.OnSuccess
)