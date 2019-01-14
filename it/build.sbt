enablePlugins(DockerPlugin)

inTask(docker)(
  Seq(
    dockerfile := {
      val configTemplate = (Compile / resourceDirectory).value / "template.conf"
      val startEncry = sourceDirectory.value / "container" / "startNode.sh"

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
inConfig(Test) {
  val commonFlags = "-XX:+UnlockExperimentalVMOptions -XX:+UseCGroupMemoryLimitForHeap"
  Seq(
    parallelExecution := false,
    envVars in test += "CONTAINER_JAVA_OPTS" -> s"-Xmx1500m $commonFlags",
    envVars in testOnly += "CONTAINER_JAVA_OPTS" -> s"-Xmx512m $commonFlags",
    test :=(test dependsOn docker).value,
  )
}