name := "transient-core-wave"

inThisBuild(
  List(
    organization := "com.carvana",
    scalaVersion := "2.13.6",
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Ymacro-annotations")
  )
)

sources in (Compile, doc) := Seq.empty

publishArtifact in (Compile, packageDoc) := false

Test / run / fork := true
Compile / run / fork := true

lazy val root = (project in file("."))
  .configs(IntegrationTest)
  .settings(
    Defaults.itSettings
  )

testOptions in IntegrationTest += Tests.Argument(TestFrameworks.ScalaTest, "-n", "ai.propel.core.testkit.tags.CiIntegrationTest")
coverageExcludedPackages := ".*wiring.*;.*startup.*"

enablePlugins(JavaAppPackaging)

dockerRepository := Some("gcr.io")
dockerUsername := Some("nosidelines")
dockerBaseImage := " adoptopenjdk:14.0.2_8-jre-openj9-0.21.0"
Docker / packageName := "transient-core-wave"
executableScriptName := "server"

addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)

enablePlugins(GatlingPlugin)

libraryDependencies ++= {
  val pureConfigVersion = "0.13.0"
  val carvanaZioVersion = "0.4.4"
  val bcryptVersion = "0.9.0"
  val tapirVersion = "0.19.0-M8"
  val zioVersion = "1.0.11"
  val sttpVersion = "3.3.7"
  val zioZmxVersion = "0.0.9"
  val diffsonVersion = "4.1.1"
  val embeddedKafkaVersion = "2.8.0"
  val gatlingVersion = "3.6.1"

  Seq(
    "com.carvana"                   %% "zio-core"                 % carvanaZioVersion,
    "com.carvana"                   %% "zio-core-db"              % carvanaZioVersion,
    "com.carvana"                   %% "zio-core-kafka"           % carvanaZioVersion,
    "dev.zio"                       %% "zio-zmx"                  % zioZmxVersion,
    "com.softwaremill.magnolia1_2"  %% "magnolia"                 % "1.0.0-M7",
    "io.github.embeddedkafka"       %% "embedded-kafka"           % embeddedKafkaVersion,
    "at.favre.lib"                  % "bcrypt"                    % bcryptVersion,
    "com.github.pureconfig"         %% "pureconfig"               % pureConfigVersion,
    "io.scalaland"                  %% "chimney"                  % "0.6.1",
    "org.gnieh"                     %% "diffson-core"             % diffsonVersion,
    "com.softwaremill.sttp.tapir"   %% "tapir-core"               % tapirVersion,
    "com.softwaremill.sttp.tapir"   %% "tapir-json-zio"           % tapirVersion,
    "com.softwaremill.sttp.tapir"   %% "tapir-openapi-docs"       % tapirVersion,
    "com.softwaremill.sttp.tapir"   %% "tapir-openapi-circe-yaml" % tapirVersion,
    "com.softwaremill.sttp.tapir"   %% "tapir-swagger-ui"         % tapirVersion,
    "com.softwaremill.sttp.tapir"   %% "tapir-redoc"              % tapirVersion,
    "com.softwaremill.sttp.tapir"   %% "tapir-zio"                % tapirVersion,
    "com.softwaremill.sttp.tapir"   %% "tapir-zio-http"           % tapirVersion,
    "com.softwaremill.sttp.client3" %% "zio-json"                 % sttpVersion,
    "com.softwaremill.sttp.tapir"   %% "tapir-sttp-client"        % tapirVersion % "test,it",
    "com.softwaremill.sttp.client3" %% "httpclient-backend-zio"   % sttpVersion % "test,it",
    "dev.zio"                       %% "zio-test-sbt"             % zioVersion % "test,it",
    "io.gatling.highcharts"         % "gatling-charts-highcharts" % gatlingVersion % "test,it",
    "io.gatling"                    % "gatling-test-framework"    % gatlingVersion % "test,it",
    "org.gnieh"                     %% "diffson-testkit"          % diffsonVersion % "test,it"
  )
}
