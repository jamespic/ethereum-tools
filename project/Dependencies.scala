import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.1"
  lazy val ethereumJ = "org.ethereum" % "ethereumj-core" % "1.6.0-RELEASE"
  lazy val akkaHttp = "com.typesafe.akka" %% "akka-http" % "10.0.10"
  lazy val akkaHttpXml = "com.typesafe.akka" %% "akka-http-xml" % "10.0.10"
  lazy val akkaHttpTestKit = "com.typesafe.akka" %% "akka-http-testkit" % "10.0.10"
  lazy val web3j = "org.web3j" % "core" % "3.0.1"
}
