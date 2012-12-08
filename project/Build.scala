import sbt._
import Keys._
import com.twitter.sbt._

object SumitoTextBuild extends Build {

  lazy val sumitoText = Project(
    id = "sumito-base",
    base = file("."),
    settings = StandardProject.newSettings
  ).settings(
    Seq(
      version := "0.0.1",
      scalaVersion := "2.10.0-RC3",
      scalacOptions ++= Seq(
        "-target:jvm-1.7",
        "-deprecation",
        "-explaintypes",
        "-unchecked"
        ),
      publishMavenStyle := true,
      publishTo := Some(Resolver.sftp(
        "sumito3478 Maven Repository",
        "maven.sumito3478.info",
        46877,
        "/var/www/maven.sumito3478.info") as (
        "sumito3478",
        new java.io.File(
          new java.io.File(System.getProperty("user.home")),
          ".ssh/sumito3478-sshkey"))),
      resolvers := Seq(
        "Maven Repository Mirror" at "http://uk.maven.org/maven2"),
      externalResolvers <<= resolvers map {
        rs =>
          Resolver.withDefaultResolvers(rs, mavenCentral = false)
      },
      libraryDependencies ++= Seq(
        "org.specs2" %% "specs2" % "1.12.3" % "test",
        "org.mockito" % "mockito-core" % "1.9.5" % "test",
        "junit" % "junit" % "4.11" % "test",
        "com.google.guava" % "guava" % "12.0.1"
      )
    ): _*
  )

}

