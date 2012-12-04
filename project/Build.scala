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
      libraryDependencies ++= Seq(
        "org.specs2" %% "specs2" % "1.12.3" % "test",
        "org.mockito" % "mockito-core" % "1.9.5" % "test",
        "junit" % "junit" % "4.11" % "test"
      )
    ): _*
  )

}

