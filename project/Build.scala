import sbt._
import Keys._

object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "org.scalamacros",
    scalaVersion := "2.10.0",
    scalacOptions ++= Seq("-Ywarn-numeric-widen", "-Yno-adapted-args", "-Ywarn-all")
  )
}

object DancingBuild extends Build {
  import BuildSettings._

  /*lazy val root = Project(
    "root",
    file("core"),
    settings = buildSettings
  ) aggregate(macros, core)*/

  lazy val macros = Project(
    "macros",
    file("macros"),
    settings = buildSettings ++ Seq(
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
      libraryDependencies += ("com.chuusai" %% "shapeless" % "1.2.3")
    )
  )

  lazy val core = Project(
    "core",
    file("core"),
    settings = buildSettings
  ) dependsOn(macros)
}
