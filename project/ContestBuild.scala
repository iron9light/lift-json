import sbt._

object ContestBuild extends Build {
  lazy val liftJson = Project(id = "lift-json", base = file("."))
  lazy val app = Project(id = "lift-json-app", base = file("app")).dependsOn(liftJson)
}