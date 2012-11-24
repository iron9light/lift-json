scalaVersion := "2.10.0-RC3"

organization in ThisBuild          := "net.liftweb"

version in ThisBuild               := "3.0-SNAPSHOT"

homepage in ThisBuild              := Some(url("http://www.liftweb.net"))

licenses in ThisBuild              += ("Apache License, Version 2.0", url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

startYear in ThisBuild             := Some(2006)

organizationName in ThisBuild      := "WorldWide Conferencing, LLC"

libraryDependencies in ThisBuild  <+= scalaVersion { sv => "org.scala-lang" % "scala-reflect" % sv }

libraryDependencies in ThisBuild   += "org.specs2" %% "specs2" % "1.12.3" % "test"