name := "Yahtzee"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.0",
  "org.typelevel" %% "cats" % "0.4.0",
  "org.scalacheck" %% "scalacheck" % "1.13.0" % "test"
)

