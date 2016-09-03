name := "scala-http-parser"

version := "0.1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.specs2" % "specs2-core_2.11" % "3.8.4.1-scalaz-7.1" % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos")
