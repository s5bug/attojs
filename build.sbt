lazy val attojs = (project in file("attojs")).settings(
  organization := "tf.bug",
  name := "attojs",
  version := "0.1.0",
  scalaVersion := "2.13.1",
  libraryDependencies ++= Seq(
    "org.tpolecat" %% "atto-core" % "0.7.2",
    "org.scalatest" %% "scalatest" % "3.1.0" % "test",
  ),
)
