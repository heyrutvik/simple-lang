name := "simple-lang"
version := "0.0.1"
scalaVersion := "2.12.2"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.4" % "test",
  "org.typelevel" %% "cats-core" % "1.0.1"
)

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

logBuffered in Test := false

scalacOptions ++= Seq(
  "-Ypartial-unification",
  "-language:higherKinds"
)
