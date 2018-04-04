name := "simple-lang"
version := "0.0.1"

inThisBuild(Seq(
  scalaOrganization := "org.typelevel",
  scalaVersion      := "2.12.4-bin-typelevel-4"
))

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.4" % "test",
  "org.typelevel" %% "cats-core" % "1.0.1",
  "com.chuusai" %% "shapeless" % "2.3.3",
  "com.lihaoyi" %% "fastparse" % "1.0.0"
)

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

logBuffered in Test := false

scalacOptions ++= Seq(
  "-Ypartial-unification",
  "-language:higherKinds",
  "-Xlog-implicits"
)
