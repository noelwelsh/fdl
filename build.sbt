// give the user a nice default project!
lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.noelwelsh",
      scalaVersion := "2.12.6"
    )),
    name := "Functional Differentiable Programming",
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7"),
    libraryDependencies ++= Seq(
      Dependencies.catsCore,
      Dependencies.catsEffect,
      Dependencies.catsScalaCheck,
      Dependencies.miniTest,
      Dependencies.miniTestLaws,
      Dependencies.scalaCheck
    )
  )
