import sbt._

object Dependencies {
  // Library Versions
  val catsVersion       = "1.1.0"
  val catsEffectVersion = "0.10.1"

  val catsScalaCheckVersion = "0.1.0"
  val miniTestVersion       = "2.1.1"
  val scalaCheckVersion     = "1.14.0"

  // Libraries
  val catsCore   = "org.typelevel" %% "cats-core"   % catsVersion
  val catsEffect = "org.typelevel" %% "cats-effect" % catsEffectVersion

  val catsScalaCheck = "io.chrisdavenport" %% "cats-scalacheck" % catsScalaCheckVersion % "test"
  val miniTest       = "io.monix"          %% "minitest"        % miniTestVersion       % "test"
  val miniTestLaws   = "io.monix"          %% "minitest-laws"   % miniTestVersion       % "test"
  val scalaCheck     = "org.scalacheck"    %% "scalacheck"      % scalaCheckVersion     % "test"
}
