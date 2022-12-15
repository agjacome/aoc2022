import $ivy.`io.github.davidgregory084::mill-tpolecat::0.3.1`

import mill._
import mill.scalalib._
import mill.scalalib.scalafmt.ScalafmtModule

import io.github.davidgregory084.TpolecatModule

object Versions {
  val Scala = "2.13.10"

  val BetterMonadicFor = "0.3.1"
  val ContextApplied   = "0.1.4"
  val KindProjector    = "0.13.2"

  val MUnit = "1.0.0-M7"
}

trait BaseModule extends ScalaModule with ScalafmtModule with TpolecatModule

trait Module extends BaseModule {

  def scalaVersion = Versions.Scala

  def scalacPluginIvyDeps =
    super.scalacPluginIvyDeps() ++ Agg(
      ivy"com.olegpy::better-monadic-for:${Versions.BetterMonadicFor}",
      ivy"org.augustjune::context-applied:${Versions.ContextApplied}",
      ivy"org.typelevel:::kind-projector:${Versions.KindProjector}"
    )

  def scalacOptions = T(super.scalacOptions() :+ "-Ymacro-annotations")

  def sources = T.sources(os.pwd / "src")

  object test extends BaseModule with Tests with TestModule.Munit {
    def ivyDeps = Agg(ivy"org.scalameta::munit:${Versions.MUnit}")
    def sources = T.sources(os.pwd / "test")
  }

}

object aoc2022 extends Module
