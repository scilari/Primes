import mill._, scalalib._, scalanativelib._, mill.scalajslib._

val scalaVersionString = "2.13.7"

object sieveJVM extends ScalaModule {
  def scalaVersion = scalaVersionString
  def millSourcePath = os.pwd / 'sieve

  def ivyDeps = Agg(
    ivy"org.scala-lang.modules::scala-parallel-collections::1.0.4"
  )
}


