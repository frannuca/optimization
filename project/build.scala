import sbt._
import Keys._


//import com.github.retronym.SbtOneJar

object BuildSettings {


  val buildOrganization = "org.fjn"
  val buildVersion      = "1.0.0"
  val buildScalaVersion = "2.10.3"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization,
    version      := buildVersion,
    scalaVersion := buildScalaVersion//,
  )
}

object Resolvers {


}

object Dependencies {

     val matrixdep = "org.fjn" %"matrix_2.10" % "1.0.0"
}

object PythiaBuild extends Build {
  import Resolvers._
  import Dependencies._
  import BuildSettings._

  /**
   * top layer  pythia
   */
  lazy val optimizationPrj = Project (
    "optimization",
    file ("."),
    settings = buildSettings++ Seq (resolvers :=  Seq(), libraryDependencies ++=Seq(matrixdep))

  ) //aggregate (optimizer,ia, org.fjn.org.fjn.org.fjn.pythia.pricers)



}
