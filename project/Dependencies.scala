
import sbt._

object Dependencies {
  // Versions
//  lazy val akkaVersion = "2.3.8"

  // Libraries
  val arm 		= "com.michaelpollmeier" %% "scala-arm" % "2.1"
  val scallop 	= "org.rogach" %% "scallop" % "4.1.0"
  val csvjdbc	= "net.sourceforge.csvjdbc" % "csvjdbc" % "1.0.40"
//  val liftJson	= "net.liftweb" %% "lift-json" % "2.6.2"    //for json  //do not work well with proguard (reflection)

//                            "javax.transaction" % "jta" % "1.1"
  val scalaLogging = "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5"
  val logback      = "ch.qos.logback" % "logback-classic" % "1.3.3"    //1.4.x does not work with java 8
  val scalatest	= "org.scalatest" %% "scalatest" % "3.2.14"

  val scalaSwing = "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
  val miglayout  = "com.miglayout" % "miglayout-swing" % "5.2"

//  val gtkFileChooser = "eu.kostia" % "gtkfilechooser" % "provided"

  // Projects
  val fireindiceslibDeps =
    Seq(arm, scallop, csvjdbc, scalaLogging, logback, scalatest % Test)
//  Seq(arm, scallop, csvjdbc, liftJson, scalaLogging, logback, scalatest % Test)

  val fireindicesuiDeps = fireindiceslibDeps ++ Seq(scalaSwing, miglayout)
}
