
import sbt._

object Dependencies {
  // Versions
  lazy val akkaVersion = "2.3.8"

  // Libraries
  val arm 		= "com.jsuereth" %% "scala-arm" % "1.4"
  val argot 	= "org.clapper" % "argot_2.11" % "1.0.3"
  val csvjdbc	= "net.sourceforge.csvjdbc" % "csvjdbc" % "1.0.20"
  val liftJson	= "net.liftweb" %% "lift-json" % "2.6.2"    //for json  //do not work well with proguard (reflection)
  
//                            "javax.transaction" % "jta" % "1.1"
  val scalaLogging = "com.typesafe.scala-logging" %% "scala-logging" % "3.4.0"
  val logback      = "ch.qos.logback" % "logback-classic" % "1.1.7"
  val scalatest	= "org.scalatest" % "scalatest_2.11" % "2.2.6"

  val scalaSwing = "org.scala-lang.modules" % "scala-swing_2.11" % "2.0.0-M2"
  val miglayout  = "com.miglayout" % "miglayout-swing" % "4.2"



  // Projects
  val fireindiceslibDeps =
    Seq(arm, argot, csvjdbc, liftJson, scalaLogging, logback, scalatest % Test)
    
  val fireindicesuiDeps = fireindiceslibDeps ++ Seq(scalaSwing, miglayout)
}
