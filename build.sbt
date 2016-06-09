
import Dependencies._

lazy val commonSettings = Seq(
  organization := "ch.wsl",
  version := "1.22",
  scalaVersion := "2.11.8"
)

lazy val root = (project in file(".")).
  dependsOn(fireindiceslib, fireindicesui).
  aggregate(fireindiceslib, fireindicesui).
  settings(commonSettings: _*).
  settings(
    name := "firecalculator"
//    aggregate in proguard.proguard := false
  )

lazy val fireindiceslib = project.in(file("fireindiceslib")).
  enablePlugins(BuildInfoPlugin).
  settings(commonSettings: _*).
  settings(
    libraryDependencies ++= fireindiceslibDeps,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "ch.wsl.fireindices.app"
  )

lazy val fireindicesui = project.dependsOn(fireindiceslib).
  settings(commonSettings: _*).
  settings(
    libraryDependencies ++= fireindicesuiDeps
  )

  
  

resolvers ++= Seq(  "java.net Maven2 Repository" at "http://download.java.net/maven/2/",
                    "SourceForge" at "http://csvjdbc.sourceforge.net/maven2"
)







exportJars := true    //in order to create MANIFEST.MF

proguardSettings

inConfig(Proguard)(javaOptions in ProguardKeys.proguard := Seq("-Xmx2g"))

ProguardKeys.proguardVersion in Proguard := "5.2"

ProguardKeys.options in Proguard ++= Seq("-dontnote", "-dontwarn", "-ignorewarnings")

ProguardKeys.merge in Proguard := true

ProguardKeys.mergeStrategies in Proguard += ProguardMerge.first("META-INF/MANIFEST.MF")

ProguardKeys.mergeStrategies in Proguard += ProguardMerge.append("reference.conf")

ProguardKeys.mergeStrategies in Proguard += ProguardMerge.first("rootdoc.txt")

ProguardKeys.inputFilter in Proguard := { file => None }

ProguardKeys.options in Proguard += ProguardOptions.keepMain("ch.wsl.fireindices.app.LauncherApp")

ProguardKeys.options in Proguard += "-keep public class org.relique.jdbc.csv.* {public protected *;}"

//to make liftweb-json work ... but it doesn't work :o(
ProguardKeys.options in Proguard += "-keep class net.liftweb.json.* { *; }"
ProguardKeys.options in Proguard += "-keep class ch.wsl.fireindices.log.* { *; }"

ProguardKeys.options in Proguard += "-keep class ch.wsl.** { *; }"

ProguardKeys.options in Proguard += "-keep class scala.** { *; }"

//to include resources
//ProguardKeys.options in Proguard += "-keepclassmembers class **.R$* {public static <fields>;}"

//ProguardKeys.options in Proguard += "-keep class **.R$*"


scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

