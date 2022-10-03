
import Dependencies._

lazy val commonSettings = Seq(
  organization := "ch.wsl",
  version := "2.00-M2",
  scalaVersion := "2.13.9"
)

lazy val root = (project in file(".")).
  dependsOn(fireindiceslib, fireindicesui).
  aggregate(fireindiceslib, fireindicesui).
  settings(commonSettings: _*).
  settings(
    name := "firecalculator",
//    aggregate in proguard.proguard := false,
//      assembly / assemblyJarName := "firecalculator.jar",
//      assembly / mainClass := Some("ch.wsl.fireindices.app.LauncherApp")
////    assembly / assemblyMergeStrategy  := {
////      case PathList("META-INF", _*) => MergeStrategy.discard
////      case _                        => MergeStrategy.first
////    }
  )

lazy val fireindiceslib = project.in(file("fireindiceslib")).
  enablePlugins(BuildInfoPlugin).
  settings(commonSettings: _*).
  settings(
    libraryDependencies ++= fireindiceslibDeps,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "ch.wsl.fireindices.app",
    Proguard/proguard/javaOptions := Seq("-Xmx4g"),
    Proguard / proguardOptions ++= Seq("-dontnote", "-dontwarn",  "-ignorewarnings", "-dontoptimize"),
    Proguard / proguardOptions += "-keep class ch.qos.logback.** { *; }",
    Proguard / proguardOptions += "-keep public class org.slf4j.** { *;}",
    Proguard / proguardOptions += "-keep public class org.relique.jdbc.csv.* {public protected *;}",
//    Proguard / proguardOptions += "-keep class ch.wsl.fireindices.log.* { *; }",
    Proguard / proguardOptions += "-keep class ch.wsl.fireindices.** { *; }",
    Proguard / proguardOptions += "-keep class scala.** { *; }",

    Proguard / proguardMergeStrategies  += ProguardMerge.first("META-INF/MANIFEST.MF"),

    Proguard / proguardInputFilter := { file => None },

//    ThisBuild / assemblyMergeStrategy  := {
//      case PathList("module-info.class") => MergeStrategy.discard
//      case x if x.endsWith("/module-info.class") => MergeStrategy.discard
//      case x =>
//        val oldStrategy = (ThisBuild / assemblyMergeStrategy).value
//        oldStrategy(x)
//    }
  )

lazy val fireindicesui = project.dependsOn(fireindiceslib).
  settings(commonSettings: _*).
  settings(
    libraryDependencies ++= fireindicesuiDeps,
    Proguard / unmanagedJars := (baseDirectory.value ** "*.jar").classpath,
    Proguard/proguard/javaOptions := Seq("-Xmx4g"),
    Proguard / proguardOptions ++= Seq("-dontnote", "-dontwarn",  "-ignorewarnings", "-dontoptimize"),
    Proguard / proguardOptions += "-keep class ch.qos.logback.** { *; }",
    Proguard / proguardOptions += "-keep public class org.slf4j.** { *;}",
//    Proguard / proguardOptions += "-keep class ch.wsl.fireindices.log.* { *; }",
    Proguard / proguardOptions += "-keep class ch.wsl.fireindices.** { *; }",
    Proguard / proguardOptions += "-keep class scala.** { *; }",

    Proguard / proguardMergeStrategies += ProguardMerge.discard("META-INF/.*".r),

    Proguard / proguardInputFilter := { file => None }

  )

  
  

resolvers ++= Seq(  //"java.net Maven2 Repository" at "http://download.java.net/maven/2/",
                  //  "SourceForge" at "http://csvjdbc.sourceforge.net/maven2"
)



exportJars := true    //in order to create MANIFEST.MF

//
//
//ThisBuild / assemblyMergeStrategy  := {
//  case PathList("module-info.class") => MergeStrategy.discard
//  case x if x.endsWith("/module-info.class") => MergeStrategy.discard
//  case x =>
//    val oldStrategy = (ThisBuild / assemblyMergeStrategy).value
//    oldStrategy(x)
//}





enablePlugins(SbtProguard)

//inConfig(Proguard)(Proguard / javaOptions  := Seq("-Xmx4g"))

Proguard/proguard/javaOptions := Seq("-Xmx2g")

//fireindicesui/Proguard/proguard/javaOptions := Seq("-Xmx2g")
//
//fireindiceslib/Proguard/proguard/javaOptions := Seq("-Xmx2g")

Proguard/proguard/javaOptions := Seq("-Xmx4g")

//Proguard / proguardVersion := "7.2.2" // "5.2"

//Proguard / proguardOptions ++= Seq("-dontnote", "-dontwarn", "-ignorewarnings", "-dontshrink", "-dontwarn scala.swing.**")

Proguard / proguardOptions ++= Seq("-dontnote", "-dontwarn",  "-ignorewarnings", "-dontoptimize")

Proguard / proguardOptions += ProguardOptions.keepMain("ch.wsl.fireindices.app.LauncherApp")

Proguard / proguardOptions += "-keep public class org.relique.jdbc.csv.* {public protected *;}"

Proguard / proguardOptions += "-keep class ch.qos.logback.** { *; }"

Proguard / proguardOptions += "-keep public class org.slf4j.** { *;}"

//Proguard / proguardOptions += "-keep class ch.wsl.fireindices.log.* { *; }"

Proguard / proguardOptions += "-keep class ch.wsl.fireindices.** { *; }"

Proguard / proguardOptions+= "-keep class scala.** { *; }"


//Proguard / proguardOptions+= "-keep class com.toedter.** { *; }"

Proguard / proguardMerge  := true

//Proguard / proguardMergeStrategies += ProguardMerge.discard("META-INF/.*".r)


Proguard / proguardMergeStrategies  += ProguardMerge.first("META-INF/MANIFEST.MF")

Proguard / proguardMergeStrategies  += ProguardMerge.append("reference.conf")

Proguard / proguardMergeStrategies  += ProguardMerge.discard("*/module-info.class")

Proguard / proguardMergeStrategies  += ProguardMerge.discard("META-INF/versions/9/module-info.class")

Proguard / proguardMergeStrategies  += ProguardMerge.discard("META-INF/LICENSE.txt")

Proguard / proguardMergeStrategies  += ProguardMerge.discard("META-INF/INDEX.LIST")

Proguard / proguardMergeStrategies  += ProguardMerge.append("reference.conf")

Proguard / proguardMergeStrategies  += ProguardMerge.first("rootdoc.txt")

Proguard / proguardInputFilter := { file => None }

//Proguard / proguardInputFilter := { file =>
//  if (file.name == s"scala-library-${scalaVersion.value}.jar")
//    Some("!META-INF/**")
//  else
//    None
//}

//Proguard / proguardInputFilter := { file =>
//  file.name match {
//    case "scala-library.jar" => Some("!META-INF/**")
//    case _                   => None
//  }
//}

//to include resources
//ProguardKeys.options in Proguard += "-keepclassmembers class **.R$* {public static <fields>;}"

//ProguardKeys.options in Proguard += "-keep class **.R$*"


scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

