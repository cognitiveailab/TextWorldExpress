name := "textworldexpress"

version := "1.0.0"

//scalaVersion := "2.12.9"
scalaVersion := "2.13.8"

resolvers += "jetbrains-intellij-dependencies" at "https://packages.jetbrains.team/maven/p/ij/intellij-dependencies"

libraryDependencies += "net.liftweb" %% "lift-json" % "3.4.3"

// Parser combinators
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.0.0"

// For JSON in SuperFastTextGames
libraryDependencies += "com.lihaoyi" %% "upickle" % "2.0.0"
libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.8.0"

// JSON Serialization
val circeVersion = "0.14.1"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)


// PY4J (Java <-> Python Interoperability)
libraryDependencies += "net.sf.py4j" % "py4j" % "0.10.3"

// Set main class to be the Python Interface
mainClass in Compile := Some("textworldexpress.runtime.PythonInterface")

//
// Proguard
//

/*
enablePlugins(SbtProguard)
proguardOptions in Proguard ++= Seq("-dontoptimize", "-dontobfuscate", "-dontnote", "-dontwarn", "-ignorewarnings")
proguardOptions in Proguard += "-keepclasseswithmembers class textworldexpress.runtime.PythonInterface {*;}"
javaOptions in (Proguard, proguard) := Seq("-Xmx1G")
*/
