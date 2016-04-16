name := "SALMA-SC"

version := "1.0"

scalaVersion := "2.11.7"

publishArtifact in Test := true

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.7"

javaOptions in (Test,run) += "-Declipse.directory=/opt/eclipseclp -Djava.library.path=/opt/eclipseclp/lib/x86_64_macosx"