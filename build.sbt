name := "opticas"

version := "0.1"

scalaVersion := "2.12.4"

val monocleVersion = "1.4.0" // 1.5.0-cats-M1 based on cats 1.0.0-MF

libraryDependencies ++= Seq(
  "com.github.julien-truffaut" %%  "monocle-core"  % monocleVersion,
  "com.github.julien-truffaut" %%  "monocle-macro" % monocleVersion,
  "com.github.julien-truffaut" %%  "monocle-law"   % monocleVersion % "test",
  "org.scalatest" %% "scalatest" % "3.0.4" % "test"
)