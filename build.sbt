name := "fprog-exercises"

version := "0.1.0"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
  "junit"             %  "junit"           % "4.11"   % "test",
  "org.scalatest"     %% "scalatest"       % "2.0"    % "test"
)

// Adding src/main/resources etc. to the source entries, so Eclipse "compiles" them, i.e. copies them to the target
EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource

// Add source entries to library dependencies
EclipseKeys.withSource := true
