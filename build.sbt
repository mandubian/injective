name := "injective"

scalaVersion := "2.11.0"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  "com.chuusai"     %% "shapeless"        % "2.0.0",
  "org.scalaz"      %% "scalaz-core"      % "7.1.0-M7",
  "org.scalatest"   %  "scalatest_2.11"   % "2.1.3"             % "test",
  "org.scala-lang"  %  "scala-reflect"    % scalaVersion.value  % "provided",
  "org.scala-lang"  %  "scala-compiler"   % scalaVersion.value  % "test"    
)

resolvers ++= Seq(
  "Sonatype OSS Releases"  at "http://oss.sonatype.org/content/repositories/releases/",
  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
)


scalacOptions ++= Seq("-unchecked", "-deprecation", "-Xlog-implicits")
