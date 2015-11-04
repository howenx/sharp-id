import play.routes.compiler.InjectedRoutesGenerator
import play.sbt.PlayScala

name := """style-id"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws,
  specs2 % Test,
  "org.postgresql" % "postgresql" % "9.3-1102-jdbc41",
  "com.aliyun.oss" % "aliyun-sdk-oss" % "2.0.1",
  "com.typesafe.play" %% "anorm" % "2.5.0",
  "com.typesafe.play" % "play-mailer_2.11" % "2.4.0",
  "com.github.mumoshu" %% "play2-memcached-play24" % "0.7.0"

)

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

// Play provides two styles of routers, one expects its actions to be injected, the
// other, legacy style, accesses its actions statically.
routesGenerator := InjectedRoutesGenerator
