import play.routes.compiler.InjectedRoutesGenerator
import play.sbt.PlayScala

name := """style-id"""

version := "0.4.0"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws,
  filters,
  "com.aliyun.oss" % "aliyun-sdk-oss" % "2.0.1",
  "com.typesafe.play" %% "anorm" % "2.5.0",
  "com.github.mumoshu" %% "play2-memcached-play24" % "0.7.0"

)
libraryDependencies += "com.fasterxml.jackson.core" % "jackson-core" % "2.6.3"
libraryDependencies += "com.fasterxml.jackson.core" % "jackson-databind" % "2.6.3"
libraryDependencies += "com.fasterxml.jackson.core" % "jackson-annotations" % "2.6.3"
libraryDependencies += "com.fasterxml.jackson.module" % "jackson-module-scala_2.11" % "2.6.3"
libraryDependencies += "org.postgresql" % "postgresql" % "9.4-1206-jdbc41"
libraryDependencies += "com.squareup.okhttp" % "okhttp" % "2.7.5"

libraryDependencies += "com.typesafe.akka" % "akka-actor_2.11" % "2.4.1" withSources() withJavadoc()

libraryDependencies += "com.typesafe.akka" % "akka-kernel_2.11" % "2.4.1" withSources() withJavadoc()

libraryDependencies += "com.typesafe.akka" % "akka-slf4j_2.11" % "2.4.1" withSources() withJavadoc()

libraryDependencies += "com.typesafe.akka" %% "akka-remote" % "2.4.1" withSources() withJavadoc()

libraryDependencies += "redis.clients" % "jedis" % "2.8.1"

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"



// Play provides two styles of routers, one expects its actions to be injected, the
// other, legacy style, accesses its actions statically.
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
javacOptions += "-Xlint:unchecked"

routesGenerator := InjectedRoutesGenerator
