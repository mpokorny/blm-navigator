/* Copyright © 2016 Martin Pokorny <martin@truffulatree.org>
 *
 * This file is part of blm-navigator.
 *
 * blm-navigator is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * blm-navigator is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * blm-navigator. If not, see <http://www.gnu.org/licenses/>.
 */
name := "blm-navigator"

val catsVersion = "0.8.1"

val circeVersion = "0.6.1"

val roshttpVersion = "2.0.0-RC1"

val poiVersion = "3.14"

val scalaJsDomVersion = "0.9.1"

lazy val commonSettings = Seq(
    version := "0.1.0-SNAPSHOT",
    organization := "org.truffulatree",
    licenses := Seq(
        "GNU General Public License Version 3.0" ->
          url("https://www.gnu.org/licenses/gpl-3.0.txt")),
    homepage := Some(url("https://github.com/mpokorny/blm-navigator")),
    scalaVersion in ThisBuild := "2.11.8",
    scalacOptions ++= Seq(
        "-deprecation",
        "-unchecked",
        "-feature",
        "-explaintypes",
        "-Xlint"),
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"),
    resolvers += Resolver.sonatypeRepo("releases"),
    libraryDependencies ++= Seq(
        "org.typelevel" %%% "cats" % catsVersion,
        "org.scalatest" %% "scalatest" % "3.0.1" % "test"),
    publishMavenStyle := true,
    publishTo <<= version { (v: String) =>
        val nexus = "https://oss.sonatype.org/"
        if (v.trim.endsWith("SNAPSHOT"))
          Some("snapshots" at nexus + "content/repositories/snapshots")
        else
          Some("staging"  at nexus + "service/local/staging/deploy/maven2")
      },
    credentials += Credentials(Path.userHome / ".sbt" / "sonatype.txt"),
    publishArtifact in Test := false,
    pomIncludeRepository := { _ => false },
    pomExtra := (
      <scm>
        <url>git@github.com:mpokorny/blm-navigator.git</url>
        <connection>scm:git:git@github.com:mpokorny/blm-navigator.git</connection>
        </scm>
        <developers>
        <developer>
        <id>martin</id>
        <name>Martin Pokorny</name>
        <email>martin@truffulatree.org</email>
        <timezone>America/Denver</timezone>
        </developer>
        </developers>)
  )

lazy val root = (project in file(".")).
  settings((commonSettings ++ Seq(packagedArtifacts := Map.empty)): _*).
  aggregate(libblmnavRoot , trs2LatLonRoot)

lazy val libblmnavRoot = project.in(file("libblmnav")).
  aggregate(libblmnavJVM, libblmnavJS).
  settings(
    publish := {},
    publishLocal := {})

lazy val libblmnav = crossProject.in(file("libblmnav")).
  settings(commonSettings: _*).
  settings(
    libraryDependencies ++=
      Seq("io.circe" %%% "circe-core" % circeVersion,
          "io.circe" %%% "circe-generic" % circeVersion,
          "io.circe" %%% "circe-parser" % circeVersion,
          "fr.hmil" %%% "roshttp" % roshttpVersion))

lazy val libblmnavJVM = libblmnav.jvm

lazy val libblmnavJS = libblmnav.js.enablePlugins(ScalaJSPlugin)

lazy val trs2LatLonRoot = project.in(file("trs2LatLon")).
  aggregate(trs2LatLonJS, trs2LatLonJVM).
  settings(
    publish := {},
    publishLocal := {})

lazy val trs2LatLon = crossProject.in(file("trs2LatLon")).
  settings(commonSettings: _*).
  settings(
    libraryDependencies +=
      "fr.hmil" %%% "roshttp" % roshttpVersion).
  /* jvmSettings(
   *   libraryDependencies +=
   *     "org.apache.poi" % "poi" % poiVersion). */
  jsSettings(
    libraryDependencies +=
      "org.scala-js" %%% "scalajs-dom" % scalaJsDomVersion).
  dependsOn(libblmnav)

lazy val trs2LatLonJVM = trs2LatLon.jvm

lazy val trs2LatLonJS = trs2LatLon.js.enablePlugins(ScalaJSPlugin)
