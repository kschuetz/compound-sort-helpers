import SonatypeKeys._

sonatypeSettings

name := "compound-sort-helpers"

organization := "com.github.kschuetz"

version := "1.0.0"

crossScalaVersions := Seq("2.10.1", "2.10.0", "2.9.2", "2.9.1", "2.9.0-1", "2.9.0")

scalaVersion := "2.10.2"

libraryDependencies += "org.scalacheck" % "scalacheck_2.10" % "1.11.2" % "test"

pomExtra := {
  <url>https://github.com/kschuetz/compound-sort-helpers</url>
    <licenses>
      <license>
        <name>Apache 2</name>
        <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
      </license>
    </licenses>
    <scm>
      <connection>scm:git:git@github.com:kschuetz/compound-sort-helpers.git</connection>
      <developerConnection>scm:git:git@github.com:kschuetz/compound-sort-helpers.git</developerConnection>
      <url>github.com/kschuetz/compound-sort-helpers.git</url>
    </scm>
    <developers>
      <developer>
        <id>kschuetz</id>
        <name>Kevin Schuetz</name>
        <url>http://www.kschuetz.com</url>
      </developer>
    </developers>
}