resolvers += "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"

//testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-workers", "1")
