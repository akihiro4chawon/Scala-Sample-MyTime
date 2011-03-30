import sbt._
import de.element34.sbteclipsify._

class MyTime(info: ProjectInfo) extends DefaultProject(info) with Eclipsify {
    //val junit = "junit" % "junit" % "4.4" % "test"
    val scalatest = "org.scalatest" % "scalatest" % "1.3" % "test"
}

