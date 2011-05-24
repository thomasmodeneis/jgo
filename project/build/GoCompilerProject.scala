import sbt._
import reaktor.scct.ScctProject

class GoCompilerProject(info: ProjectInfo) extends DefaultProject(info) with ScctProject {
  //val kiama = "com.googlecode" %% "kiama" % "[1.0.2,)" withSources()
  //val scalacheck = "org.scala-tools.testing" %% "scalacheck" % "[1.8,)" withSources() withJavadoc()
  //val scalatest = "org.scalatest" % "scalatest" % "[1.3,)"
  
  override def compileOptions = super.compileOptions ++ Seq(Unchecked, ExplainTypes) //++
    //compileOptions("-Xlog-implicits") //-Yclosure-elim -Ydead-code -Yinline -optimise
}
