import sbt._
import reaktor.scct.ScctProject

class GoCompilerProject(info: ProjectInfo) extends DefaultProject(info) with AutoCompilerPlugins {
  //val kiama = "com.googlecode" %% "kiama" % "[1.0.2,)" withSources()
  //val scalacheck = "org.scala-tools.testing" %% "scalacheck" % "[1.8,)" withSources() withJavadoc()
  //val scalatest = "org.scalatest" % "scalatest" % "[1.3,)"
  
  val core = "com.github.scala-incubator.io" %% "core" % "[0.1.2,)"
  val file = "com.github.scala-incubator.io" %% "file" % "[0.1.2,)"
  
  val sxr = compilerPlugin("org.scala-tools.sxr" %% "sxr" % "[0.2.7,)")
  
  override def compileOptions =
    super.compileOptions :::
    Unchecked ::
    CompileOption("-Xplugin-list") ::
    CompileOption("-P:sxr:base-directory:" + mainScalaSourcePath.absolutePath) ::
    Nil
     //++
    //compileOptions("-Xlog-implicits") //-Yclosure-elim -Ydead-code -Yinline -optimise
  
  override def compileOrder = CompileOrder.ScalaThenJava
}
