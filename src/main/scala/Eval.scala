object Eval {

  import scala.reflect.runtime.{currentMirror, universe}
  import scala.tools.reflect.ToolBox

  lazy val toolbox: ToolBox[universe.type] = currentMirror.mkToolBox()

  def apply[A](string: String): A = {
    val tree = toolbox.parse(string)
    toolbox.eval(tree).asInstanceOf[A]
  }
}