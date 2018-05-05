import scala.concurrent.{ExecutionContext, Future}
import scala.io.StdIn
import scala.util.matching.Regex


object Checklist {
  val variablePattern: Regex = """(?iU)[$]((\w+)|([{]([^}]*)[}]))""".r

  def getAttribute[A](globals: Map[String, A], locals: Map[String, A])(name: String): A = {
    locals.getOrElse(name, globals(name))
  }

  def evaluate[A](globals: Map[String, String], locals: Map[String, String])(code: String): A = {
    Eval[A]("""\w+""".r.replaceAllIn(code, (replacer) => getAttribute(globals, locals)(replacer.group(0))))
  }

  trait Element {
    def run(globalFunctions: Map[String, Function], globals: Map[String, String]): List[String]
  }

  case class Function(paramNames: Iterable[String], block: Block) {
    def apply(params: Iterable[String],
              globalFunctions: Map[String, Function], globals: Map[String, String]): List[String] =
    {
      block.run(globalFunctions, globals ++ Map[String, String](paramNames.zip(params).toList: _*))
    }
  }

  case class Block(lines: Iterable[String]) extends Element {
    val blocks = Map.empty[String, Block]

    val indentPattern: Regex = """(?iU)^([ ]{4}|\t)(.*)$""".r

    val functionDefinitionPattern: Regex = """(?iU)^[$][$](\w+)[(](.*)[)]\s*$""".r
    val functionCallPattern: Regex = """(?iU)^[$](\w+)[(](.*)[)]\s*$""".r
    val ifPattern: Regex = """(?iU)^[$][$][{](.*)[}]\s*$""".r
    val elsePattern: Regex = """(?iU)^[$][$]\s*$""".r
    val inputPattern: Regex = """(?iU)^[-][>]\s*(.*)\s*[:]\s*(\w+)\s*$""".r

    override def run(globalFunctions: Map[String, Function] = Map.empty[String, Function],
                     globals: Map[String, String] = Map.empty[String, String]): List[String] =
    {
      var result = List.empty[String]

      var localFunctions = Map.empty[String, Function]
      var locals = Map.empty[String, String]

      var linesIterator = lines.iterator
      while (linesIterator.hasNext) {
        linesIterator.next match {
          case functionDefinitionPattern(functionName, functionParams) =>
            val (blockIterator, nextIterator) = linesIterator.span(indentPattern.findFirstIn(_).isDefined)
            linesIterator = nextIterator

            localFunctions += (functionName -> Function("""\s*[,]\s*""".r.split(functionParams),
              Block(blockIterator.map(indentPattern.findFirstMatchIn(_).get.group(1)).to)))

          case functionCallPattern(functionName, functionParams) =>
            result :::= getAttribute(localFunctions, globalFunctions)(functionName)(
              """\s*[,]\s*""".r.split(functionParams), globalFunctions, globals)

          case ifPattern(statement) =>
            val (ifElseIterator, nextIterator) = linesIterator.span(p => elsePattern.findFirstIn(p).isDefined || indentPattern.findFirstIn(p).isDefined)
            val ifIterator = ifElseIterator.takeWhile(indentPattern.findFirstIn(_).isDefined)
            if (evaluate[Boolean](globals, locals)(statement))
              result :::= Block(ifIterator.map(indentPattern.findFirstMatchIn(_).get.group(1)).to).run(
                globalFunctions ++ localFunctions, globals ++ locals)
            else
              result :::= Block(ifElseIterator.map(indentPattern.findFirstMatchIn(_).get.group(1)).to).run(
                globalFunctions ++ localFunctions, globals ++ locals)

          case inputPattern(description, name) =>
            print(s"$description: ")
            locals += (name -> StdIn.readLine())

          case line =>
            result :::= Item(line).run(globalFunctions ++ localFunctions, globals ++ locals)
        }
      }
      result
    }
  }

  case class Item(code: String) extends Element {
    def calculateContext(globals: Map[String, String])(matcher: Regex.Match): String = {
      if (matcher.group(2) == null) {
        Eval[String]("""(?iU)\w+""".r.replaceAllIn(matcher.group(4), (replacer) => '"' + globals(replacer.group(0)) + '"'))
      }
      else {
        globals(matcher.group(2))
      }
    }

    def substituteVariables(globals: Map[String, String])(code: String): String = {
      var parts = variablePattern.split(code)
      val matches = variablePattern.findAllMatchIn(code).toList

      println(parts.length)

      while (parts.length <= matches.length) parts :+= ""

      StringContext(parts: _*).s(matches.map(calculateContext(globals)): _*)
    }

    override def run(globalFunctions: Map[String, Function], globals: Map[String, String]): List[String] = {
      substituteVariables(globals)(code) :: Nil
    }
  }

  def run(code: String): List[String] =
    Block(code.split("\n")).run()
}
