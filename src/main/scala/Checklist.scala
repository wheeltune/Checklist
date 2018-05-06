import scala.io.StdIn
import scala.util.matching.Regex


object Checklist {
  val variablePattern: Regex = """[$]((\pL[\pL\d_]*)|([{]([^}]*)[}]))""".r

  def getAttribute[A](globals: Map[String, A], locals: Map[String, A])(name: String): A = {
    locals.getOrElse(name, globals(name))
  }

  def evaluate[A](globals: Map[String, String], locals: Map[String, String])(code: String): A = {
    Eval[A]("""(^|\s+)(\pL[\pL\d_]*)($|\s+)""".r.replaceAllIn(code, (replacer) => getAttribute(globals, locals)(replacer.group(2))))
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

    val indentPattern: Regex = """^([ ]{2}|\t)(.*)$""".r

    val functionDefinitionPattern: Regex = """^[$][$](\pL[\pL\d_]*)[(](.*)[)]\s*$""".r
    val functionCallPattern: Regex = """^[$](\pL[\pL\d_]*)[(](.*)[)]\s*$""".r
    val ifPattern: Regex = """^[$][$][{](.*)[}]\s*$""".r
    val elsePattern: Regex = """^[$][$]\s*$""".r
    val inputPattern: Regex = """^[-][>]\s*(.*)\s*[:]\s*(\pL[\pL\d_]*)\s*[:]\s*(\pL[\pL\d_]*)\s*$""".r

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

            localFunctions += (functionName -> Function(
              """\s*[,]\s*""".r.split(functionParams),
              Block(blockIterator.map(indentPattern.findFirstMatchIn(_).get.group(2)).to)))

          case functionCallPattern(functionName, functionParams) =>
            result :::= getAttribute(localFunctions, globalFunctions)(functionName)(
              """\s*[,]\s*""".r.split(functionParams), globalFunctions, globals)

          case ifPattern(statement) =>
            val (ifElseIterator, nextIterator) = linesIterator.span(
              p => elsePattern.findFirstIn(p).isDefined || indentPattern.findFirstIn(p).isDefined)
            linesIterator = nextIterator

            if (evaluate[Boolean](globals, locals)(statement)) {
              val ifIterator = ifElseIterator.takeWhile(indentPattern.findFirstIn(_).isDefined)
              result :::= Block(ifIterator.map(indentPattern.findFirstMatchIn(_).get.group(2)).to).run(
                globalFunctions ++ localFunctions, globals ++ locals)
            } else {
              val elseIterator = ifElseIterator.dropWhile(indentPattern.findFirstIn(_).isDefined).drop(1)
              result :::= Block(elseIterator.map(indentPattern.findFirstMatchIn(_).get.group(2)).to).run(
                globalFunctions ++ localFunctions, globals ++ locals)
            }

          case inputPattern(description, valueType, name) =>
            print(s"$description: ")
            valueType match {
              case "Строка" =>
                locals += (name -> ("\"" + StdIn.readLine() + "\""))
              case _ =>
                locals += (name -> StdIn.readLine())
            }

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
        evaluate(globals, Map.empty)(matcher.group(4)).toString
      } else {
        globals(matcher.group(2))
      }
    }

    def substituteVariables(globals: Map[String, String])(code: String): String = {
      var parts = variablePattern.split(code)
      val matches = variablePattern.findAllMatchIn(code).toList

      while (parts.length <= matches.length) parts :+= ""

      StringContext(parts: _*).s(matches.map(calculateContext(globals)): _*)
    }

    override def run(globalFunctions: Map[String, Function], globals: Map[String, String]): List[String] = {
      substituteVariables(globals)(code) :: Nil
    }
  }

  def run(code: String): List[String] =
    Block(code.split("\n")).run().reverse
}
