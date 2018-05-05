

object ChecklistApp extends App {
  Checklist.run(
    """Носки
      |Тапочки
      |-> Куку: куку
      |${куку}
    """.stripMargin
  ).foreach(println)

//  test = test.updated("a", "a")
//  print(test("a"))
//  println(test("b", "test"))
//  println(Eval[String](s"($test)"))
//  println(Eval[Int](s"($test + 2) / 2 + 10 - 100 * 2"))
//  println(Eval[Int](s"($test + 2) / 2 + 10 - 100 * 2"))
//  println(Eval[Int](s"($test + 2) / 2 + 10 - 100 * 2"))
//  println(Eval[Int](s"($test + 2) / 2 + 10 - 100 * 2"))
//  println(Eval[Int](s"($test + 2) / 2 + 10 - 100 * 2"))
//  println(Eval[Int](s"($test + 2) / 2 + 10 - 100 * 2"))
//  println(Eval[Int](s"($test + 2) / 2 + 10 - 100 * 2"))
//  println(Eval[Int](s"($test + 2) / 2 + 10 - 100 * 2"))
//  val code = "$тест "
//  var variables = Map.empty[String, String]
//  variables = variables.updated("тест", "Привет")
//
//
//  println(StringContext("a", "b").s(code))
//
//  val pattern = """(?iU)[$]((\w+)|([{][^\}]*[}]))""".r
//  print(StringContext(pattern.split(code): _*).s(
//    pattern.findAllMatchIn(code).map((f) => variables(f.group(1))).toList: _*))
}
