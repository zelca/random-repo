package util

trait IO {

  val Separator = "------------------------------"

  def output(text: String) = {
    println(Separator)
    println(text)
  }

  def input(text: String)(processor: String => Boolean) =
    do {
      println(Separator)
      print(text)
    } while (processor(scala.io.StdIn.readLine().trim))

}
