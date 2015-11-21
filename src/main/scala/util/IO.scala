package util

trait IO {

  val Separator = "------------------------------"

  def write(text: String) = {
    println(Separator)
    println(text)
  }

  def read(text: String)(processor: String => Boolean) =
    do {
      println(Separator)
      print(text)
    } while (processor(scala.io.StdIn.readLine().trim))

}
