package util

import java.io.ByteArrayInputStream

import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import util.Parser._

class ParserSpec extends Specification with Mockito {

  val data = new ByteArrayInputStream("\"id\",\"name\"\n\"1\",\"test\"".getBytes)

  "Parser" should {
    "close resource" in {
      val stream = spy(data)
      parse(stream)
      there was one(stream).close()
    }
    "parse csv" in {
      parse(data).toList mustEqual List(Map("id" -> "1", "name" -> "test"))
    }
  }

}
