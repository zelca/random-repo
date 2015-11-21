import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import util.IO

class AirportDatabaseSpec extends Specification with Mockito {

  def intro = " Welcome to Airport Database!"

  def outro = " Thank you. Come again."

  "AirportDatabase1" should {
    "wait for correct input" in {
      val app = new AirportDatabase with MockIO
      app.input = List("xx", "3")
      app.output = List(intro, " Invalid input. Try again.", outro)

      app.menu
      app.input mustEqual Nil
      app.output mustEqual Nil
    }
    "print reports" in {
      val app = new AirportDatabase with MockIO
      app.input = List("2", "3")
      val reports = List(
        """Top 10 by number of airports:
          |
          |Country[NL]: Netherlands - 2
          |Country[TT]: Test - 0""".stripMargin,
        """Bottom 10 by number of airports:
          |
          |Country[TT]: Test - 0
          |Country[NL]: Netherlands - 2""".stripMargin,
        """Surfaces by country:
          |
          |Country[NL]: Netherlands - GR
          |Country[TT]: Test - """.stripMargin,
        "Top 10 latitude: LB, LA")
      app.output = (intro :: reports) ++ List(outro)

      app.menu
      app.input mustEqual Nil
      app.output mustEqual Nil
    }
    "print empty query result" in {
      val app = new AirportDatabase with MockIO
      app.input = List("1", "xx", "3")
      app.output = List(intro, " 0 countries were found.", outro)

      app.menu
      app.input mustEqual Nil
      app.output mustEqual Nil
    }
    "print query result" in {
      val app = new AirportDatabase with MockIO
      app.input = List("1", "ne", "3")
      val result = """Country[NL]: Netherlands
                     |  Airport[A1]: Test 1
                     |    Runway[R1] surface: GR
                     |    Runway[R2] surface: GR
                     |    Runway[R3] surface: GR
                     |  Airport[A2]: Test 2""".stripMargin
      app.output = List(intro, result, outro)

      app.menu
      app.input mustEqual Nil
      app.output mustEqual Nil
    }
  }

  trait MockIO extends IO {

    var output = List[String]()

    var input = List[String]()

    override def write(text: String) =
      if (output.head == text) output = output.tail

    override def read(text: String)(processor: (String) => Boolean) = {
      var next = ""
      do {
        next = input.head
        input = input.tail
      } while (processor(next))
    }
  }

}
