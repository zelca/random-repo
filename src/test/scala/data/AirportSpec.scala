package data

import org.specs2.mutable.Specification

class AirportSpec extends Specification {

  def fields = List("id", "name", "iso_country")

  "Airport Fields" should {
    "be equal" in {
      Airport.Fields mustEqual fields
    }
  }

  "Airport" should {
    "be mapped" in {
      def map(v: String*) = (fields zip v).toMap

      val aa = List(map("1", "Test 1", "NL"), map("1"), map("2", "Test 2", "DE")) collect {
        case Airport(airport) => airport
      }
      aa mustEqual List(Airport("1", "Test 1", "NL"), Airport("2", "Test 2", "DE"))
    }
  }

}
