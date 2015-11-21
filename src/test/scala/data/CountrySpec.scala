package data

import org.specs2.mutable.Specification

class CountrySpec extends Specification {

  val fields = List("code", "name")

  "Country Fields" should {
    "be equal" in {
      Country.Fields mustEqual fields
    }
  }

  "Country" should {
    "be mapped" in {
      def map(v: String*) = (fields zip v).toMap

      val cc = List(map("NL", "Netherlands"), map("NL"), map("UA", "Ukraine")) collect {
        case Country(country) => country
      }
      cc mustEqual List(Country("NL", "Netherlands"), Country("UA", "Ukraine"))
    }
  }

}
