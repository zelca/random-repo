package data

import org.specs2.mutable.Specification

class CountrySpec extends Specification {

  def fields = List("id", "code", "name")

  "Country" should {
    "be mapped" in {
      def map(v: String*) = (fields zip v).toMap

      val cc = List(map("1", "NL", "Netherlands"), map("NL"), map("2", "UA", "Ukraine")) collect {
        case Country(country) => country
      }
      cc mustEqual List(Country("1", "NL", "Netherlands"), Country("2", "UA", "Ukraine"))
    }
  }

}
