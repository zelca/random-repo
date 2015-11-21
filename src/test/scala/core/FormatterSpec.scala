package core

import core.Formatter._
import data.{Airport, Country, Runway}
import org.specs2.mutable.Specification

class FormatterSpec extends Specification {

  val cc = List(Country("NL", "Netherlands"), Country("TT", "Test"))

  val aa = Map("NL" -> List(Airport("A1", "Test 1", "NL"), Airport("A2", "Test 2", "NL")))

  val rr = Map("A1" -> List(Runway("R1", "A1", "GR", "L1")))

  "format" should {
    "format Country" in {
      format(Country("NL", "Netherlands")) mustEqual "Country[NL]: Netherlands"
    }
    "format Airport" in {
      format(Airport("A1", "Test Airport", "NL")) mustEqual "Airport[A1]: Test Airport"
    }
    "format Runway" in {
      format(Runway("R1", "A1", "GR", "L1")) mustEqual "Runway[R1] surface: GR"
    }
  }

  "formatDistinctSurfaces" should {
    "format" in {
      val data = cc.map(c => (c, List("A", "B")))
      formatDistinctSurfaces(data) mustEqual "Country[NL]: Netherlands - A, B\nCountry[TT]: Test - A, B"
    }
  }

  "formatCommonLatitudes" should {
    "format" in {
      formatCommonLatitudes(List("LB", "LA")) mustEqual "LB, LA"
    }
  }

  "formatAirportsNumber" should {
    "format" in {
      val data = cc zip List(2, 1)
      formatAirportsNumber(data) mustEqual "Country[NL]: Netherlands - 2\nCountry[TT]: Test - 1"
    }
  }

  "formatCountries" should {
    "format" in {
      val data = cc.map(c => (c, aa.getOrElse(c.code, Nil).map(a => (a, rr.getOrElse(a.id, Nil)))))
      val expected = """Country[NL]: Netherlands
                       |  Airport[A1]: Test 1
                       |    Runway[R1] surface: GR
                       |  Airport[A2]: Test 2
                       |Country[TT]: Test""".stripMargin
      formatCountries(data) mustEqual expected
    }
  }

}
