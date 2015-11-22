package core

import core.Formatter._
import data.{Airport, Country, Runway}
import org.specs2.mutable.Specification

class FormatterSpec extends Specification {

  def cc = List(Country("1", "NL", "Netherlands", Map()), Country("2", "TT", "Test", Map()))

  def aa = Map("NL" -> List(Airport("A1", "Test 1", "NL"), Airport("A2", "Test 2", "NL")))

  def rr = Map("A1" -> List(Runway("R1", "A1", Map("surface" -> "GR", "ie_ident" -> "L1"))))

  "format" should {
    "format Country" in {
      format(Country("1", "NL", "Netherlands", Map())) mustEqual "Country[NL]: Netherlands"
    }
    "format Airport" in {
      format(Airport("A1", "Test Airport", "NL")) mustEqual "Airport[A1]: Test Airport"
    }
    "format Runway" in {
      format(rr.values.head.head) mustEqual "Runway[R1] surface: GR"
    }
  }

  "formatDistinctSurfaces" should {
    "format" in {
      val data = cc.map(c => (c, List("A", "B")))
      formatDistinctSurfaces(data) mustEqual "\nCountry[NL]: Netherlands - A, B\nCountry[TT]: Test - A, B"
    }
  }

  "formatCommonLatitudes" should {
    "format" in {
      formatCommonLatitudes(List("LB", "LA")) mustEqual "\nLB, LA"
    }
  }

  "formatAirportsNumber" should {
    "format" in {
      val data = cc zip List(1, 2)
      formatAirportsNumber(data) mustEqual "\nCountry[NL]: Netherlands - 1\nCountry[TT]: Test - 2"
    }
  }

  "formatCountries" should {
    "format" in {
      val data = cc.map(c => (c, aa.getOrElse(c.code, Nil).map(a => (a, rr.getOrElse(a.id, Nil)))))
      val expected = """
                       |Country[NL]: Netherlands
                       |  Airport[A1]: Test 1
                       |    Runway[R1] surface: GR
                       |  Airport[A2]: Test 2
                       |Country[TT]: Test""".stripMargin
      formatCountries(data) mustEqual expected
    }
  }

  "formatAirports" should {
    "format" in {
      val data = aa("NL").map(a => (a, rr.getOrElse(a.id, Nil)))
      val expected = """
                       |  Airport[A1]: Test 1
                       |    Runway[R1] surface: GR
                       |  Airport[A2]: Test 2""".stripMargin
      formatAirports(data) mustEqual expected
    }
  }

  "formatRunways" should {
    "format" in {
      val expected = "\n    Runway[R1] surface: GR"
      formatRunways(rr("A1")) mustEqual expected
    }
  }

}
