package core

import core.Query._
import data.{Airport, Country, Runway}
import org.specs2.mutable.Specification

class QuerySpec extends Specification {

  def cc = List(Country("1", "NL", "Netherlands"), Country("2", "TT", "Test"))

  def aa = Map("NL" -> List(Airport("A1", "Test 1", "NL"), Airport("A2", "Test 2", "NL")))

  def rr = Map("A1" -> List(Runway("R1", "A1", Map("surface" -> "GR", "le_ident" -> "LB")),
    Runway("R2", "A1", Map("surface" -> "GR", "le_ident" -> "LA")),
    Runway("R3", "A1", Map("surface" -> "GR", "le_ident" -> "LB"))))

  "fullMatch" should {
    "ignore case and match name" in {
      val testMatch = fullMatch("test") _
      testMatch(Country("1", "TT", "Test")) mustEqual true
    }
    "match name" in {
      val testMatch = fullMatch("test") _
      testMatch(Country("1", "TT", "test")) mustEqual true
    }
    "match code" in {
      val testMatch = fullMatch("test") _
      testMatch(Country("1", "test", "222")) mustEqual true
    }
    "not match" in {
      val testMatch = fullMatch("test") _
      testMatch(Country("1", "TT", "Testy")) mustEqual false
    }
    "not match" in {
      val testMatch = fullMatch("test") _
      testMatch(Country("1", "TT", "Tes")) mustEqual false
    }
  }

  "startWith" should {
    "ignore case and match name" in {
      val testMatch = startsWith("test") _
      testMatch(Country("1", "TT", "Test")) mustEqual true
    }
    "match code" in {
      val testMatch = startsWith("test") _
      testMatch(Country("1", "testy", "222")) mustEqual true
    }
    "match name" in {
      val testMatch = startsWith("test") _
      testMatch(Country("1", "TT", "Testy")) mustEqual true
    }
    "not match" in {
      val testMatch = startsWith("test") _
      testMatch(Country("1", "TT", "Tes")) mustEqual false
    }
  }

  "airportNumber" should {
    "count 0 if no airports" in {
      airportNumber(Country("1", "TT", "Test"))(aa) mustEqual 0
    }
    "return count of airports" in {
      airportNumber(Country("1", "NL", "test"))(aa) mustEqual 2
    }
  }

  "distinctSurfaces" should {
    "return distinct surfaces" in {
      distinctRunwayValues("surface")(cc, aa, rr) mustEqual (cc zip List(List("GR"), Nil))
    }
  }

  "mostCommonLatitudes" should {
    "return latitudes" in {
      commonRunwayValues("le_ident")(rr) mustEqual List("LB", "LA")
    }
  }

  "topCountries" should {
    "return lowest first" in {
      countriesBy(airportNumber(_)(aa), Ordering[Int])(cc) mustEqual (cc zip List(2, 0)).reverse
    }
    "return highest first" in {
      countriesBy(airportNumber(_)(aa), Ordering[Int].reverse)(cc) mustEqual (cc zip List(2, 0))
    }
  }

  "findByFilter" should {
    "return filtered countries" in {
      val expected = List(List(Country("2", "TT", "Test")), List(Country("1", "NL", "Netherlands")))
      findByFilter(fullMatch("TT"), startsWith("Neth"))(cc) mustEqual expected
    }
    "return empty if filter don't match" in {
      findByFilter(fullMatch("NLL"), startsWith("A"))(cc) mustEqual Nil
    }
  }

  "countryData" should {
    "return filtered countries" in {
      val input = List(List(Country("2", "TT", "Test")), List(Country("1", "NL", "Netherlands")))
      val expected = cc.reverse.map(c => (c, aa.getOrElse(c.code, Nil).map(a => (a, rr.getOrElse(a.id, Nil)))))
      countryData(input)(aa, rr) mustEqual expected
    }
    "return empty for empty" in {
      countryData(Nil)(aa, rr) mustEqual Nil
    }
  }


}
