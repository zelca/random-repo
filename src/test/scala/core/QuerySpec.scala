package core

import core.Query._
import data.{Airport, Country, Runway}
import org.specs2.mutable.Specification

class QuerySpec extends Specification {

  implicit val cc = List(Country("NL", "Netherlands"), Country("TT", "Test"))

  implicit val aa = Map("NL" -> List(Airport("A1", "Test 1", "NL"), Airport("A2", "Test 2", "NL")))

  implicit val rr = Map("A1" ->
    List(Runway("R1", "A1", "GR", "LB"), Runway("R2", "A1", "GR", "LA"), Runway("R3", "A1", "GR", "LB")))

  "fullMatch" should {
    "ignore case and match name" in {
      val testMatch = fullMatch("test") _
      testMatch(Country("TT", "Test")) mustEqual true
    }
    "match name" in {
      val testMatch = fullMatch("test") _
      testMatch(Country("TT", "test")) mustEqual true
    }
    "match code" in {
      val testMatch = fullMatch("test") _
      testMatch(Country("test", "222")) mustEqual true
    }
    "not match" in {
      val testMatch = fullMatch("test") _
      testMatch(Country("TT", "Testy")) mustEqual false
    }
    "not match" in {
      val testMatch = fullMatch("test") _
      testMatch(Country("TT", "Tes")) mustEqual false
    }
  }

  "startWith" should {
    "ignore case and match name" in {
      val testMatch = startsWith("test") _
      testMatch(Country("TT", "Test")) mustEqual true
    }
    "match code" in {
      val testMatch = startsWith("test") _
      testMatch(Country("testy", "222")) mustEqual true
    }
    "match name" in {
      val testMatch = startsWith("test") _
      testMatch(Country("TT", "Testy")) mustEqual true
    }
    "not match" in {
      val testMatch = startsWith("test") _
      testMatch(Country("TT", "Tes")) mustEqual false
    }
  }

  "airportNumber" should {
    "count 0 if no airports" in {
      airportNumber(Country("TT", "Test")) mustEqual 0
    }
    "return count of airports" in {
      airportNumber(Country("NL", "test")) mustEqual 2
    }
  }

  "distinctSurfaces" should {
    "return distinct surfaces" in {
      distinctSurfaces mustEqual (cc zip List(List("GR"), Nil))
    }
  }

  "mostCommonLatitudes" should {
    "return Nil if n ==0" in {
      mostCommonLatitudes(0) mustEqual Nil
    }
    "return latitudes" in {
      mostCommonLatitudes(5) mustEqual List("LB", "LA")
    }
  }

  "topCountries" should {
    "return Nil if n == 0" in {
      topCountries(0, airportNumber, Ordering[Int]) mustEqual Nil
    }
    "return lowest first" in {
      topCountries(5, airportNumber, Ordering[Int]) mustEqual (cc zip List(2, 0)).reverse
    }
    "return highest first" in {
      topCountries(5, airportNumber, Ordering[Int].reverse) mustEqual (cc zip List(2, 0))
    }
  }

  "findByFilter" should {
    "return filtered countries" in {
      val expected = cc.reverse.map(c => (c, aa.getOrElse(c.code, Nil).map(a => (a, rr.getOrElse(a.id, Nil)))))
      findByFilter(fullMatch("TT"), startsWith("Neth")) mustEqual expected
    }
    "return Nil if criteria don't match" in {
      findByFilter(fullMatch("NLL"), startsWith("A")) mustEqual Nil
    }
  }

}