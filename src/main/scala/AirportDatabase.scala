
import core.Formatter._
import core.Query._
import data.{Airport, Country, Runway}
import util.IO
import util.Parser._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._

class AirportDatabase extends IO {

  val countriesLoader: Future[List[Country]] = Future {
    val is = getClass.getResourceAsStream("countries.csv")
    parse(is).collect {
      case Country(c) => c
    }
  }

  val airportsLoader: Future[Map[String, List[Airport]]] = Future {
    val is = getClass.getResourceAsStream("airports.csv")
    parse(is).collect {
      case Airport(a) => a
    }.groupBy(_.country)
  }

  val runwaysLoader: Future[Map[String, List[Runway]]] = Future {
    val is = getClass.getResourceAsStream("runways.csv")
    parse(is).collect {
      case Runway(r) => r
    }.groupBy(_.airport)
  }

  implicit lazy val countries: List[Country] = Await.result(countriesLoader, 1 minute)
  implicit lazy val airports: Map[String, List[Airport]] = Await.result(airportsLoader, 1 minute)
  implicit lazy val runways: Map[String, List[Runway]] = Await.result(runwaysLoader, 1 minute)

  def menu = {
    output(" Welcome to Airport Database!")
    input(" [1] Query \n [2] Reports\n [3] Exit\n Make your choice [1,2,3]: ") {
      case "1" =>
        input(" Enter country code or name: ") {
          case "" =>
            output("Invalid input. Try again.")
            true
          case filter =>
            val result = findByFilter(fullMatch(filter), startsWith(filter)) match {
              case Nil => " 0 countries were found."
              case matches => formatCountries(countryData(matches))
            }
            output(result)
            false
        }
        true
      case "2" =>
        output("Top 10 by number of airports:")
        output(formatAirportsNumber(countriesBy(airportNumber, Ordering[Int].reverse).take(10)))

        output("Bottom 10 by number of airports:")
        output(formatAirportsNumber(countriesBy(airportNumber, Ordering[Int]).take(10)))

        output("Surfaces by country:")
        output(formatDistinctSurfaces(distinctRunwayValues("surface")))

        output("Top 10 latitudes:")
        output(formatCommonLatitudes(commonRunwayValues("le_ident").take(10)))
        true
      case "3" =>
        output(" Thank you. Come again.")
        false
      case _ =>
        output(" Invalid input. Try again.")
        true
    }
  }

}

object AirportDatabase extends App {

  (new AirportDatabase).menu

}
