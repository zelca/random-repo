import core.Formatter._
import core.Query._
import data.{Airport, Country, Runway}
import util.IO
import util.Parser._

class AirportDatabase extends IO {

  implicit val countries = {
    val is = getClass.getResourceAsStream("countries.csv")
    parse(is).collect {
      case Country(c) => c
    }
  }

  implicit val airports = {
    val is = getClass.getResourceAsStream("airports.csv")
    parse(is).collect {
      case Airport(a) => a
    }.groupBy(_.country).toMap
  }

  implicit val runways = {
    val is = getClass.getResourceAsStream("runways.csv")
    parse(is).collect {
      case Runway(r) => r
    }.groupBy(_.airport).toMap
  }

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
