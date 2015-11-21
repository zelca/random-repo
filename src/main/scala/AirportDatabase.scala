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
    write(" Welcome to Airport Database!")
    read(" [1] Query \n [2] Reports\n [3] Exit\n Make your choice [1,2,3]: ") {
      case "1" =>
        read(" Enter country code or name: ") {
          case criteria =>
            val result = findByFilter(fullMatch(criteria), startsWith(criteria)) match {
              case Nil => " 0 countries were found."
              case matches => formatCountries(matches)
            }
            write(result)
            false
        }
        true
      case "2" =>
        val top10 = topCountries(10, airportNumber, Ordering[Int].reverse)
        write("Top 10 by number of airports:\n\n" + formatAirportsNumber(top10))
        val bottom10 = topCountries(10, airportNumber, Ordering[Int])
        write("Bottom 10 by number of airports:\n\n" + formatAirportsNumber(bottom10))
        write("Surfaces by country:\n\n" + formatDistinctSurfaces(distinctSurfaces))
        write("Top 10 latitude: " + formatCommonLatitudes(mostCommonLatitudes(10)))
        true
      case "3" =>
        write(" Thank you. Come again.")
        false
      case _ =>
        write(" Invalid input. Try again.")
        true
    }
  }

}

object AirportDatabase extends App {

  (new AirportDatabase).menu

}
