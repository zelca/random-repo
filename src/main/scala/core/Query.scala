package core

import data.{Airport, Country, Runway}

object Query {

  type RunwayByAirport = Map[String, List[Runway]]

  type AirportByCountry = Map[String, List[Airport]]

  /** returns matches per filter */
  def findByFilter(filters: (Country => Boolean)*)(implicit cc: List[Country]): List[List[Country]] =
    filters.foldLeft((cc, List[List[Country]]())) {
      case ((rest, result), filter) =>
        rest.partition(filter) match {
          case (Nil, rr) => (rr, result)
          case (filtered, rr) => (rr, result ++ List(filtered))
        }
    }._2

  /** collects airports and runways data for list of countries */
  def countryData(countries: List[List[Country]])(implicit aa: AirportByCountry, rr: RunwayByAirport) = {
    def data(c: Country) =
      aa.getOrElse(c.code, Nil).map(a => (a, rr.getOrElse(a.id, Nil))).sortBy(_._2.size)(Ordering[Int].reverse)

    countries.flatMap(_.map(c => (c, data(c))).sortBy(_._2.size)(Ordering[Int].reverse))
  }

  /** returns countries sorted by criteria */
  def countriesBy[B](by: Country => B, ordering: Ordering[B])(implicit cc: List[Country]): List[(Country, B)] =
    cc.map(c => (c, by(c))).sortBy(_._2)(ordering)

  /** returns common values for specific field */
  def commonRunwayValues(field: String)(implicit rr: RunwayByAirport): List[String] = {
    val values = for (r <- rr.values.flatten; v <- r.properties.get(field)) yield v
    values.groupBy(x => x).toList.sortBy(_._2.size)(Ordering[Int].reverse).map(_._1)
  }

  /** returns distinct values for specific field per country for all countries */
  def distinctRunwayValues(field: String)(implicit cc: List[Country], aa: AirportByCountry, rr: RunwayByAirport) = {
    def values(c: Country) =
      for (a <- aa.getOrElse(c.code, Nil); r <- rr.getOrElse(a.id, Nil); v <- r.properties.get(field)) yield v

    cc.map(c => (c, values(c).distinct.sorted)).sortBy(_._2.size)(Ordering[Int].reverse)
  }

  /** criteria and filters */

  def airportNumber(c: Country)(implicit aa: AirportByCountry): Int =
    aa.getOrElse(c.code, Nil).size

  def fullMatch(text: String)(c: Country): Boolean =
    text.toLowerCase == c.code.toLowerCase || text.toLowerCase == c.name.toLowerCase

  def startsWith(text: String)(c: Country): Boolean =
    c.code.toLowerCase.startsWith(text.toLowerCase) || c.name.toLowerCase.startsWith(text.toLowerCase)

}
