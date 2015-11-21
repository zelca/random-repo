package core

import data.{Airport, Country, Runway}

object Query {

  type RunwayByAirport = Map[String, List[Runway]]

  type AirportByCountry = Map[String, List[Airport]]

  /** returns countries that match any of filters */
  def findByFilter(filters: (Country => Boolean)*)(implicit cc: List[Country], aa: AirportByCountry, rr: RunwayByAirport) = {
    val countries = filters.foldLeft((cc, List[Country]())) {
      case ((rest, result), filter) =>
        val part = rest.partition(filter)
        (part._2, result ++ part._1)
    }
    countries._2.map(c => (c, aa.getOrElse(c.code, Nil).map(a => (a, rr.getOrElse(a.id, Nil)))))
  }

  /** returns N top countries sorted by criteria */
  def topCountries[B](n: Int, by: Country => B, ordering: Ordering[B])(implicit cc: List[Country]): List[(Country, B)] =
    cc.map(c => (c, by(c))).sortBy(_._2)(ordering).take(n)

  /** returns N most common latitudes */
  def mostCommonLatitudes(n: Int)(implicit rr: RunwayByAirport): List[String] =
    rr.values.flatMap(_.map(_.latitude)).groupBy(x => x).toList.sortBy(_._2.size)(Ordering[Int].reverse).take(n).map(_._1)

  /** returns distinct surfaces per country for all countries */
  def distinctSurfaces(implicit cc: List[Country], aa: AirportByCountry, rr: RunwayByAirport): List[(Country, List[String])] =
    cc.map(c => (c, aa.getOrElse(c.code, Nil).flatMap(a => rr.getOrElse(a.id, Nil).map(_.surface)).distinct))

  /** criteria and filters */

  def fullMatch(text: String)(c: Country): Boolean =
    text.toLowerCase == c.code.toLowerCase || text.toLowerCase == c.name.toLowerCase

  def startsWith(text: String)(c: Country): Boolean =
    c.code.toLowerCase.startsWith(text.toLowerCase) || c.name.toLowerCase.startsWith(text.toLowerCase)

  def airportNumber(c: Country)(implicit aa: AirportByCountry): Int = aa.getOrElse(c.code, Nil).size

}
