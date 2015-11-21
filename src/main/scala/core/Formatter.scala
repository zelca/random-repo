package core

import data.{Airport, Country, Runway}

object Formatter {

  val indent1 = "\n  "

  val indent2 = "\n    "

  def format(c: Country): String = "Country[%s]: %s".format(c.code, c.name)

  def format(a: Airport): String = "Airport[%s]: %s".format(a.id, a.name)

  def format(r: Runway): String = "Runway[%s] surface: %s".format(r.id, r.surface)

  def formatCountries(result: List[(Country, List[(Airport, List[Runway])])]): String =
    result.map {
      case c =>
        format(c._1) + (c._2.sortBy(_._2.size)(Ordering[Int].reverse) map {
          case a => indent1 + format(a._1) + a._2.map(r => indent2 + format(r)).mkString
        }).mkString
    } mkString "\n"

  def formatCommonLatitudes(result: List[String]): String =
    result.mkString(", ")

  def formatAirportsNumber(result: List[(Country, Any)]): String =
    result.map(c => format(c._1) + " - " + c._2) mkString "\n"

  def formatDistinctSurfaces(result: List[(Country, List[String])]): String =
    result.sortBy(_._2.size)(Ordering[Int].reverse).map {
      case c => format(c._1) + " - " + c._2.sorted.mkString(", ")
    } mkString "\n"

}
