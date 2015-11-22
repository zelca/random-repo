package core

import data.{Airport, Country, Runway}

object Formatter {

  val indent0 = "\n"

  val indent1 = "\n  "

  val indent2 = "\n    "

  def format(c: Country): String = "Country[%s]: %s".format(c.code, c.name)

  def format(a: Airport): String = "Airport[%s]: %s".format(a.id, a.name)

  def format(r: Runway): String = "Runway[%s] surface: %s".format(r.id, r.properties.getOrElse("surface", "unknown"))

  def formatCommonLatitudes(result: List[String]): String =
    indent0 + result.mkString(", ")

  def formatAirportsNumber(result: List[(Country, Int)]): String =
    result.map(c => indent0 + format(c._1) + " - " + c._2).mkString

  def formatDistinctSurfaces(result: List[(Country, List[String])]): String =
    result.map(c => indent0 + format(c._1) + " - " + c._2.mkString(", ")).mkString

  def formatCountries(result: List[(Country, List[(Airport, List[Runway])])]): String =
    result.map(c => indent0 + format(c._1) + formatAirports(c._2)).mkString

  def formatAirports(result: List[(Airport, List[Runway])]): String =
    result.map(a => indent1 + format(a._1) + formatRunways(a._2)).mkString

  def formatRunways(result: List[Runway]): String =
    result.map(r => indent2 + format(r)).mkString

}
