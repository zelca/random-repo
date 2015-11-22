package data

case class Runway(id: String, airport: String, properties: Map[String, String] = Map())

object Runway {

  val fields = List("id", "airport_ref")

  def unapply(values: Map[String, String]) = fields.map(values.get) match {
    case Some(id) :: Some(airport) :: Nil =>
      Some(Runway(id, airport, values.filter(_._2.nonEmpty) -- fields))
    case _ => None
  }

}

