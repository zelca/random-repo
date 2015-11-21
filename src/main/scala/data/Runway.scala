package data

case class Runway(id: String, airport: String, surface: String, latitude: String)

object Runway {

  val Fields = List("id", "airport_ref", "surface", "le_ident")

  def unapply(values: Map[String, String]) = Fields.map(values.get) match {
    case Some(id) :: Some(airport) :: Some(surface) :: Some(latitude) :: Nil =>
      Some(Runway(id, airport, surface, latitude))
    case _ => None
  }

}

