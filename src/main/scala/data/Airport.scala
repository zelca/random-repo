package data

case class Airport(id: String, name: String, country: String)

object Airport {

  val Fields = List("id", "name", "iso_country")

  def unapply(values: Map[String, String]) = Fields.map(values.get) match {
    case Some(id) :: Some(name) :: Some(country) :: Nil => Some(Airport(id, name, country))
    case _ => None
  }

}

