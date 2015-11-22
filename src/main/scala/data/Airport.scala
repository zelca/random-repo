package data

case class Airport(id: String, name: String, country: String, properties: Map[String, String] = Map())

object Airport {

  val fields = List("id", "name", "iso_country")

  def unapply(values: Map[String, String]) = fields.map(values.get) match {
    case Some(id) :: Some(name) :: Some(country) :: Nil =>
      Some(Airport(id, name, country, values.filter(_._2.nonEmpty) -- fields))
    case _ => None
  }

}

