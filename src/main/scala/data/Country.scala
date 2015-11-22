package data

case class Country(id: String, code: String, name: String, properties: Map[String, String] = Map())

object Country {

  val fields = List("id", "code", "name")

  def unapply(values: Map[String, String]) = fields.map(values.get) match {
    case Some(id) :: Some(code) :: Some(name) :: Nil =>
      Some(Country(id, code, name, values.filter(_._2.nonEmpty) -- fields))
    case _ => None
  }

}
