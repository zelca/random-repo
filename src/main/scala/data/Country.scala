package data

case class Country(code: String, name: String)

object Country {

  val Fields = List("code", "name")

  def unapply(values: Map[String, String]) = Fields.map(values.get) match {
    case Some(code) :: Some(name) :: Nil => Some(Country(code, name))
    case _ => None
  }

}
