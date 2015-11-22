package data

import org.specs2.mutable.Specification

class RunwaySpec extends Specification {

  def fields = List("id", "airport_ref", "surface", "le_ident")

  "Runway" should {
    "be mapped" in {
      def map(v: String*) = (fields zip v).toMap

      val aa = List(map("1", "A1", "TURF", "H1"), map("3"), map("2", "A2", "TURF", "H1")) collect {
        case Runway(runway) => runway
      }
      val properties = Map("surface" -> "TURF", "le_ident" -> "H1")
      aa mustEqual List(Runway("1", "A1", properties), Runway("2", "A2", properties))
    }
  }

}
