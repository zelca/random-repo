package util

import java.io.InputStream
import java.util.{Map => JavaMap}

import com.fasterxml.jackson.databind.{MappingIterator, ObjectReader}
import com.fasterxml.jackson.dataformat.csv.{CsvMapper, CsvSchema}

import scala.collection.JavaConversions._

object Parser {

  def parse(is: InputStream): List[Map[String, String]] =
    managed(is) {
      source => stream(source).toList
    }

  private def managed[C <: AutoCloseable, T](closeable: C)(f: C => T) =
    try f(closeable) finally closeable.close()

  private def stream(is: InputStream): Iterator[Map[String, String]] = {
    val mapper = new CsvMapper()
    val schema = CsvSchema.emptySchema.withHeader
    val reader: ObjectReader = mapper.readerFor(classOf[JavaMap[_, _]])
    val iterator: MappingIterator[JavaMap[String, String]] = reader.`with`(schema).readValues(is)
    iterator.map(_.toMap)
  }

}
