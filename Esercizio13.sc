

import scala.io.Source

def wordFrequency(filename: String): List[(String, Int)] = {
  Source.fromFile(filename)
    .getLines
    .flatMap(_.split("[,;\\s]+"))
    .filter(_.nonEmpty)
    .toList
    .groupBy(identity)
    .mapValues(_.size)
    .toList
    .sortBy(-_._2)
}


println(wordFrequency("path to file"))
