import jdk.internal.util.xml.impl.Input

import scala.io.Source
import scala.util.Try

object Main {
  def main(args: Array[String]): Unit = {
    val filename = "football.dat"
    def isNumeric(input: String) : Boolean =Try(input.toInt).isSuccess

    try {
      val Lines = Source.fromFile(filename).getLines().toList
      val teamstats = Lines
        .drop(1)
        .filterNot(line => line.trim.isEmpty || line.trim.startsWith("---"))
        .map{ Line =>


          val teamName = Line.substring(7, 22).trim
          val goaslForString = Line.substring(43, 45).trim
          val goalsAgainistString = Line.substring(50, 52).trim

          if(isNumeric(goaslForString) && isNumeric(goalsAgainistString)){
            val goalsFor = goaslForString.toInt
            val goalsAgainist = goalsAgainistString.toInt
            Some((teamName, Math.abs(goalsFor - goalsAgainist)))

          } else None
        }
        .flatten


      val TeamwithsmallestGoalDifference = teamstats.minBy(_._2)
      println(s"The team with the smallest goal difference is : ${TeamwithsmallestGoalDifference._1}")
    }

    catch {
      case e:Exception => println(s"An error has occured : ${e.getMessage}")
    }
  }
}