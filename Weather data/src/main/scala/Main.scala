import scala.io.StdIn.readLine
import scala.io.Source
object Main {
  def main(args: Array[String]): Unit = {
try {

  val filename = "weather.dat"
  val Lines = Source.fromFile(filename).getLines().toList
  val weatherdata = Lines
    .drop(2)
    .filter(line => line.trim.nonEmpty && !line.trim.startsWith("mo"))
    .map{line =>
      val day = line.substring(0,4).trim.toInt
      val maxTemp = line.substring(6,10).trim.replaceAll("\\*","").toInt
      val minTemp = line.substring(12,16).trim.replaceAll("\\*","").toInt
      (day,maxTemp - minTemp)
    }
  val daywithsmallestpread = weatherdata.minBy(_._2)
  println(s"The day with the smallest temperaturespread is : day ${daywithsmallestpread._1}")
}

    catch {
      case e: Exception => println(s"An error has occcured ${e.getMessage}")
    }


  }
}