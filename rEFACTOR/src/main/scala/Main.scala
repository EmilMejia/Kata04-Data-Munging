
import scala.io.Source
import scala.util.Try

object DataProcessor {
  type DataRow = Map[String, String]

  def readFile(filename: String, skipLines: Int): List[String] = {
    Source.fromFile(filename).getLines().drop(skipLines).filterNot(line => line.trim.isEmpty || line.trim.startsWith("mo")  || line.trim.contains("-")) // Excluye líneas vacías y líneas que empiezan con "mo".
      .toList
  }

  def parseLine(line: String, schema: List[(String, Int, Int)]): Option[DataRow] = {
    Try {
      schema.foldLeft(Map.empty[String, String]) { (acc, col) =>
        val (name, start, end) = col
        acc + (name -> line.substring(start, end).trim)
      }
    }.toOption
  }

  def isNumeric(input: String): Boolean = Try(input.toInt).isSuccess

  def extractValidRows(lines: List[String], schema: List[(String, Int, Int)]): List[DataRow] = {
    lines.map(parseLine(_, schema)).collect { case Some(dataRow) => dataRow }
  }
}

object WeatherData extends App {
  try {
    val filename = "weather.dat"
    val schema = List(("day", 0, 4), ("maxTemp", 6, 10), ("minTemp", 12, 16))

    val lines = DataProcessor.readFile(filename, 2)
    val weatherData: List[(Int, Int)] = DataProcessor.extractValidRows(lines, schema)
      .map { dataRow =>
        val day = dataRow("day").toInt
        val maxTemp = dataRow("maxTemp").filter(_.isDigit).toInt // make sure the string can be converted to Int
        val minTemp = dataRow("minTemp").filter(_.isDigit).toInt // make sure the string can be converted to Int
        (day, maxTemp - minTemp)
      }


    val dayWithSmallestSpread = weatherData.minBy(_._2)
    println(s"The day with the smallest temperature spread is: Day ${dayWithSmallestSpread._1}")
  }
  catch {
    case e: Exception => println(s"An error occurred: ${e.getMessage}")
  }
}

object FootballData extends App {
  try {

    val filename = "football.dat"
    val schema = List(("teamName", 7, 22), ("goalsFor", 43, 45), ("goalsAgainst", 50, 52))

    val lines = DataProcessor.readFile(filename, 1)
    val teamStats = DataProcessor.extractValidRows(lines, schema)
      .map { dataRow =>
        val teamName = dataRow("teamName")
        val goalsForString = dataRow("goalsFor").trim
        val goalsAgainstString = dataRow("goalsAgainst").trim

        // Convert to Int only if the string is numeric, otherwise use 0 as a default
        val goalsFor = if (goalsForString.forall(_.isDigit)) goalsForString.toInt else 0
        val goalsAgainst = if (goalsAgainstString.forall(_.isDigit)) goalsAgainstString.toInt else 0

        (teamName, Math.abs(goalsFor - goalsAgainst))
      }


    val teamWithSmallestGoalDiff = teamStats.minBy(_._2)
    println(s"The team with the smallest goal difference is: ${teamWithSmallestGoalDiff._1}")
  }
  catch {
    case e: Exception => println(s"An error occurred: ${e.getMessage}")


  }
}



