package task

import java.io.File

import scala.io.Source
import task.pure._
import task.model._

object SensorStatisticsTask {

  private def printResult(stats: Statistics, filesAmount: Int): Unit = {
    println(s"Num of processed files: $filesAmount")
    println(s"Num of processed measurements: ${stats.measurementsCnt}")
    println(s"Num of failed measurements: ${stats.failedMeasurementsCnt}")
    println()
    println("Sensors with highest avg humidity:")
    println()
    println("sensor-id,min,avg,max")
    stats.sensors.foreach(println)
  }

  private def getFilesOfDir(dirName: String) = {
    val dir = new File(dirName)
    if (dir.exists && dir.isDirectory)
      dir.listFiles.filter(_.isFile).toList
    else
      List[File]()
  }

  def proceedDir(dirName: String): Unit = {
    val files = getFilesOfDir(dirName)
    //here the opened source will be not closed on an exception but it doesn't matter for such toy application
    val iterators = files.map(f => Source.fromFile(f).getLines).map { i =>
      i.next //skip a header line. to simplify code of the toy task it is assumed that first column is always id and the second one is humidity
      i
    }
    val mergedIterators = iterators.foldLeft(Iterator[String]())(_ ++ _)
    val stats = calcStatistics(mergedIterators)
    printResult(stats, files.size)
  }

  def main(args: Array[String]): Unit =
    args match {
      case Array(dirName, _*) => proceedDir(dirName)
      case _ => println("Directory name as a single argument is expected")
    }
}
