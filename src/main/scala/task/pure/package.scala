package task

import scala.collection.immutable
import task.model._

//Here are only pure functions
//Exceptions are not counted because in the toy application they are not being caught so they serve as panic in case of incorrect input
package object pure {

  def calcStatistics(itr: Iterator[String]): Statistics =
    accToStatistics(
      gatherStatistics(
        stringsToMeasurements(itr)
      )
    )

  private[pure] def accToStatistics(statAcc: StatAcc) =
    Statistics(
      calcAvgAndSort(statAcc),
      statAcc.successfulCnt + statAcc.failedCnt,
      statAcc.failedCnt)

  private[pure] def calcAvgAndSort(statAcc: StatAcc) = {
    val successful = statAcc.successfulSensors.map { case (id, h) =>
      SensorStat(id, Some(HumidityStat(h.min, (h.sum / h.cnt).toInt, h.max)))
    }
    val successfulSorted = successful.toSeq.sortBy(_.h.get.avg).reverse
    val failed = (statAcc.failedSensors -- statAcc.successfulSensors.keys).map(id => SensorStat(id, None))
    successfulSorted ++ failed
  }

  private[pure] def gatherStatistics(measurements: Iterator[SensorMeasurement]) =
    measurements.foldLeft(
      StatAcc(
        immutable.HashMap.empty[String, HumidityAcc],
        immutable.HashSet.empty[String],
        0,
        0)
    )(handleMeasurement)


  private[pure] def handleMeasurement(acc: StatAcc, msmt: SensorMeasurement) =
    msmt.humidity match {
      case Some(h) => successfulToAcc(acc, msmt.id, h)
      case _ => failedToAcc(acc, msmt.id)
    }

  private[pure] def failedToAcc(acc: StatAcc, id: String) =
    acc.copy(
      failedSensors = acc.failedSensors + id,
      failedCnt = acc.failedCnt + 1)

  private[pure] def successfulToAcc(acc: StatAcc, id: String, h: Int) =
    acc.successfulSensors.get(id) match {
      case None => acc.copy(
        successfulSensors = acc.successfulSensors + (id -> HumidityAcc.initial(h)),
        successfulCnt = acc.successfulCnt + 1)
      case Some(humidityAcc) =>
        acc.copy(
          successfulSensors = acc.successfulSensors + (id -> handleHumidity(humidityAcc, h)),
          successfulCnt = acc.successfulCnt + 1)
    }

  private[pure] def handleHumidity(acc: HumidityAcc, h: Int) =
    acc.copy(
      min = math.min(acc.min, h),
      sum = acc.sum + h,
      cnt = acc.cnt + 1,
      max = math.max(acc.max, h))

  private[pure] def stringsToMeasurements(itr: Iterator[String]) =
    itr.map { v =>
      val name :: value :: Nil = v.split(",").map(_.trim).toList
      val resultValue = value match {
        case "NaN" => None
        case i => Some(i.toInt) //It will throw an exception thus interrupting application input is not correct. It is ok for a toy application only to throw a default exception without wrapping it in a more sensible one
      }
      SensorMeasurement(name, resultValue)
    }
}
