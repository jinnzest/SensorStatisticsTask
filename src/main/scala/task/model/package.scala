package task

import scala.collection.immutable

package object model {

  case class SensorMeasurement(id: String, humidity: Option[Int])

  // Usage of BigInt is very questionable here.
  // But as the task description says nothing about precision and max possible amount of measurements per sensor I assumed that most precise is required.
  // Knowing more details allows us to optimize performance by using for example Double to gather sum and Int to hold count of measurements per sensor.
  case class HumidityAcc(min: Int, sum: BigInt, cnt: Long, max: Int)

  object HumidityAcc {
    def initial(h: Int) = HumidityAcc(h, BigInt(h), 1, h)
  }

  case class StatAcc(successfulSensors: immutable.HashMap[String, HumidityAcc], failedSensors: immutable.HashSet[String], successfulCnt: Long, failedCnt: Long)


  case class HumidityStat(min: Int, avg: Int, max: Int)

  case class SensorStat(id: String, h: Option[HumidityStat]) {
    override def toString: String = h match {
      case None => s"$id,NaN,NaN,NaN"
      case Some(HumidityStat(min, avg, max)) => s"$id,$min,$avg,$max"
    }
  }

  case class Statistics(sensors: Seq[SensorStat], measurementsCnt: Long, failedMeasurementsCnt: Long)

}
