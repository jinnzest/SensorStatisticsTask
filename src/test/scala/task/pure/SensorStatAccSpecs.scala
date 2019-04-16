package task.pure

import org.scalatest.{FreeSpec, Matchers}

import scala.collection.immutable.{HashMap, HashSet}
import task.model._

class SensorStatAccSpecs extends FreeSpec with Matchers {

  trait InitialStatAccFixture {
    val acc = StatAcc(HashMap.empty[String, HumidityAcc], HashSet.empty[String], 0, 0)
  }

  trait IntermediateStatAccFixture {
    val humidityAcc = HumidityAcc(10, 5, 2, 15)
    val acc = StatAcc(HashMap("acc_id" -> humidityAcc), HashSet("acc_failed_id"), 1, 1)
  }

  trait HumidityAccFixture {
    val acc = HumidityAcc(10, 5, 2, 15)
  }

  "pure functions" - {
    "updateHumidityAcc" - {
      "smaller humidity should update only min, sum and cnt" in new HumidityAccFixture {
        val h = 5

        val res = handleHumidity(acc, h)

        res.min shouldBe h
        res.sum shouldBe (acc.sum + h)
        res.cnt shouldBe acc.cnt + 1
        res.max shouldBe acc.max
      }
      "bigger humidity should update only max, sum and cnt" in new HumidityAccFixture {
        val h = 20

        val res = handleHumidity(acc, h)

        res.min shouldBe acc.min
        res.sum shouldBe (acc.sum + h)
        res.cnt shouldBe acc.cnt + 1
        res.max shouldBe h
      }
      "humidity which is bigger than min but smaller than max should update only sum and cnt" in new HumidityAccFixture {
        val h = 12

        val res = handleHumidity(acc, h)

        res.min shouldBe acc.min
        res.sum shouldBe (acc.sum + h)
        res.cnt shouldBe acc.cnt + 1
        res.max shouldBe acc.max
      }
      "updateStatAcc" - {
        "initial acc" - {
          "should add id to failed sensors and increment failed count" in new InitialStatAccFixture {
            val someId = "some_id"
            val failedMsmt = SensorMeasurement(someId, None)

            val res = handleMeasurement(acc, failedMsmt)

            res.failedSensors should contain(someId)
            res.failedCnt shouldBe 1
            res.successfulSensors shouldBe empty
            res.successfulCnt shouldBe 0
          }
          "should add id and humidity to successful sensors and increment successful ones count" in new InitialStatAccFixture {
            val someId = "some_id"
            val humidity = 1
            val successfulMsmg = SensorMeasurement(someId, Some(humidity))

            val res = handleMeasurement(acc, successfulMsmg)

            res.failedSensors shouldBe empty
            res.failedCnt shouldBe 0
            res.successfulSensors should contain(someId -> HumidityAcc(humidity, BigInt(humidity), 1, humidity))
            res.successfulCnt shouldBe 1
          }
        }
        "intermediate acc" - {
          "unique new measurement" - {
            "should add id to failed sensors and increment failed count" in new IntermediateStatAccFixture {
              val someId = "some_id"
              val failedMsmt = SensorMeasurement(someId, None)

              val res = handleMeasurement(acc, failedMsmt)

              res.failedSensors should contain(someId)
              res.failedCnt shouldBe acc.failedCnt + 1
              res.successfulSensors shouldBe acc.successfulSensors
              res.successfulCnt shouldBe acc.successfulCnt
            }
            "should add id and humidity to successful sensors and increment successful ones count" in new IntermediateStatAccFixture {
              val someId = "some_id"
              val humidity = 1
              val successfulMsmg = SensorMeasurement(someId, Some(humidity))

              val res = handleMeasurement(acc, successfulMsmg)

              res.failedSensors shouldBe acc.failedSensors
              res.failedCnt shouldBe acc.failedCnt
              res.successfulSensors should contain(someId -> HumidityAcc(humidity, BigInt(humidity), 1, humidity))
              res.successfulCnt shouldBe acc.successfulCnt + 1
            }
          }
          "next measurement of an existing sensor" - {
            "should add id to failed sensors and increment failed count" in new IntermediateStatAccFixture {
              val someId = "acc_failed_id"
              val failedMsmt = SensorMeasurement(someId, None)

              val res = handleMeasurement(acc, failedMsmt)

              res.failedSensors should contain(someId)
              res.failedCnt shouldBe acc.failedCnt + 1
              res.successfulSensors shouldBe acc.successfulSensors
              res.successfulCnt shouldBe acc.successfulCnt
            }
            "should add id and humidity to successful sensors and increment successful ones count" in new IntermediateStatAccFixture {
              val someId = "acc_id"
              val humidity = 1
              val successfulMsmg = SensorMeasurement(someId, Some(humidity))

              val res = handleMeasurement(acc, successfulMsmg)

              res.failedSensors shouldBe acc.failedSensors
              res.failedCnt shouldBe acc.failedCnt
              res.successfulSensors should contain(someId -> humidityAcc.copy(min = humidity, sum = humidityAcc.sum + humidity, cnt = humidityAcc.cnt + 1))
              res.successfulCnt shouldBe acc.successfulCnt + 1
            }
          }
        }
      }
      "accToStatistics" - {
        "should be sorted by avg and failed at the end" in {
          val someIdBiggerAvg = "some_id_bigger_avg"
          val someIdSmallerAvg = "some_id_smaller_avg"
          val failedId = "failed_id"
          val biggerAvgHumidity = HumidityAcc(1, BigInt(3), 1, 4)
          val smallerAvgHumidity = HumidityAcc(1, BigInt(10), 5, 4)
          val failedCnt = 1
          val successfulCnt = 1
          val statAcc = StatAcc(HashMap(someIdBiggerAvg -> biggerAvgHumidity, someIdSmallerAvg -> smallerAvgHumidity), HashSet(failedId), successfulCnt, failedCnt)

          val res = accToStatistics(statAcc)

          res.sensors.size shouldBe 3
          res.failedMeasurementsCnt shouldBe failedCnt
          res.measurementsCnt shouldBe failedCnt + successfulCnt

          val biggerAvg :: smallerAvg :: failedOne :: Nil = res.sensors
          biggerAvg.id shouldBe someIdBiggerAvg
          smallerAvg.id shouldBe someIdSmallerAvg
          failedOne.id shouldBe failedId

          biggerAvg.h.get.avg shouldBe biggerAvgHumidity.sum
          smallerAvg.h.get.avg shouldBe smallerAvgHumidity.sum / smallerAvgHumidity.cnt

          biggerAvg.h.get.min shouldBe biggerAvgHumidity.min
          biggerAvg.h.get.max shouldBe biggerAvgHumidity.max

          smallerAvg.h.get.min shouldBe smallerAvgHumidity.min
          smallerAvg.h.get.max shouldBe smallerAvgHumidity.max
        }
      }
      "linesToMeasurements" - {
        "extract id and value from each line" in {
          val lines = "  id1,23" :: " id2,43 " :: "id1 ,2" :: "id2,  NaN" :: Nil

          val msmt1 :: msmt2 :: msmt3 :: msmt4 :: Nil = stringsToMeasurements(lines.toIterator).toList

          msmt1.id shouldBe "id1"
          msmt1.humidity.get shouldBe 23

          msmt2.id shouldBe "id2"
          msmt2.humidity.get shouldBe 43

          msmt3.id shouldBe "id1"
          msmt3.humidity.get shouldBe 2

          msmt4.id shouldBe "id2"
          msmt4.humidity shouldBe empty
        }
      }
    }
  }

}
