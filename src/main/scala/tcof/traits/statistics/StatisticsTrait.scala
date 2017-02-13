package tcof.traits.statistics

import tcof.{LogicalBoolean, Model}
import tcof.traits.Trait
import org.apache.commons.math3.distribution.BinomialDistribution

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

trait StatisticsTrait extends Trait {
  this: Model =>

  val defaultBucketSize = 1
  val defaultTimeOffset = 0
  val defaultConfidence = 0.95

  class TimeSeriesOfBooleans(val timeOffset: Int, val bucketSize: Int, val trueCounts: ArrayBuffer[Int], val falseCounts: ArrayBuffer[Int]) {
    def this(timeOffset: Int, bucketSize: Int) = this(timeOffset, bucketSize, ArrayBuffer.empty[Int], ArrayBuffer.empty[Int])

    private def prepareBucket(time: Int): Int = {
      val bucket = (time - timeOffset) / bucketSize
      val dataSize = trueCounts.size

      if (dataSize <= bucket) {
        val zeros = new Array[Int](bucket - dataSize + 1)
        trueCounts.appendAll(zeros)
        falseCounts.appendAll(zeros)
      }

      bucket
    }

    def add(time: Int, value: Boolean): Unit = {
      val bucket = prepareBucket(time)

      if (value)
        trueCounts(bucket) = trueCounts(bucket) + 1
      else
        falseCounts(bucket) = falseCounts(bucket) + 1
    }

    def add(time: Int, trueCount: Int, falseCount: Int): Unit = {
      val bucket = prepareBucket(time)

      trueCounts(bucket) = trueCounts(bucket) + trueCount
      falseCounts(bucket) = falseCounts(bucket) + falseCount
    }
    
    def apply(timeLow: Int, timeHigh: Int): TimeSeriesOfBooleans = {
      require(timeLow >= timeOffset)
      require((timeLow - timeOffset) % bucketSize == 0)
      require((timeHigh - timeLow) % bucketSize == 0)

      val size = trueCounts.size
      val upperLimit = size * bucketSize + timeOffset


      if (timeLow < upperLimit) {
        val bucket = (timeLow - timeOffset) / bucketSize
        val bucketUntil = if (timeHigh < upperLimit) (timeHigh - timeOffset) / bucketSize + 1 else size

        new TimeSeriesOfBooleans(timeLow, bucketSize, trueCounts.slice(bucket, bucketUntil), falseCounts.slice(bucket, bucketUntil))
      } else {
        new TimeSeriesOfBooleans(timeLow, bucketSize)
      }
    }
  }

  object TestOperator extends Enumeration {
    type TestOperator = Value
    val LE = Value("<=")
    val GE = Value(">=")
    val EQ = Value("==")
    val LT = Value("<")
    val GT = Value(">")
  }

  import TestOperator._

  class BinomialTimeSeriesProbabilityTest(val ts: BinomialTimeSeries, val probability: Double, val operator: TestOperator) {
    def withConfidence(conf: Double): Boolean = {
      val trueCount = ts.data.trueCounts.sum
      val falseCount = ts.data.falseCounts.sum
      val totalCount = trueCount + falseCount

      val dist = new BinomialDistribution(totalCount, probability)

      val alpha = 1 - conf
      val cumulativeProb = dist.cumulativeProbability(trueCount)

      operator match {
        case EQ => cumulativeProb >= alpha / 2 && (1 - cumulativeProb) >= alpha / 2
        case GE => cumulativeProb >= alpha
        case LE => 1 - cumulativeProb >= alpha
        case GT => 1 - cumulativeProb < alpha
        case LT => cumulativeProb < alpha
      }
    }
  }

  implicit def binomialTimeSeriesProbabilityTestToBoolean(test: BinomialTimeSeriesProbabilityTest) = test.withConfidence(defaultConfidence)
  implicit def binomialTimeSeriesProbabilityTestToILogical(test: BinomialTimeSeriesProbabilityTest) = LogicalBoolean(test.withConfidence(defaultConfidence))

  class BinomialTimeSeriesProbability(val ts: BinomialTimeSeries) {
    def ===(prob: Double): BinomialTimeSeriesProbabilityTest = new BinomialTimeSeriesProbabilityTest(ts, prob, EQ)
    def >=(prob: Double): BinomialTimeSeriesProbabilityTest = new BinomialTimeSeriesProbabilityTest(ts, prob, GE)
    def <=(prob: Double): BinomialTimeSeriesProbabilityTest = new BinomialTimeSeriesProbabilityTest(ts, prob, LE)
    def >(prob: Double): BinomialTimeSeriesProbabilityTest = new BinomialTimeSeriesProbabilityTest(ts, prob, GT)
    def <(prob: Double): BinomialTimeSeriesProbabilityTest = new BinomialTimeSeriesProbabilityTest(ts, prob, LT)
  }

  class BinomialTimeSeries(private val trueIn: () => Int, private val falseIn: () => Int, timeOffset: Int, bucketSize: Int, val data: TimeSeriesOfBooleans) extends TimeSeries {
    def this(trueIn: () => Int, falseIn: () => Int, timeOffset: Int, bucketSize: Int) =
      this(trueIn, falseIn, timeOffset, bucketSize, new TimeSeriesOfBooleans(timeOffset, bucketSize))

    override def traitStep(): Unit = {
      data.add(time, trueIn(), falseIn())
    }

    def probability = new BinomialTimeSeriesProbability(this)
    
    def apply(timeLow: Int, timeHigh: Int): BinomialTimeSeries = {
      new BinomialTimeSeries(null, null, timeOffset, bucketSize, data(timeLow, timeHigh))
    }
  }

  trait TimeSeries {
    def traitStep(): Unit
  }

  object timeseries {
    val registeredTS = ListBuffer.empty[TimeSeries]

    def binomial(trueIn: => Int, falseIn: => Int, timeOffset: Int = defaultTimeOffset, bucketSize: Int = defaultBucketSize) = {
      val ts = new BinomialTimeSeries(trueIn _, falseIn _, timeOffset, bucketSize)
      registeredTS += ts
      ts
    }
  }

  override protected def traitStep(): Unit = {
    timeseries.registeredTS.foreach(_.traitStep())
  }

}

