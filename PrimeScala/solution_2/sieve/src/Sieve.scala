import Math.sqrt
import scala.annotation.tailrec
import scala.collection.parallel.ForkJoinTaskSupport
import scala.util.Success
import scala.util.Failure
import java.util.concurrent.Executors

final class Sieve(val size: Int) {
  // Allocate one extra element for sentinel value
  private[this] val bits: Array[Boolean] =
    new Array[Boolean]((size + 1) / 2 + 1)
  bits(bits.size - 1) = true

  private[this] val upperBound = sqrt(size).toInt + 1

  @tailrec
  def mainLoop(factor: Int): Unit = {
    if (factor < upperBound) {
      markMultiples(factor * factor, factor * 2)
      mainLoop(nextPrime(factor + 2))
    }
  }

  mainLoop(3)

  private[this] def markBit(index: Int): Unit = bits(index >> 1) = true

  private[this] def notPrime(k: Int): Boolean = bits(k >> 1)

  @tailrec
  def nextPrime(i: Int): Int = {
    if (notPrime(i)) nextPrime(i + 2) else i
  }

  @tailrec
  def markMultiples(i: Int, increment: Int): Unit = {
    if (i < size) {
      markBit(i)
      markMultiples(i + increment, increment)
    }
  }

  private[this] def run(): Unit = {}

  def primeCount: Int = bits.take(size / 2 + 1).count(_ == false)

  def isPrime(k: Int): Boolean = k == 2 || (k % 2 == 1 && !notPrime(k))

  def getPrimes = for (i <- 2 until size if isPrime(i)) yield i

}

object Sieve {
  val sieveSize = 1000000
  val runTimeMs: Long = 5000
  
  val warmUp = true
  val warmUpTimeMs: Long = 5000

  val validate = true

  def main(args: Array[String]): Unit = {
    if (warmUp) measurePasses(sieveSize, warmUpTimeMs)

    parallelVsSerial

    if (validate) validateResults
  }

  def parallelVsSerial = {
    import scala.collection.parallel.CollectionConverters._
    import scala.concurrent.{Await, Future}
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global

    println("Running parallel vs serial comparison")
    def largeTask() = {
      (0 until 5000).map{_ => new Sieve(1000000).primeCount}
    }

    // 100X smaller
    def smallTask() = {
      (0 until 5000).map{_ => new Sieve(10000).primeCount} 
    }

    def selectTask(i: Int, total_t0: Long) =  {
      val t0 = System.currentTimeMillis
      val k = if (i % 3 == 0) smallTask() else largeTask()
      val t = System.currentTimeMillis
      println(s"Task took: ${t - t0} (total: ${t - total_t0})")
      k
    }

    val runCount = 20
    println("\nSerial run:")
    val t0Serial = System.currentTimeMillis
    val foundCountsSer = (0 until runCount).map { i => 
      selectTask(i, t0Serial)
    }
    println(s"Running tasks serially took ${System.currentTimeMillis - t0Serial} ms")

    val t0Parallel = System.currentTimeMillis

    
    println(s"\nParallel run:")

    (0 until runCount).par.foreach{ i =>
      selectTask(i, t0Parallel)
    }

    println(s"Running tasks parallel took ${System.currentTimeMillis - t0Parallel} ms")

  
  }

  def measurePasses(sieveSize: Int, runTimeMs: Long): (Int, Double) = {
    var passes = 0
    val t0 = System.currentTimeMillis

    while (System.currentTimeMillis() - t0 < runTimeMs) {
      val sieve = new Sieve(sieveSize)
      passes += 1
    }

    val dt = System.currentTimeMillis() - t0
    (passes, dt / 1000.0)
  }

  def validateResults = {
    val primeCounts: Map[Int, Int] = Map(
      10 -> 4,
      100 -> 25,
      1000 -> 168,
      10000 -> 1229,
      100000 -> 9592,
      1000000 -> 78498,
      10000000 -> 664579,
      100000000 -> 5761455
    )

    val sieve = new Sieve(200)
    println(sieve.getPrimes.mkString(s"Primes up to ${sieve.size}: ", ", ", ""))

    for ((size, expectedCount) <- primeCounts) {
      val sieve = new Sieve(size)
      val count = sieve.primeCount
      assert(
        count == expectedCount,
        "Found prime count does not match expected count"
      )
      println(
        s"${sieve.size}: Found prime count ($count) matches with expected ($expectedCount)"
      )
    }
  }



}
