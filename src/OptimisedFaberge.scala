import java.math.BigInteger
import java.math.BigInteger.{ZERO, ONE, valueOf}

import scala.annotation.tailrec

object OptimisedFaberge {
  var computations = Map[BigInteger, Map[BigInteger, BigInteger]]()

  def height(eggs: BigInteger, tries: BigInteger): BigInteger = {
    if (eggs == ZERO || tries == ZERO) ZERO
    else if (eggs == valueOf(1)) tries
    else {
      nuMaxReached(eggs, tries)
    }
  }

  @tailrec
  def nuMaxReached(eggs: BigInteger, tries: BigInteger, currentEggs: BigInteger = ONE, currentTries: BigInteger = ONE): BigInteger = {
    def addToComputations(eggs: BigInteger, tries: BigInteger, result: BigInteger) = {
      computations = computations + (eggs -> (computations.getOrElse(eggs, Map()) + (tries -> result)))
    }
    def currentMaxFloorNumber = if(currentEggs.subtract(ONE).compareTo(ZERO) == 0) ZERO
    else {
      computations(currentEggs.subtract(ONE))(currentTries.subtract(ONE))
        .add(computations(currentEggs)(currentTries.subtract(ONE)))
        .add(ONE)
    }

    if(currentEggs.compareTo(eggs) == 0 && currentTries.compareTo(tries) == 0 ) {
      currentMaxFloorNumber
    } else {
      if(currentEggs.compareTo(currentTries) == 0) {
        computations = Map(currentEggs.subtract(ONE) -> computations(currentEggs.subtract(ONE)))
        addToComputations(currentEggs, currentTries, valueOf(2).pow(currentEggs.intValue()).subtract(ONE))
        nuMaxReached(eggs, tries, currentEggs, currentTries.add(ONE))
      } else if(currentTries.compareTo(tries) < 0) {
        addToComputations(currentEggs, currentTries, currentMaxFloorNumber)
        nuMaxReached(eggs, tries, currentEggs, currentTries.add(ONE))
      } else if (currentTries.compareTo(tries) == 0) {
        addToComputations(currentEggs, currentTries, currentMaxFloorNumber)
        nuMaxReached(eggs, tries, currentEggs.add(ONE), currentEggs.add(ONE))
      } else {
        currentMaxFloorNumber
      }
    }
  }

  def main(args: Array[String]): Unit = {
    computations = computations + (ZERO -> Map(ZERO -> ZERO, ONE -> ZERO))
    computations = computations + (ONE -> Map(ZERO -> ZERO))

    println(height(valueOf(500), valueOf(512)))
  }
}
