import java.math.BigInteger
import java.math.BigInteger.{ZERO, ONE, valueOf}

object Faberge {

  def height(eggs: BigInteger, tries: BigInteger): BigInteger = {
    if(eggs == ZERO || tries == ZERO) ZERO
    else if(eggs == valueOf(1)) tries
    else{
      maxReachable(eggs, tries)
    }
  }

  def maxReachable(eggsLeft: BigInteger, triesLeft: BigInteger, highestUntilNow: BigInteger = ZERO): BigInteger = {
    if(eggsLeft.compareTo(triesLeft) == 0){
      valueOf(2).pow(eggsLeft.intValue()).subtract(ONE)
    } else if (eggsLeft.compareTo(ONE) > 0) {
        if (triesLeft.compareTo(ZERO) > 0) {

          val oneLessEggOneLessTry = maxReachable(eggsLeft.subtract(ONE), triesLeft.subtract(ONE), ZERO)
          val oneLessTry = maxReachable(eggsLeft, triesLeft.subtract(ONE), ZERO)

          oneLessEggOneLessTry
            .add(oneLessTry)
            .add(ONE)
        } else {
          highestUntilNow
        }
      } else {
        highestUntilNow.add(triesLeft)
      }
  }

  def main(args: Array[String]): Unit = {


    println(height(valueOf(4), valueOf(17)))
  }
}
