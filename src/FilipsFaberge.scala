import java.math.BigInteger
import java.math.BigInteger.{ONE, ZERO, valueOf}

import com.google.common.base.Stopwatch

import scala.annotation.tailrec

object FilipsFaberge {


  def height(eggs: BigInteger, tries: BigInteger): BigInteger = eggs match {
    case _ if eggs == ZERO || tries == ZERO             => ZERO
    case _ if tries == ONE                              => ONE
    case _ if tries == eggs                             => valueOf(2).pow(tries.intValue()).subtract(ONE)
    case _                                              =>
      valueOf(2).pow(tries.intValue()).subtract(ONE)
        .subtract( Σ(tries.subtract(eggs).subtract(ONE))( x => fac(tries, tries.subtract(valueOf(x))) divide fac(valueOf(x))))
  }


  def Σ(topMargin: BigInteger)(f: Int => BigInteger): BigInteger =
    (0 to topMargin.intValue())
      .map(f)
      .foldLeft(ZERO)(_ add _)

  @tailrec
  def fac(ground: BigInteger, to: BigInteger = ONE, prodUntilNow: BigInteger = ONE) : BigInteger = ground match {
    case number:BigInteger if number.compareTo(ZERO) < -1 => throw new NumberFormatException("ground has to be greater than zero")
    case ZERO                            => prodUntilNow
    case x if x == to                    => prodUntilNow
    case _                               => fac(ground.subtract(ONE), to, ground.multiply(prodUntilNow))
  }

  def main (args: Array[String]): Unit = {
    val watch = new Stopwatch
    watch.start()
    println(height(valueOf(19900), valueOf(20000)))
    watch.stop()
    println(s"Berekening duurde ca. ${watch.elapsedMillis()} ms")
  }
}
