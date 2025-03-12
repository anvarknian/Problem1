import java.util.Scanner

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

/**
  * @author Anwar Knyane
  */


object Main extends App {

  print("-- \t Prime Division \t -- " +
    "\n\n\nType a number:")

  val numberInput = Try(new Scanner(System.in).nextLong())

  numberInput match {
    case Success(a) => print(s"Finding the divider of $a: ${factors(a)}")
    case Failure(ex) => print("Incorrect type, please enter a positive Long")
  }

  def factors(n: Long): List[Long] = {

    // Prime divider - 2, 3, 5, 7, 11, 13, 17, 19, ...
    // We need to find all the prime number smaller than 'a'

    assert(n >= 1, "Argument must be positive")

    @tailrec
    def primeNumberDivisor(num: Long, list: List[Long]): List[Long] = {
      if (num == 1) list
      else {
        val valueAdd = callRecursionFunction(num)
        primeNumberDivisor(num / valueAdd, valueAdd :: list)
      }
    }

    primeNumberDivisor(n, List()).reverse
  }

  private def callRecursionFunction(n: Long) = recursionFunction(2, n)

  @tailrec
  private def recursionFunction(firstPrimeNumber: Long, target: Long): Long = {
    if (target % firstPrimeNumber == 0) firstPrimeNumber
    else if (firstPrimeNumber * firstPrimeNumber > target) target
    else recursionFunction(firstPrimeNumber + 1, target)
  }
}
