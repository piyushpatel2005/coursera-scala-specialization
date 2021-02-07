import math.abs

val tolerance = 0.0001

def isCloseEnough(x: Double, y: Double) =
  abs((x - y) / x) / x < tolerance

def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  def iterate(guess: Double): Double = {
    val next = f(guess)
    if (isCloseEnough(guess, next)) next
    else iterate(next)
  }
  iterate(firstGuess)
}

fixedPoint(x => 1 + x / 2)(1)

/**
 * If we want to find sqrt using this fixedPoint function, we can use it.
 * Square root for x is defined as number `y` such that `y * y = x`.
 * That means it's a function `y = x / y`
 */
def sqrt(x: Double) = fixedPoint(y => x / y)(1)

/**
 * With above method if we try to find `sqrt(2)`, the value keeps oscillating between 1 and 2.
 * One way to converge is to average those values.
*/
def sqrt(x: Double) = fixedPoint(y => (y + x / y) / 2)(1)
sqrt(2)

/**
 * Given `averageDamp` to dampen the output, write `sqrt` function which uses both functions.
 */
def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

def sqrt (x: Double) =
  fixedPoint(averageDamp(y => x / y))(1)