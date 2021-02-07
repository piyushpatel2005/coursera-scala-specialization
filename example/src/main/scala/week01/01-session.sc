// Newton's method to calculate sqrt
// Guess    Quotient            Mean
// 1        2 / 1 = 2           (2+1)/2 = 1.5
// 1.5      2 / 1.5 = 1.333     1.4167
// 1.4167   2 / 1.4167 = 1.4118 1.4142

def abs(x: Double) = if (x < 0) -x else x
def sqrt(x: Double) = {
  def isGoodEnough(guess: Double) =
    abs(guess * guess - x) < 0.001

  // For very large and very small numbers, this isGoodEnough does not terminate
  // So, better version is given below.
  def isGoodEnough2(guess: Double) =
    abs(guess * guess - x) / x < 0.001

  def improve(guess: Double) =
    (guess + x / guess) / 2

  def sqrtIter(guess: Double): Double =
    if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess))

  sqrtIter(1.0)
}

sqrt(2)