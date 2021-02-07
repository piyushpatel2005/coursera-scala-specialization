// Commong functions
//
def sum(f: Int => Int, a: Int, b: Int): Int =
  if (a > b) 0
  else f(a) + sum(f, a + 1, b)

def id(x: Int): Int = x

def cube(x: Int): Int = x * x * x

def fact(n: Int): Int = {
  def loop(acc: Int, n: Int): Int =
    if (n == 0) acc
    else loop(acc * n, n - 1)
  loop(1, n)
}

// Currying
def sumInts(a: Int, b: Int) = sum(x => x, a, b)
def sumCubes(a: Int, b: Int): Int = sum(x => x * x * x, a, b)
def sumFactorials(a:Int, b: Int): Int = sum(fact, a, b)

/**
 * In above code, we still see repetition by passing the same kinds of parameters in all functions.
 * In below function, the function takes two parameters and returns a new function.
 */

def sum(f: Int => Int): (Int, Int) => Int = {
  def sumF(a: Int, b: Int): Int =
    if (a > b) 0
    else f(a) + sumF(a + 1, b)
  sumF
}

// Now we can define new sumInts, sumCubes and sumFactorials
def sumInts = sum(x => x)
def sumCubes = sum(x => x * x * x)
def sumFactorials = sum(fact)

/**
 * Above code could be written in succint manner using following syntax.
 * Here, `sum(cube)` returns a function which is applied to arguments 1 to 10.
 */

sum(cube) (1, 10)

// This is called currying. With this, we can write our sum function again as below.
def sum(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 0 else f(a) + sum(f)(a + 1, b)

/**
 * Write a product function that calculates the product of the values of a function of the points on
 * a given interval.
 *
 * Next, write `factorial` in terms of product.
 * Can you write more general function, which generalizes both `sum` and `product`?
 */
def product(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 1 else f(a) * product(f)(a + 1, b)

product(x => x)(1,3)

def factorial(n: Int): Int = product(id)(1, n)

factorial(5)

/**
 * Create a common function which can be applied to create both `sum` and `product` functions above.
 * @param f function to apply
 * @param combine Whether to sum or multiply?
 * @param zero Unit value for specific operation; 0 for sum and 1 for multiply
 * @param a First parameter, begin value
 * @param b second parameter, end value
 * @return
 */
def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a:Int, b:Int): Int =
  if (a > b) zero else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))

def product(f: Int => Int) (a: Int, b: Int): Int =
  mapReduce(f, (x, y) => x * y, 1)(a, b)

def sum(f: Int => Int) (a: Int, b: Int): Int =
  mapReduce(f, (x, y) => x + y, 0)(a,b)

product(id)(1, 5)