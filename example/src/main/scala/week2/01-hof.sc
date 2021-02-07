import scala.annotation.tailrec

def sumInts(a: Int, b: Int): Int =
  if (a > b) 0 else a + sumInts(a + 1, b)

def cube(x: Int): Int = x * x * x

def fact(n: Int): Int = {
  def loop(acc: Int, n: Int): Int =
    if (n == 0) acc
    else loop(acc * n, n - 1)
  loop(1, n)
}

// We have to define the same style of function three times.
def sumCubes(a: Int, b: Int): Int =
  if (a > b) 0 else cube(a) + sumCubes(a + 1, b)

def sumFactorials(a: Int, b: Int): Int =
  if (a > b) 0 else fact(a) + sumFactorials(a + 1, b)

// We can reuse those by using sum function defined
def sum(f: Int => Int, a: Int, b: Int): Int =
  if (a > b) 0
  else f(a) + sum(f, a + 1, b)


def id(x: Int): Int = x


def sumInts(a: Int, b: Int) = sum(id, a, b)
def sumCubes(a: Int, b: Int): Int = sum(cube, a, b)
def sumFactorials(a: Int, b: Int): Int = sum(fact, a, b)

// Using anonymous functions
def sumInts(a: Int, b: Int) = sum(x => x, a, b)
def sumCubes(a: Int, b: Int): Int = sum(x => x * x * x, a, b)

// Write tail recursive version of sum
def sum(f: Int => Int, a: Int, b: Int): Int = {
  @tailrec
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, f(a) + acc)
  }
  loop(a, 0)
}

sum(x => x * x, 3, 5)