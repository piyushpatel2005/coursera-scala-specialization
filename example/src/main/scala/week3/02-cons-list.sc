import java.util.NoSuchElementException


trait List[T] {
  def isEmpty: Boolean

  def head: T

  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false

}

class Nil[T] extends List[T] {
  def isEmpty = true

  def head: Nothing = throw new NoSuchElementException("Nil.head")

  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

/**
 * Write a function nth that takes an integer `n` and `list` and
 * selects the `n`th element of the list
 * @param n index to select
 * @param xs list
 * @tparam T
 * @return
 */
def nth[T](n: Int, xs: List[T]): T = {
  if (xs.isEmpty) throw new IndexOutOfBoundsException
  if (n == 0) xs.head
  else nth(n - 1, xs.tail)
}

val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))
nth(2, list)
nth(-1, list)