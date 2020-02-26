// Currying
object exercise2 {
  def sum(f: Int => Int, a: Int, b: Int): Int = {
    @scala.annotation.tailrec
    def loop(a: Int, acc: Int): Int =
      if (a > b) acc
      else loop(a + 1, f(a) + acc)

    loop(a, 0)
  }

  def product(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)

  def factorial(i: Int): Int = product(x => x)(1, i)

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a+1, b))
  }
}

println(exercise2.sum(x => x, 1, 1))
println(exercise2.product(x => x)(1, 3))
println(exercise2.factorial(5))