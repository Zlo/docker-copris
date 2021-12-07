// AIVD Kerstpuzzel 2020 19
// https://www.aivd.nl/onderwerpen/aivd-kerstpuzzel
// Blame it on Zlo

import jp.kobe_u.copris._
import jp.kobe_u.copris.dsl._

object kp2020_19 {
  implicit class Sieve(val N: Int) extends AnyVal {
    def primesUpTo() = {
      val isPrime = collection.mutable.BitSet(0 to N: _*) -- (4 to N by 2)
      for (p <- 2 +: (3 to Math.sqrt(N).toInt by 2) if isPrime(p)) {
	isPrime --= p*p to N by p
      }
      isPrime
    }
  } 

  def main(args: Array[String]) = {
    def solve(puzzles: Seq[Seq[Seq[Int]]]) = {
      val primes1 = 4.primesUpTo
      val primes3 = 444.primesUpTo
      val primes4 = 4444.primesUpTo
      val quad1 = List(0, 1,4)
      val quad3 = List(0, 121,144,324,441)
      val fib1 = List(0, 1, 2, 3)
      val fib2 = List(0, 13,21,34)
      val fib3 = List(0, 144,233)
      val div1 = List(0, 1, 2, 3, 4)
      val div2 = List(0, 11, 12, 14, 21, 22, 24, 33, 42, 44)
      val div3 = List(0, 112, 132, 144, 231)

      for (i <- 0 until 4; j <- 0 until 4; k <- 0 until 4)
        int('x(i,j,k), 1, 4)

      for (i <- 0 until 4; j <- 0 until 4; k <- 0 until 4 ; if puzzles(i)(j)(k) > 0)
        add('x(i,j,k) === puzzles(i)(j)(k))

      // Puzzle 1 and 2
      for (i <- 0 until 2; j <- 0 until 4)
        add(Alldifferent((0 until 4).map('x(i,j,_))))
      for (i <- 0 until 2; j <- 0 until 4)
        add(Alldifferent((0 until 4).map('x(i,_,j))))
      for (i <- 0 until 2; j <- 0 until 4 by 2; k <- 0 until 4 by 2) {
        val xs = for (dj <- 0 until 2; dk <- 0 until 2) yield 'x(i, j+dj,k+dk)
        add(Alldifferent(xs))
      }

      // Jigoku
      add('x(1,2,1) > 'x(1,3,1))
      add('x(1,3,2) < 'x(1,3,3))

      // Kakuro
      add(Add(for (k <- 1 to 2) yield 'x(2, 1, k)) === 5)
      add(Alldifferent(for (k <- 1 to 2) yield 'x(2, 1, k)))

      add(Add(for (k <- 1 to 3) yield 'x(2, 2, k)) === 8)
      add(Alldifferent(for (k <- 1 to 3) yield 'x(2, 2, k)))

      add(Add(for (k <- 1 to 3) yield 'x(2, 3, k)) === 7)
      add(Alldifferent(for (k <- 1 to 3) yield 'x(2, 3, k)))

      add(Add(for (j <- 1 to 3) yield 'x(2, j, 1)) === 7)
      add(Alldifferent(for (j <- 1 to 3) yield 'x(2, j, 1)))

      add(Add(for (j <- 1 to 3) yield 'x(2, j, 2)) === 8)
      add(Alldifferent(for (j <- 1 to 3) yield 'x(2, j, 2)))

      add(Add(for (j <- 2 to 3) yield 'x(2, j, 3)) === 5)
      add(Alldifferent(for (j <- 2 to 3) yield 'x(2, j, 3)))

      // Doorloper
      // Fibonacci tot 3 cijfers: 1, 2, 3, 13, 21, 34, 144
      // Kwadraat tot 3 cijfers: 1, 4, 121, 144, 324, 441
      // H1
      int('p1, primes1.toSet)
      int('p3, primes3.toSet)
      int('q1, quad1.toSet)
      int('q3, quad3.toSet)

      add('x(3,0,3) === 1)
      add(('x(3,0,0) === 'p1 && ('x(3,0,1) * 100 + 'x(3,0,2) * 10 + 'x(3,0,3) === 'q3) && 'q1 === 0 && 'p3 === 0)
        || ('x(3,0,0) * 100 + 'x(3,0,1) * 10 + 'x(3,0,2) === 'p3 && 'x(3,0,3) === 'q1 && 'q3 === 0 && 'p1 === 0)
        )

      // H2
      add('x(3,1,3) % 2 === 1) // not divisible by 2

      add(('x(3,1,0) === 4 && ('x(3,1,1) + 'x(3,1,2) + 'x(3,1,3)) % 3 =/= 0)
        || (('x(3,1,0) * 10 + 'x(3,1,1)) % 4 === 0 && (('x(3,1,2) + 'x(3,1,3)) % 3 =/= 0))
        || (('x(3,1,1) * 10 + 'x(3,1,2)) % 4 === 0 && 'x(3,1,3) % 3 =/= 0))

      // H3
      int('p4, primes4.toSet)
      add(('x(3,2,0) * 1000 + 'x(3,2,1) * 100 + 'x(3,2,2) * 10 + 'x(3,2,3)) === 'p4)

      // H4
      add(('x(3,3,0) * 1000 + 'x(3,3,1) * 100 + 'x(3,3,2) * 10 + 'x(3,3,3)) % 7 === 0)

      // V1
      int('f1(0), fib1.toSet)
      int('f2(0), fib2.toSet)
      int('f3(0), fib3.toSet)
      int('d1, div1.toSet)
      int('d2, div2.toSet)
      int('d3, div3.toSet)
      add(('x(3,0,0) === 'f1(0) && ('x(3,1,0) * 100 + 'x(3,2,0) * 10 + 'x(3,3,0)) === 'd3 && 'd2 === 0 && 'd1 === 0 && 'f2(0) === 0 && 'f3(0) === 0)
        || ('x(3,0,0) * 10 + 'x(3,1,0) === 'f2(0) && ('x(3,2,0) * 10 + 'x(3,3,0)) === 'd2 && 'd3 === 0 && 'd1 === 0 && 'f1(0) === 0 && 'f3(0) === 0)
        || ('x(3,0,0) * 100 + 'x(3,1,0) * 10 + 'x(3,2,0) === 'f3(0) && 'x(3,3,0) === 'd1 && 'd3 === 0 && 'd2 === 0 && 'f1(0) === 0 && 'f2(0) === 0)
        )

      // V2
      add('x(3,0,1) === 'x(3,3,1))
      add('x(3,1,1) === 'x(3,2,1))
      // V3
      add(Alldifferent(for (k <- 0 to 3) yield 'x(3, k, 2)))
      // V4
      int('f1(1), fib1.toSet)
      int('f1(2), fib1.toSet)
      int('f2(1), fib2.toSet)
      add(('x(3,0,3) === 'f1(1) && ('x(3,1,3) === 'f1(2) && ('x(3,2,3) * 10 + 'x(3,3,3) === 'f2(1))
                                || (('x(3,1,3) * 10 + 'x(3,2,3)) === 'f2(1) && 'x(3,3,3) === 'f1(2))))
          || (('x(3,0,3) * 10 + 'x(3,1,3)) === 'f2(1) && 'x(3,2,3) === 'f1(1) && 'x(3,3,3) === 'f1(2))
        )


      // Letters
      add('x(1, 0, 0) === 'x(2, 2, 1)) // A
      add('x(1, 0, 0) === 'x(3, 0, 0)) // A
      add('x(0, 1, 1) === 'x(3, 2, 2)) // B
      add('x(0, 3, 0) === 'x(1, 3, 0)) // C
      add('x(0, 2, 1) === 'x(2, 3, 2)) // D
      add('x(0, 1, 3) === 'x(3, 3, 0)) // E
      add('x(0, 2, 3) === 'x(1, 0, 1)) // F
      add('x(0, 2, 3) === 'x(3, 3, 2)) // F
      add('x(1, 2, 0) === 'x(3, 2, 1)) // G
      add('x(1, 1, 2) === 'x(3, 2, 3)) // H
    }
    val puzzles = Seq(
      Seq(
        Seq(0, 0, 0, 0),
        Seq(0, 0, 0, 0),
        Seq(0, 0, 0, 0),
        Seq(0, 0, 0, 0)),
      Seq(
        Seq(0, 0, 0, 4),
        Seq(0, 0, 0, 0),
        Seq(0, 0, 0, 0),
        Seq(0, 0, 0, 0)),
      Seq(
        Seq(4, 4, 4, 4),
        Seq(4, 0, 0, 4),
        Seq(4, 0, 0, 0),
        Seq(4, 0, 0, 0)),
      Seq(
        Seq(0, 0, 0, 0),
        Seq(0, 0, 0, 0),
        Seq(0, 0, 0, 0),
        Seq(0, 0, 0, 0)))

    solve(puzzles)

    if (find) {
      do {
        for (i <- 0 until 4) {
          println(i + ":")
          for (j <- 0 until 4) {
            val as = for (k <- 0 until 4) yield solution('x(i,j,k))
            println(as.map("%3d".format(_)).mkString(""))
          }
        }
        val s0 = solution('x(0,0,0)).toInt + solution('x(1,0,0)).toInt + solution('x(3,0,0)).toInt
        val s1 = solution('x(0,1,1)).toInt + solution('x(1,1,1)).toInt + solution('x(2,1,1)).toInt + solution('x(3,1,1)).toInt
        val s2 = solution('x(0,2,2)).toInt + solution('x(1,2,2)).toInt + solution('x(2,2,2)).toInt + solution('x(3,2,2)).toInt
        val s3 = solution('x(0,3,3)).toInt + solution('x(1,3,3)).toInt + solution('x(2,3,3)).toInt + solution('x(3,3,3)).toInt
        println("sum: " + (s0 * 1000 + s1 * 100 + s2 * 10 + s3))
        println("d1: " + solution('d1))
        println("d2: " + solution('d2))
        println("d3: " + solution('d3))
        println("q1: " + solution('q1))
        println("q3: " + solution('q3))
        println("f1(0): " + solution('f1(0)))
        println("f2(0): " + solution('f2(0)))
        println("f3(0): " + solution('f3(0)))
        println("f1(1): " + solution('f1(1)))
        println("f1(2): " + solution('f1(2)))
        println("f2(1): " + solution('f2(1)))
        println("p1: " + solution('p1))
        println("p3: " + solution('p3))
        println("p4: " + solution('p4))
      } while (findNext)
    }
  }
}
