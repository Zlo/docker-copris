/*
 * This program is a part of Copris software package.
 * http://bach.istc.kobe-u.ac.jp/copris/
 */

import jp.kobe_u.copris._

/**
 * Sudoku 5x5 puzzle solver
 * @see [[http://www.nikoli.com/en/puzzles/sudoku/rule.html]]
 * @see [[http://puzzle.gr.jp]]
 */
object Sudoku5 {
  import jp.kobe_u.copris.dsl._
  def solve(m: Int, n: Int, puzzle: Seq[Seq[Int]]) = {
    for (i <- 0 until 21; j <- 0 until 21) {
      if (puzzle(i)(j)> -1)
        int('x(i,j), 1, n)
      else
        int('x(i,j), -1, -1)
    }

    for (i <- 0 until n) {
      add(Alldifferent((0 until n).map('x(i,_))))
      add(Alldifferent((0 until n).map('x(i + 12,_))))
      add(Alldifferent((12 until n + 12).map('x(i,_))))
      add(Alldifferent((12 until n + 12).map('x(i + 12,_))))
      add(Alldifferent((6 until n + 6).map('x(i + 6,_))))
    }

    for (j <- 0 until n) {
      add(Alldifferent((0 until n).map('x(_,j))))
      add(Alldifferent((0 until n).map('x(_,j + 12))))
      add(Alldifferent((12 until n + 12).map('x(_,j))))
      add(Alldifferent((12 until n + 12).map('x(_,j + 12))))
      add(Alldifferent((6 until n + 6).map('x(_,j + 6))))
    }

    for (i <- 0 until n by m; j <- 0 until n by m) {
      val xs0 = for (di <- 0 until m; dj <- 0 until m) yield 'x(i+di,j+dj)
      add(Alldifferent(xs0))
      val xs1 = for (di <- 0 until m; dj <- 0 until m) yield 'x(i+di + 12,j+dj)
      add(Alldifferent(xs1))
      val xs2 = for (di <- 0 until m; dj <- 0 until m) yield 'x(i+di,j+dj + 12)
      add(Alldifferent(xs2))
      val xs3 = for (di <- 0 until m; dj <- 0 until m) yield 'x(i+di + 12,j+dj + 12)
      add(Alldifferent(xs3))
      val xs4 = for (di <- 0 until m; dj <- 0 until m) yield 'x(i+di + 6,j+dj + 6)
      add(Alldifferent(xs4))
    }

    for (i <- 0 until 21; j <- 0 until 21; if puzzle(i)(j) > 0) {
      add('x(i,j) === puzzle(i)(j))
    }
    if (find) {
      for (i <- 0 until 21) {
	println((0 until 21).map(j => solution('x(i,j))).mkString(" "))
      }
      if (findNext)
        println("Multiple solutions")
    }
  }
  def main(args: Array[String]) = {
    /* http://puzzle.gr.jp */
    val puzzle = Seq(
      Seq( 5,  0,  0,  2,  1,  4,  7,  0,  0, -1, -1, -1,  0,  0,  3,  5,  7,  2,  0,  0,  9),
      Seq( 0,  0,  0,  8,  0,  0,  2,  0,  0, -1, -1, -1,  0,  0,  9,  0,  0,  1,  0,  0,  0),
      Seq( 0,  0,  0,  0,  0,  6,  0,  0,  5, -1, -1, -1,  8,  0,  0,  3,  0,  0,  0,  0,  0),

      Seq( 9,  2,  0,  3,  0,  0,  0,  7,  0, -1, -1, -1,  0,  4,  0,  0,  0,  3,  0,  7,  2),
      Seq( 4,  0,  0,  0,  0,  2,  0,  3,  0, -1, -1, -1,  0,  3,  0,  1,  0,  0,  0,  0,  8),
      Seq( 6,  0,  7,  0,  5,  0,  1,  0,  0, -1, -1, -1,  0,  0,  2,  0,  9,  0,  5,  0,  1),

      Seq( 8,  9,  0,  0,  0,  7,  0,  0,  0,  0,  6,  0,  0,  0,  0,  2,  0,  0,  0,  4,  3),
      Seq( 0,  0,  0,  5,  4,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  3,  4,  0,  0,  0),
      Seq( 0,  0,  4,  0,  0,  0,  0,  0,  6,  9,  0,  7,  3,  0,  0,  0,  0,  0,  7,  0,  0),

      Seq(-1, -1, -1, -1, -1, -1,  0,  0,  4,  2,  0,  8,  9,  0,  0, -1, -1, -1, -1, -1, -1),
      Seq(-1, -1, -1, -1, -1, -1,  5,  0,  0,  0,  0,  0,  0,  0,  8, -1, -1, -1, -1, -1, -1),
      Seq(-1, -1, -1, -1, -1, -1,  0,  0,  8,  4,  0,  3,  7,  0,  0, -1, -1, -1, -1, -1, -1),

      Seq( 0,  0,  8,  0,  0,  0,  0,  0,  9,  1,  0,  6,  8,  0,  0,  0,  0,  0,  6,  0,  0),
      Seq( 0,  0,  0,  9,  8,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  8,  5,  0,  0,  0),
      Seq( 1,  4,  0,  0,  0,  6,  0,  0,  0,  0,  3,  0,  0,  0,  0,  4,  0,  0,  0,  8,  7),

      Seq( 9,  0,  3,  0,  6,  0,  1,  0,  0, -1, -1, -1,  0,  0,  8,  0,  7,  0,  3,  0,  5),
      Seq( 7,  0,  0,  0,  0,  9,  0,  2,  0, -1, -1, -1,  0,  3,  0,  1,  0,  0,  0,  0,  9),
      Seq( 8,  1,  0,  5,  0,  0,  0,  9,  0, -1, -1, -1,  0,  5,  0,  0,  0,  4,  0,  6,  8),

      Seq( 0,  0,  0,  0,  0,  3,  0,  0,  4, -1, -1, -1,  3,  0,  0,  5,  0,  0,  0,  0,  0),
      Seq( 0,  0,  0,  2,  0,  0,  9,  0,  0, -1, -1, -1,  0,  0,  4,  8,  0,  3,  0,  0,  0),
      Seq( 3,  0,  0,  4,  7,  8,  5,  0,  0, -1, -1, -1,  0,  0,  1,  7,  6,  9,  0,  0,  3)
    )
    solve(3, 9, puzzle)
  }
}
