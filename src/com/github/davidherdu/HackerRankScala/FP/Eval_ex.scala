package com.github.davidherdu.HackerRankScala.FP

object Eval_ex {

  def fact(x: Int): Double = {
    if (x == 1) x
    else x * fact(x - 1)
  }

  def pow(x: Double, y: Int): Double = {
    if (y == 0) 1
    else if (y == 1) x
    else x * pow(x, y - 1)
  }

  def fracc(x: Double, y: Int): Double = {
    pow(x, y) / fact(y)
  }

  def exp2(x: Double, y: Int): Double = {
    if (y == 0) 1
    else fracc(x, y) + exp2(x, y - 1)
  }

  def exp(x: Double): Double = {
    exp2(x, 9)
  }

  def eX(x: Double, y: Int) = 1 + (1 until y).map(n => fracc(x, n)).sum

  def round4Decimals(d: Double): Double = (d * 10000).round / 10000.0

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val n = stdin.readLine.trim.toInt

    for (nItr <- 1 to n) {
      val x = stdin.readLine.trim.toDouble
      println(round4Decimals(exp(x)))
    }
  }
}