package de.uni_luebeck.isp.tessla.interpreter

object Main extends App {


  object MySpec extends Specification[Int] {
    val in1 = Input[Int]()
    printStream(in1, "in1")
    val op1 = in1 + 2
    printStream(op1, "op1")


    val s1 = last(op1, op1.default(10))
    printStream(s1, "s1")


    lazy val period: Stream[Int] = delayedLast(period, period).default(3)
    printStream(period, "period")

    val s3 = period.time / in1
    printStream(s3, "s3")

    lazy val s4: (Lazy[Stream[Int]], Lazy[Stream[Int]]) = period.resetCount(s4._2 > 3)
    printStream(s4._1, "###")

  }

  MySpec.step(5)
  MySpec.in1.provide(1)
  MySpec.step()
  MySpec.step(1)
  MySpec.step(2)
  MySpec.in1.provide(6)
  MySpec.step(1)
  MySpec.step(14)
  MySpec.step()


}
