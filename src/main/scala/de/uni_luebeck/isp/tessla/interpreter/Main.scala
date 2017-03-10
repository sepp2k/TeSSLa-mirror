package de.uni_luebeck.isp.tessla.interpreter

// scalastyle:off
object Main extends App {

  object MySpec extends Specification[Int] {
    val in1 = Input[Int]()
    printStream(in1, "in1")
    val op1 = in1 + 2
    printStream(op1, "op1")

    val s1 = last(op1, op1.default(10))
    printStream(s1, "s1")

    val prd = period(3)
    printStream(prd, "period")

    val s3 = prd.time / in1
    printStream(s3, "s3")

    val s4: ResetStream[Int] = prd.resetCount((s4.proposed > 3).ifThen())
    printStream(s4, "###")

    val in2 = Input[Unit]()
    val in3 = Input[Unit]()
    printStream(in3, "in3")
    printStream(in2, "in2")
    val s5 = in2.const(true).resetExists(in3)
    printStream(s5, "s5")

    val in4 = Input[Int]()
    val s6 = in4.resetMax(in3)
    printStream(s6, "s6")
  }

  MySpec.step(5)
  MySpec.in1.provide(1)
  MySpec.in2.provide(())
  MySpec.in4.provide(3)
  MySpec.step()
  MySpec.step(1)
  MySpec.in2.provide(())
  MySpec.in4.provide(1)
  MySpec.step(2)
  MySpec.in3.provide(())
  MySpec.in1.provide(6)
  MySpec.step(1)
  MySpec.in2.provide(())
  MySpec.in4.provide(2)
  MySpec.step(13)
  MySpec.in4.provide(1)
  MySpec.step(1)
  MySpec.in4.provide(0)
  MySpec.step()

}
