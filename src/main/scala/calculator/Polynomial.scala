package calculator

import Math._

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = {
    Signal{
      pow(b(), 2) - (4 * a() * c())
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val delta = computeDelta(a,b,c)
      val twoA = 2*a()

      if (delta() < 0) {
        Set()
      } else {
        Set((-b() + sqrt(delta()))/twoA, (-b() - sqrt(delta()))/twoA)
      }
    }
  }
}
