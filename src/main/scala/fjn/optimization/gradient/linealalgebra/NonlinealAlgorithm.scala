package fjn.optimization.gradient.linealalgebra

import org.fjn.matrix.Matrix
import scala.math

trait NonlinealAlgorithm{

  def x0:Matrix[Double]
  def next(x:Matrix[Double]) : Matrix[Double]


  def iterate(numberOfIterations:Int):Seq[Matrix[Double]]={

    var xnow = x0.clone()

    val nIter:Int = math.max(numberOfIterations,1)

    (0 until nIter).map(i => {
      xnow = next(xnow)
      xnow.clone()
    })

  }
}
