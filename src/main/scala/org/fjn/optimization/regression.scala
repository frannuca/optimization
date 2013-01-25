package org.fjn.optimization

import org.fjn.matrix.Matrix

case class Regression(order:Int) {
  def getRegressionCoefficients(v:Seq[Double]):Seq[Double]={

    val Y  = (new Matrix[Double](v.size,1) <= v)

    val X = new Matrix[Double](v.size,order+1)
    for {
      i <- 0 until v.size;
      j <- 0 to order
    }{

      X.set(i,j,math.pow(i.toDouble,j))

    }

    val aux: Matrix[Double] = (X.transpose * X)
    aux.invert
    val aux2 = (aux * X.transpose * Y).getArray().toSeq
    aux2
  }
}
