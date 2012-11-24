package org.fjn.optimization.gradient.differentiation

import org.fjn.matrix.Matrix
import collection.immutable.IndexedSeq

trait DifferentialOperators{
  val dx:Seq[Double]
  val pFunc:(org.fjn.matrix.Matrix[Double])=>Double

  val dOffsets: IndexedSeq[Matrix[Double]] = (0 until dx.length).map(i =>{
    val dd = new Matrix[Double](dx.length,1)
    dd.zeros
    dd.set(i,0,dx(i))
    dd
  })

  private def firstDerivative= (x:Matrix[Double],i:Int) =>{
    (pFunc(x + dOffsets(i))-pFunc( x - dOffsets(i)))/2.0/dx(i)
  }

  private def secondDerivative= (x:Matrix[Double],i:Int,j:Int) =>{
    (firstDerivative(x+dOffsets(j),i)-firstDerivative(x-dOffsets(j),i))/2.0/dx(j)
  }


  def grad(x:org.fjn.matrix.Matrix[Double]):Matrix[Double]={
    require(x.numberCols == 1 && x.numberRows == dx.length)
    val g: IndexedSeq[Double] = (0 until dx.length).map(i => firstDerivative(x,i))

    val mGrad = new Matrix[Double](dx.length,1)
    mGrad <= g

    mGrad
  }

  def hessian(x:Matrix[Double]):Matrix[Double]={

    val mH = new Matrix[Double](dx.length,dx.length)

    for (i <- 0 until dx.length;
         j <- 0 until dx.length){
      mH.set(i,j,secondDerivative(x,i,j))
    }

    mH

  }
}
object DifferentialOpsFactory {


  def apply(ff:(Matrix[Double])=>Double,dtol:Seq[Double]):DifferentialOperators={

    new {
      val dx = dtol
      val pFunc = ff
    } with DifferentialOperators
  }

}
