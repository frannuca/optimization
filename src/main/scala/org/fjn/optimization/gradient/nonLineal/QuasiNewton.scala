package org.fjn.optimization.gradient.nonLineal

import org.fjn.optimization.gradient.differentiation.{DifferentialOpsFactory, DifferentialOperators}
import collection.mutable
import org.fjn.matrix.Matrix

trait QNUpdate{
  def next(Hk:Matrix[Double],dx:Matrix[Double],dy:Matrix[Double]):Matrix[Double]
}
trait DFPQNUpdate extends QNUpdate{

  def next(Hk:Matrix[Double],dx:Matrix[Double],dy:Matrix[Double]):Matrix[Double]={


    val dxT = dx.transpose
    val HkT = Hk.transpose
    val dyT = dy.transpose

    val aux1: Double = ( dyT * dx )(0,0)
    val aux2 = (dyT * Hk * dy)(0,0)

     Hk +  dx * dxT / aux1 - Hk * dy * dyT * HkT / aux2
  }
}

trait BFGSQNUpdate extends QNUpdate{

  def next(Hk:Matrix[Double],dx:Matrix[Double],dy:Matrix[Double]):Matrix[Double]={


    val dxT = dx.transpose
    val HkT = Hk.transpose
    val dyT = dy.transpose
    val I = new Matrix[Double](dx.numberRows,dx.numberRows)
    I.eye

    val aux1 = (dyT*dx)(0,0)
    val term1 = (I - dy * dxT /aux1)

    term1.transpose  * Hk * term1 + dx * dxT/aux1

  }
}


trait evaluationRegistry{
  private val evaluations =  new mutable.ListBuffer[(Matrix[Double],Double)]()
  val pFunc:((Matrix[Double])=>Double)

  def clearEvaluations{
    evaluations.clear()
  }


  def evaluate(x:Matrix[Double])(implicit tol:Double=1e-3):Double={


//    evaluations.collect{
//      case  y:(Matrix[Double],Double)  if ((x * y._1.transpose).getArray().head < tol) => ((x * y._1.transpose).getArray().head,y._2)
//    }
//     .map(_.asInstanceOf[(Double,Double)]).toSeq
//      match{
//        case Seq()=>
//          val a = (x,pFunc(x))
//          evaluations += a
//          evaluations.last._2
//        case s => s.minBy(_._1)._2
//      }

    pFunc(x)
  }
}
trait QuasiNewton  extends evaluationRegistry{

  self:QNUpdate =>

  val x0:Matrix[Double]
  var alpha:Double
  val pFunc:(Matrix[Double])=>Double
  val tolerance:Seq[Double]

  val ops= DifferentialOpsFactory(evaluate(_) ,tolerance)

  def ++(nIter:Int) : Matrix[Double]={


    clearEvaluations
    var Hk = new Matrix[Double](x0.numberRows,x0.numberRows)
    Hk.eye
    var x = x0.clone()

    var minX = x0.clone
    //first iteration:
    var grad0 = ops.grad(x0)
    var dx  =  Hk * grad0 * (-alpha)
    x = x0 + dx
    var grad1 = ops.grad(x)

    var counter = 0
    (0 until nIter).foreach(i =>{

      val xOld = x
      val HkOld = Hk
      val dxOld = dx

      Hk = self.next(Hk,dx,grad1-grad0)

      dx = Hk*grad1*(-alpha)


      val oldFunc = evaluate(x)

      x = x + dx
      if (evaluate(x)>oldFunc || evaluate(x).isNaN){
         x = xOld
         Hk = HkOld
         dx = dxOld
         alpha = alpha * 0.9
         counter = counter + 1

      }else{
        minX = x.clone()
        counter = 0
        grad0 = grad1
        grad1 = ops.grad(x)
      }



    })

    x


  }

}


