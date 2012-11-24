package org.fjn.optimization.gradient.nonLineal

import org.fjn.matrix.Matrix

/**
 * Created with IntelliJ IDEA.
 * User: fran
 * Date: 10/14/12
 * Time: 6:13 PM
 * To change this template use File | Settings | File Templates.
 */
object testQuasiNewton extends App{

  def pFitness(x:Matrix[Double]):Double={
    (x(1,0)-0.75)*(x(1,0)-0.75) + (x(0,0)-0.25)*(x(0,0)-0.25)

  }
  val qnewton = new {
    var alpha = 1.0
    val pFunc = pFitness _
    val tolerance = Seq(1e-6,1e-6)
    val x0 = new Matrix[Double](2,1)<=Seq(2.0,2.0)
  } with QuasiNewton with DFPQNUpdate



  val xfinal = qnewton ++ 10

  println(xfinal)

}
