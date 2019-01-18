import java.io.File
import java.util.Random
import java.util.concurrent.TimeUnit

import Interpolation.Interpolation
//import Main.{listX, listY}
import breeze.linalg.DenseVector

import scala.annotation.tailrec
import breeze.interpolation._
import com.github.tototoshi.csv._
import com.sun.java.util.jar.pack._


object Main extends App{
  val listX: List[Double] = List(5,-7,-6,0)
  val listY: List[Double] = List(1,-23,-54,-954)
  import Interpolation._
  val lag: Interpolation = lagrange(listX,listY)
//  println(evalInterpolation(lag,0))
  val newtonVal: Interpolation = newton(listX,listY)
//  println(net.size)
  val check = 1
  println("0 = " + newtonVal(0).map(_(check)))
  println("1 = " + newtonVal(1).map(_(check)))
  println("2 = " + newtonVal(2).map(_(check)))
  println("3 = " + newtonVal(3).map(_(check)))
  println(evalInterpolation(newtonVal,check))
//  val f = CubicInterpolator(DenseVector(5,-7,-6,0), DenseVector(1,-23,-54,-954))
//  println("f =" + f(check))
//  MakeCSV.toCsv()
  val example_points: List[List[Double]] = List(List(7.81515229336533, 0.10958626314134179), List(2.190433419226773, 0.13110494656767369), List(1.1166175860847705, 0.6191655900511863), List(8.728457202028592, 0.3131654005552298), List(6.194745088080948, 0.030052013487595453), List(0.5681528278618153, 0.5967652898565929), List(2.4791591999355456, 0.8223881329130259), List(3.3069387478169565, 0.1555084681715152), List(3.3866802981361155, 0.5672714149930127), List(8.132015199706576, 0.1586610303192849), List(8.103630107687096, 0.8130387494143255))
  MakeCSV.firstExerciseToCsv(example_points,"example_points.csv")
  //  Measurement.measurement()
//  Measurement.timeMeasurement()

}

object Measurement{
  import Interpolation._

  def timeMeasurement(): Unit = {
    var lagrangeTimes: Map[Double,List[Double]] = Map()
    var newtonTimes: Map[Double,List[Double]] = Map()
    for(size <- 50 to 700 by 100){
      val r = new Random
      val listX: List[Double] = (0 to size).map(_ => 10 * r.nextDouble).toList
      val listY: List[Double] = (0 to size).map(_ => 1 * r.nextDouble).toList
      var listLagrange: List[Double] = List()
      var listNewton: List[Double] = List()
      for(_ <- 1 to 10) {
        val t0: Double = System.nanoTime()
        newton(listX, listY)
        val t1: Double = System.nanoTime()
        listNewton = (t1 - t0)/1000000 :: listNewton
        val t2: Double = System.nanoTime()
        lagrange(listX, listY)
        val t3: Double = System.nanoTime()
        listLagrange = (t3 - t2)/1000000 :: listLagrange
//        import java.util.concurrent.TimeUnit
//        val i: Double = TimeUnit.NANOSECONDS.toMillis(t3 - t2)
      }
      lagrangeTimes = lagrangeTimes + (size.toDouble -> listLagrange)
      newtonTimes = newtonTimes+ (size.toDouble -> listNewton)
    }
    println(lagrangeTimes)
    println(newtonTimes)
    MakeCSV.secondExerciseToCsv(newtonTimes,"newtonTimes.csv")
    MakeCSV.secondExerciseToCsv(lagrangeTimes,"lagrangeTimes.csv")
  }
  def measurement(): Unit ={
//    val numberOfPoints = 4

    val listX: List[Double] = List(7.81515229336533, 2.190433419226773, 1.1166175860847705, 8.728457202028592, 6.194745088080948, 0.5681528278618153, 2.4791591999355456, 3.3069387478169565, 3.3866802981361155, 8.132015199706576, 8.103630107687096)
    val listY: List[Double] = List(0.10958626314134179, 0.13110494656767369, 0.6191655900511863, 0.3131654005552298, 0.030052013487595453, 0.5967652898565929, 0.8223881329130259, 0.1555084681715152, 0.5672714149930127, 0.1586610303192849, 0.8130387494143255)

    val r = new Random
//    val listX: List[Double] = (0 to 10).map(_ => 10 * r.nextDouble).toList
//    val listY: List[Double] = (0 to 10).map(_ => 1 * r.nextDouble).toList
    println("listX = "+ listX)
    println("listY = "+ listY)
    val rangeMin = listX.min
    val rangeMax = listX.max
    val frequency  = (rangeMax-rangeMin)/1000

    val newtonInterpolation: Interpolation = newton(listX,listY)
    val newListNewtonX = (rangeMin to rangeMax by frequency).map(_ => rangeMin + (rangeMax - rangeMin) * r.nextDouble).toList
    val newListNewtonY = interpolationMeasurement(newtonInterpolation,newListNewtonX)
//    MakeCSV.firstExerciseToCsv(List(newListNewtonX,newListNewtonY),"newtonMeasurement.csv")

    val lagrangeInterpolation: Interpolation = lagrange(listX,listY)
    val newListLagrangeX = (rangeMin to rangeMax by frequency).map(_ => rangeMin + (rangeMax - rangeMin) * r.nextDouble).toList
    val newListLagrangeY = interpolationMeasurement(lagrangeInterpolation,newListLagrangeX)
//    MakeCSV.firstExerciseToCsv(List(newListLagrangeX,newListLagrangeY),"lagrangeMeasurement.csv")

  }

  def interpolationMeasurement(interpolation: Interpolation, listX: List[Double] ):List[Double] = {
    if (listX.isEmpty) List()
    else{
      evalInterpolation(interpolation,listX.head) :: interpolationMeasurement(interpolation,listX.tail)
    }
  }
}


object Interpolation {
  type Factor = List[Double => Double]
  type Interpolation = List[Factor]

  def lagrange(listX: List[Double], listY: List[Double]): Interpolation = {
    val n = listX.size

    def computeLi(i: Int, j: Int, first: Boolean): Factor = {
      (i, j) match {
        case _ if  i == j =>
          computeLi(i, j + 1, first)

        case (_, a) if a == n =>
          List()

        case _ if first => ((x: Double) =>
          listY(i) * (x - listX(j)) / (listX(i) - listX(j))) :: computeLi(i, j + 1, !first)

        case _ => ((x: Double) =>
          (x - listX(j)) / (listX(i) - listX(j))) :: computeLi(i, j + 1, first)
      }
    }

    def recLagrange(i: Int): Interpolation = {
      i match {
        case _ if i == n => List()
        case _ => computeLi(i, 0, true) :: recLagrange(i + 1)
      }
    }

    recLagrange(0)
  }

  def evalInterpolation(interpolation: Interpolation, x: Double): Double = {
    interpolation.map(yIlI => yIlI.map(subFun => subFun(x)).reduce(_ * _)).sum
  }

  def newton(listX: List[Double], listY: List[Double]): Interpolation = {
    val n = listX.size
    val p0: Interpolation = List(List((_: Double) => listY.head))

    def computePk(k: Int, cK: Double): Factor = {
      (listX.take(1).map(x => ((x: Double) => cK * x) compose ((k: Double) => k - x))
       ++ listX.slice(1,k).map(x => (l: Double) => l - x))
    }

    @tailrec
    def recNewton(k: Int, acc: Interpolation): Interpolation = {
      if (k == n) acc
      else {
        val denominator: Double = listX.take(k).map(x => listX(k) - x).reduce(_ * _)
        val cK: Double = (listY(k) - evalInterpolation(acc, listX(k))) / denominator
        recNewton(k+1, computePk(k,cK) :: acc)
      }
    }
    recNewton(1, p0)
  }
}


object MakeCSV {
  def firstExerciseToCsv[A](list: List[List[A]], fileName: String) = {
    val f = new File(fileName)
    val writer = CSVWriter.open(f)
//    writer.writeAll(list(0).zipAll(list(1),-0,-0).map{ case(x,y) => List(x,y)})
    writer.writeAll(list)
    writer.close()
  }

  def secondExerciseToCsv(test1: Map[Double,List[Double]], fileName: String) = {
    val f = new File(fileName)
    val writer = CSVWriter.open(f)
    val tmp: List[List[Double]] = test1.map{ case (x,y) => y.map( (x,_)) }.flatten.map{ case (x,y) => x :: y :: List()}.toList
    writer.writeAll(tmp)
    writer.close()
  }

}