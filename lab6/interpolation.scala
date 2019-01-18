

object Main extends App{
    println("siema")
    val listX: List[Double] = List(5,-7,-6,0)
    val listY: List[Double] = List(1,-23,-54,-954)
    lagrange(listX,listY))
}


object Interpolation{
    type Interpolation = List[List[Double => Double]]
    def lagrange(listX: List[Double],listY: List[Double]): Interpolation = {
        val n = listX.size
        def computeLi(i: Int, j: Int,first: Boolean): List[Double => Double] = {
            (i,j) match{
                case _ if(i == j) => computeLi(i,j+1,first)
                case (_,a) if(a == n) => List()
                case _ if(first) => ((x: Double) => listY(i)*(x - listX(j))/(listX(i) - listX(j))) :: computeLi(i,j+1,false)
                case _     => ((x: Double) => (x - listX(j))/(listX(i) - listX(j))) :: computeLi(i,j+1,first)
            }
        }
        def recLagrange(i: Int): Interpolation = {
            i match {
                case _ if(i == n) => List()
                case _ => computeLi(i,0,true) :: recLagrange(i+1)
            }
        }
        recLagrange(0)
    }
    def evalInterpolation(interpolation: Interpolation, x: Double):Double = {
        interpolation.map(yIlI => yIlI.map(subFun => subFun(x)).reduce(_*_)).sum
    }
}

lag.map(Li => Li.map(_(1)).reduce(_*_)).sum

val lag = lagrange(listX,listY)


def netwon(listX: List[Double],listY: List[Double]): Interpolation = {
    // val n = listX.size
    val n = 2
    val p0: Interpolation = List(List((x: Double) => listY(0)))

    def newQuotient(k: Int,cK: Double): Interpolation = {
        (listX.take(1).map(x => ((x:Double) => cK*x) compose ((k:Double) => k - x))) :: 
            listX.take(k).drop(1).map(x => ((k:Double) => k - x)) :: List()
    }
    def loop(k: Int, acc: Interpolation): Interpolation = {
        if(k == n) acc
        else{
            val denominator: Double =  listX.take(k).map(x => listX(k) - x).reduce(_*_)
            val cK: Double = (listY(k) - evalInterpolation(acc,listX(k)))/denominator

            // loop(k+1, acc ++ newQuotient(k,cK))
            acc ++ newQuotient(k,cK)
        }
    } 
    loop(1,p0)
}
















def lagrange(listX: List[Double],listY: List[Double]): List[List[Double => Double]] = {
        val n = listX.size
        def loop(i: Int, j: Int): List[Double => Double] = {
            (i,j) match{
                case _ if(i == j) => loop(i,j+1)
                case (_,a) if(a == n) => List()
                case _     => ((x: Double) => (x - listX(j))/(listX(i) - listX(j))) :: loop(i,j+1)
            }
        }
        var tmp: List[List[Double => Double]] = List()
        for(i <- 0 until n ){
            tmp =  loop(i,0) :: tmp
        }
        tmp
    }
