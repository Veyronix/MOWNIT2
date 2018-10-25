
object MyApp extends App{
    val x = -2
    val tmp: Stream[(Int,Double)] = (1,1.0) #:: (tmp map {case (x,y) => (x+1,y*x)})
    val factorial: Stream[(Int,Double)] = tmp map {case (x,y) => (x-1,y)}
    val isXPositive: Boolean = if(x >= 0) true else false
    def taylor(x: Int): Double = {
        var oldSum: Double = -1;
        var sum: Double = 0;
        val iterator = factorial.iterator
        while(oldSum != sum){
            val (n,nFac) = iterator.next
            oldSum = sum
            if(nFac < 40000000 ){
                sum += {
                    if(isXPositive)
                            scala.math.pow(x,n)/nFac
                    else
                            scala.math.pow(-x,n)/nFac
                }
            }
              
        }
        if(isXPositive)
            sum
        else
            1/sum
    }
    println(taylor(x))
}
