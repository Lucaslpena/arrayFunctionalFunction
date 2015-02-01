import scala.collection.mutable.ArrayBuffer

/**
 * Created by L. on 31-Jan-15.
 * Given an array of unsorted positive integers, write a function that finds runs of 3 consecutive numbers
 * (ascending or descending) and returns the indices where such runs begin.
 * If no such runs are found, return null.
 *
 * Takes command like arguments sbt "run [integervalues seperated by spaces]" or defalts to test case
 */
object Main {
  def main (args: Array[String]) {
    val myList = if (args.length == 0) Array(1,2,3,5,10,9,8,9,10,11,7) else { //test case
      val arguments = ArrayBuffer[Int]()
      args.foreach(arguments +=_.toInt)
      arguments.toArray
    }
    arrayFunction(myList).foreach(println)
  }

  def arrayFunction(array: Array[Int]):Array[Int] = {

    def consecutiveCheck(a:Int, b:Int) = {
      if ((a*b < 0) || (a+b < 0)) false
      else {
        val test = xor(toBool(a),toBool(b))
        !test
      }
    }
    def assessNextValue(x:Int) = {
      if ((array(x) - next(x)) == -1) 1  //increment
      else if ((array(x) - next(x)) == 1) 0 //decrement
      else -1
    }
    def xor(x:Boolean,y:Boolean) = { (x && !y) || (y && !x) }
    def toBool(x:Int):Boolean = { if (x == 0) false else true }
    def next(x:Int) = {val y = x+1; array(y)}

    val returnArray = ArrayBuffer[Int]()
    var count = 0
    while (count < array.length-2)
    {
      val nextCount = count +1
      if (consecutiveCheck(assessNextValue(count),assessNextValue(nextCount))) returnArray += count
      count +=1
    }
    if (returnArray.length == 0) null else returnArray.toArray
  }
}