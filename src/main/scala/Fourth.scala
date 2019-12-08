object Fourth extends App {

  def isValid(x: Int): Boolean = {
    val valArr: Array[Int] = x.toString.toCharArray.map(x => x.toString.toInt)
    val hasDups = valArr.sliding(2).exists(x => x(0) == x(1))
    val isIncreasing = !valArr.sliding(2).exists(x => x(0) > x(1))
    hasDups && isIncreasing
  }

  def notPartOfGroup(x: Int) = {
    val valArr: Array[Int] = x.toString.toCharArray.map(x => x.toString.toInt)
    val y = valArr.sliding(2).filter(x => x(0) == x(1)).toList
    y.exists(x => valArr.groupBy(identity)(x(0)).length == 2)
  }

  println((172851 to 675869).toList.count(x => isValid(x) && notPartOfGroup(x)))

}
