object Second extends App {
  val a = List(1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,9,1,19,1,19,5,23,1,23,6,27,2,9,27,31,1,5,31,35,1,35,10,39,1,39,10,43,2,43,9,47,1,6,47,51,2,51,6,55,1,5,55,59,2,59,10,63,1,9,63,67,1,9,67,71,2,71,6,75,1,5,75,79,1,5,79,83,1,9,83,87,2,87,10,91,2,10,91,95,1,95,9,99,2,99,9,103,2,10,103,107,2,9,107,111,1,111,5,115,1,115,2,119,1,119,6,0,99,2,0,14,0)

  def multiply = (x: Int, y: Int) => x * y
  def add = (x: Int, y: Int) => x + y

  def getUpdatedList(l: List[Int], index: Int, fn: (Int, Int) => Int) = {
    val firstArg = l(l(index + 1))
    val secondArg = l(l(index + 2))
    l.updated(l(index + 3), fn(firstArg, secondArg))
  }

  def processList(l: List[Int], index: Int): List[Int] = {
    val opcode = l(index)
    if(opcode == 1) {
      val u = getUpdatedList(l, index, add)
      processList(u, index + 4)
    } else if(opcode == 2) {
      val u = getUpdatedList(l, index, multiply)
      processList(u, index + 4)
    } else {
      l
    }
  }

  def getMatchingResult(a: List[Int]): Int = {
    (for {
      i <- 0 to 99
      j <- 0  to 99 if processList(a.updated(1, i).updated(2, j), 0).head == 19690720
    } yield 100 * i + j)(0)
  }




}
