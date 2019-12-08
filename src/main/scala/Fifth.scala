import scala.annotation.tailrec

object Fifth extends App {
  val inputList = List(3,225,1,225,6,6,1100,1,238,225,104,0,1101,48,82,225,102,59,84,224,1001,224,-944,224,4,224,102,8,223,223,101,6,224,224,1,223,224,223,1101,92,58,224,101,-150,224,224,4,224,102,8,223,223,1001,224,3,224,1,224,223,223,1102,10,89,224,101,-890,224,224,4,224,1002,223,8,223,1001,224,5,224,1,224,223,223,1101,29,16,225,101,23,110,224,1001,224,-95,224,4,224,102,8,223,223,1001,224,3,224,1,223,224,223,1102,75,72,225,1102,51,8,225,1102,26,16,225,1102,8,49,225,1001,122,64,224,1001,224,-113,224,4,224,102,8,223,223,1001,224,3,224,1,224,223,223,1102,55,72,225,1002,174,28,224,101,-896,224,224,4,224,1002,223,8,223,101,4,224,224,1,224,223,223,1102,57,32,225,2,113,117,224,101,-1326,224,224,4,224,102,8,223,223,101,5,224,224,1,223,224,223,1,148,13,224,101,-120,224,224,4,224,1002,223,8,223,101,7,224,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,8,677,226,224,102,2,223,223,1006,224,329,101,1,223,223,107,677,677,224,1002,223,2,223,1006,224,344,101,1,223,223,8,226,677,224,102,2,223,223,1006,224,359,101,1,223,223,107,226,226,224,102,2,223,223,1005,224,374,1001,223,1,223,1108,677,226,224,1002,223,2,223,1006,224,389,101,1,223,223,107,677,226,224,102,2,223,223,1006,224,404,1001,223,1,223,1107,226,677,224,1002,223,2,223,1006,224,419,1001,223,1,223,108,677,677,224,102,2,223,223,1005,224,434,1001,223,1,223,1008,677,226,224,1002,223,2,223,1006,224,449,1001,223,1,223,7,226,677,224,1002,223,2,223,1006,224,464,1001,223,1,223,1007,677,677,224,102,2,223,223,1005,224,479,1001,223,1,223,1007,226,226,224,1002,223,2,223,1005,224,494,1001,223,1,223,108,226,226,224,1002,223,2,223,1005,224,509,1001,223,1,223,1007,226,677,224,1002,223,2,223,1006,224,524,101,1,223,223,1107,677,677,224,102,2,223,223,1005,224,539,101,1,223,223,1107,677,226,224,102,2,223,223,1005,224,554,1001,223,1,223,108,677,226,224,1002,223,2,223,1006,224,569,1001,223,1,223,1108,226,677,224,1002,223,2,223,1006,224,584,101,1,223,223,8,677,677,224,1002,223,2,223,1006,224,599,1001,223,1,223,1008,226,226,224,102,2,223,223,1006,224,614,101,1,223,223,7,677,677,224,1002,223,2,223,1006,224,629,101,1,223,223,1008,677,677,224,102,2,223,223,1005,224,644,101,1,223,223,7,677,226,224,1002,223,2,223,1005,224,659,101,1,223,223,1108,226,226,224,102,2,223,223,1006,224,674,1001,223,1,223,4,223,99,226)

  def isTrue: Int => Boolean = x => x != 0
  def isFalse: Int => Boolean = x => x == 0
  def equal: (Int, Int) => Boolean = (x, y) => x == y
  def lessThan: (Int, Int) => Boolean = (x, y) => x < y

  def jump(input: Input, fn: Int => Boolean) = {
    val (firstArg: Int, secondArg: Int) = getArguments(input)
    if(fn(firstArg)) {
      secondArg
    } else {
      input.index
    }
  }

  def compare(input: Input, fn: (Int, Int) => Boolean): List[Int] = {
    val (firstArg: Int, secondArg: Int) = getArguments(input)
    val res = if(fn(firstArg, secondArg)) {
      1
    } else {
      0
    }
    input.l.updated(input.l(input.index + 3), res)
  }

  def lessThan(input: Input):List[Int] = {
    compare(input, lessThan)
  }

  def equal(input: Input): List[Int] = {
    compare(input, equal)
  }

  def jumpIfTrue(input: Input) : Int = {
    jump(input, isTrue)
  }

  def jumpIfFalse(input: Input): Int = {
    jump(input, isFalse)
  }

  def add(input: Input) = {
    val (firstArg: Int, secondArg: Int) = getArguments(input)
    input.l.updated(input.l(input.index + 3), firstArg + secondArg)
  }

  def multiply(input: Input) = {
    val (firstArg: Int, secondArg: Int) = getArguments(input)
    input.l.updated(input.l(input.index + 3), firstArg * secondArg)
  }

  private def getArguments(input: Input) = {
    val (_, secondArgMode, firstArgMode) = input.paramModes
    val l = input.l
    val index = input.index
    val firstArg = if (firstArgMode == 0) {
      l(l(index + 1))
    } else {
      l(index + 1)
    }
    val secondArg = if (secondArgMode == 0) {
      l(l(index + 2))
    } else {
      l(index + 2)
    }
    (firstArg, secondArg)
  }

  def output(l: List[Int], index: Int, positional: Boolean) = {
    println(
      if(positional) {
        l(l(index + 1))
      } else {
        l(index + 1)
      }
    )
  }

  case class Input(l: List[Int], index: Int, paramModes: (Int, Int, Int))

  val programInput = 5

  @tailrec
  def processList(l: List[Int], index: Int): List[Int] = {
    val ocpm: Array[Int] = f"${l(index)}%05d".toCharArray.map(_.toString.toInt)
    val opcode = ocpm(4)
    val paramModes = (ocpm(0), ocpm(1), ocpm(2))
    val input = Input(l, index, paramModes)
    opcode match {
      case 1 =>
        val u = add(input)
        processList(u, index + 4)
      case 2 =>
        val u = multiply(input)
        processList(u, index + 4)
      case 3 =>
        val u = l.updated(l(index + 1), programInput)
        processList(u, index + 2)
      case 4 =>
        output(l, index, ocpm(2) == 0)
        processList(l, index + 2)
      case 5 =>
        val newIndex = jumpIfTrue(input)
        val updatedIndex = if(index == newIndex) {
          index + 3
        } else {
          newIndex
        }
        processList(l, updatedIndex)
      case 6 =>
        val newIndex = jumpIfFalse(input)
        val updatedIndex = if(index == newIndex) {
          index + 3
        } else {
          newIndex
        }
        processList(l, updatedIndex)
      case 7 =>
        val u = lessThan(input)
        processList(u, index + 4)
      case 8 =>
        val u = equal(input)
        processList(u, index + 4)
      case _ => l
    }
  }

  val res = processList(inputList, 0)


}
