package introduction

object CheckBrackets {
  import scala.annotation.tailrec

  type Stack[T] = List[T]

  private val OpeningBrackets: Set[Char] = Set('(', '[', '{')
  private val ClosingBrackets: Set[Char] = Set(')', ']', '}')

  final case class OpenedBracket(bracket: Char, position: Int)

  private def doBracketsMatch(openingBracket: Char, closingBracket: Char): Boolean =
    (openingBracket == '(' && closingBracket == ')') ||
      (openingBracket == '[' && closingBracket == ']') ||
      (openingBracket == '{' && closingBracket == '}')

  private def retrieveFailedOpeningIndexFromStack(stack: Stack[OpenedBracket]): Option[Int] = stack match {
    case Nil => None
    case OpenedBracket(_, ix) :: _ => Some(ix)
  }

  def findIndexOfNonMatchingBracket(string: List[Char]): Option[Int] = {
    @tailrec
    def loop(stack: Stack[OpenedBracket],
             currentString: List[(Char, Int)]): (Stack[OpenedBracket], Option[Int]) = currentString match {
      case Nil => (stack, None)
      case (letter, ix) :: rest =>
        if (OpeningBrackets.contains(letter)) loop(OpenedBracket(letter, ix) :: stack, rest)
        else if (ClosingBrackets.contains(letter)) {
          stack match {
            case Nil => (stack, Some(ix))
            case OpenedBracket(openingBracket, _) :: stackTail =>
              if (doBracketsMatch(openingBracket, letter)) loop(stackTail, rest)
              else (stack, Some(ix))
          }
        } else loop(stack, rest)
    }

    val (remainingStack, failedIndex): (Stack[OpenedBracket], Option[Int]) = loop(Nil, string.zipWithIndex)
    failedIndex match {
      case None => retrieveFailedOpeningIndexFromStack(remainingStack)
      case _ => failedIndex
    }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val string: List[Char] = reader.next().toList
    findIndexOfNonMatchingBracket(string) match {
      case None => println("Success")
      case Some(ix) => println(ix + 1)
    }
  }
}
