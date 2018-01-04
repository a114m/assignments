package challenge

import scala.io.StdIn

object Main {

//  Assignment #1
  def factorial(n: Int): Int = {
    def calcFac(acc: Int, n: Int): Int = {
      if (n == 0) acc else calcFac(acc*n, n-1)
    }
    calcFac(1, n)
  }


//  Assignment #2
  def isPalindrome(word: String): Boolean = {
    word match {
      case w if w.length > 1 && w.head == w.last => isPalindrome(w.slice(1, w.length-1))
      case w if w.length <= 1 => true
      case _ => false
    }
  }


//  Assignment #3
  def runLengthEncode(text: String): String = {
    def doEncode(acc: String, lastChar: Option[Char], lastCharCount: Int, txt: List[Char]): String = txt match {
      case Nil => (acc :+ lastChar.getOrElse("") :+ {if (lastChar.nonEmpty) lastCharCount.toString else ""}).mkString
      case h :: t if h == lastChar.getOrElse(null) => doEncode(acc, Some(h), lastCharCount+1, t)
      case h :: t => doEncode((acc :+ lastChar.getOrElse("") :+ {if (lastChar.nonEmpty) lastCharCount.toString else ""}).mkString, Some(h), 1, t)
    }
    doEncode("", None, 1, text.toList)
  }

  def runLengthDecode(compressed: String): String = {
    ???  // TODO: on it :\
  }


// Assignment #4
  def compose(f: Int => Int, g: Int => Int)(n: Int): Int = {
    f(g(n))
  }

  def main(args: Array[String]): Unit = {
    println("Factorial input: ")
    println(factorial(StdIn.readInt()))

    println("isPalindrome input: ")
    println(isPalindrome(StdIn.readLine()))

    println("runLengthEncode input: ")
    println(runLengthEncode(StdIn.readLine()))
  }

}