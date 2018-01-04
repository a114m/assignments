package challenge

import org.scalatest._
import Main.{factorial, isPalindrome, runLengthEncode, runLengthDecode, compose}

class MainSpec extends FlatSpec with Matchers {

  "factorial" should "calculate factorial for an integer" in {
    factorial(3) should be (6)
    factorial(5) should be (120)
  }

  "isPalindrome" should "tell if word is palindrome" in {
    isPalindrome("anna") should be (true)
    isPalindrome("apple") should be (false)
  }

  "runLengthEncode" should "compress a string and return an encoded version" in {
    runLengthEncode("aaaaaaaaaabbbaxxxxyyyzyx") should be ("a10b3a1x4y3z1y1x1")
  }

  "runLengthEncode" should "decode a compressed string returning the original version" in {
    runLengthDecode("a10b3a1x4y3z1y1x1") should be ("aaaaaaaaaabbbaxxxxyyyzyx")
  }

  "compose" should "compose two function into one that applies the first one to the output of the second one" in {
    def square(n: Int): Int = n * n
    def inc(n: Int): Int = n + 1
    def h(n: Int): Int = compose(square, inc)(n)

    h(6) should be (49)
  }
}
