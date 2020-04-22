import scala.annotation.tailrec
import scala.collection.immutable.ListMap

object Easy {

  // https://leetcode.com/problems/number-of-segments-in-a-string/
  def countSegments(s: String): Int =
    s.split(" ").collect {
      case seg if seg.nonEmpty => 1
    }.sum

  // https://leetcode.com/problems/find-numbers-with-even-number-of-digits/
  def findNumbers(nums: Array[Int]): Int = {
    def evenNumDigits(n: Int): Boolean = {
      n.toString.length % 2 == 0
    }
    nums.collect {
      case n if evenNumDigits(n) => 1
    }.sum
  }

  // https://leetcode.com/problems/two-sum/
  def twoSum(nums: Array[Int], target: Int): Option[(Int, Int)] = {
    val indexedNums: List[(Int, Int)] = nums.zipWithIndex.toList
    val diffMap = indexedNums.foldLeft(Map[Int, Int]()) { (acc, tup) =>
      tup match {
        case (n, i) => acc + ((target - n) -> i)
      }
    }
    indexedNums.collectFirst {
      case (n, i) if diffMap.getOrElse(n, -1) != -1 =>
        (i, diffMap.get(n).fold(-1)(j => j))
    }
  }

  // https://leetcode.com/problems/find-lucky-integer-in-an-array/
  def findLucky(nums: Array[Int]): Int = {
    val numFreq = nums.foldLeft(Map[Int, Int]()) { (acc, n) =>
      acc + (n -> (acc.getOrElse(n, 0) + 1))
    }
    val luckyNumbers = nums.collect {
      case n if numFreq.getOrElse(n, -1) == n => n
    }
    if (luckyNumbers.nonEmpty) luckyNumbers.max
    else -1
  }

  // https://leetcode.com/problems/how-many-numbers-are-smaller-than-the-current-number/
  def smallerNumbersThanCurrent(nums: Array[Int]): List[Int] =
    nums.map(n => nums.count(_ < n)).toList

  // https://leetcode.com/problems/decompress-run-length-encoded-list/
  def decompressRLE(nums: Array[Int]): Array[Int] = {
    def toPairs(xs: Array[Int]): List[(Int, Int)] = {
      xs.toList.grouped(2).map {
        case List(x, y) => (x, y)
        case _ => sys.error(s"Uneven length list: $xs")
      }.toList
    }
    toPairs(nums).flatMap {
      case (freq, n) => (1 to freq).map(_ => n)
    }.toArray
  }

  // https://leetcode.com/problems/add-digits/
  @tailrec
  def addDigits(num: Int): Int = {
    val numStr = num.toString
    if (numStr.length == 1) num
    else addDigits(numStr.map(_.asDigit).sum)
  }

  type IndexFreq = (Int, Int)

  // https://leetcode.com/problems/first-unique-character-in-a-string/
  def firstUniqueChar(s: String): Int = {
    val freqMap = s.zipWithIndex.foldLeft(Map[Char, IndexFreq]())(buildFreqMap)
    freqMap.toList.sortBy {
      case (_, indexFreq) => indexFreq match {
        case (i, _) => i
      }
    }.collectFirst {
      case (_, indexFreq) if {
        indexFreq match {
          case (_, freq) => freq == 1
        }
      } => indexFreq match {
        case (index, _) => index
      }
    }.getOrElse(-1)
  }

  // https://leetcode.com/problems/sort-array-by-parity/
  def sortArrayByParity(A: Array[Int]): Array[Int] = {
    @tailrec
    def rec(arr: List[Int], evens: List[Int], odds: List[Int]): Array[Int] =
      arr match {
        case Nil => (evens ++ odds).toArray
        case h :: t =>
          if (h % 2 == 0) rec(t, evens :+ h, odds)
          else rec(t, evens, odds :+ h)
      }
    rec(A.toList, Nil, Nil)
  }

  private[this] def buildFreqMap(acc: Map[Char, IndexFreq], charPair: (Char, Int)): Map[Char, IndexFreq] =
    charPair match {
      case (c, i) =>
        val count = acc.get(c).fold((i, 1)) {
          case (idx , freq) => (idx, freq + 1)
        }
        acc + (c -> count)
    }
}
