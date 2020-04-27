import scala.annotation.tailrec
import scala.collection.immutable.ListMap

object Easy {

  // https://leetcode.com/problems/number-of-segments-in-a-string/
  def countSegments(s: String): Int =
    s.split(" ").count(_.nonEmpty)

  // https://leetcode.com/problems/find-numbers-with-even-number-of-digits/
  def findNumbers(nums: Array[Int]): Int = {
    def evenNumDigits(n: Int): Boolean = {
      n.toString.length % 2 == 0
    }
    nums.count(evenNumDigits)
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
    val numFreq = nums.foldLeft(Map[Int, Int]())(counts)
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

  // https://leetcode.com/problems/replace-elements-with-greatest-element-on-right-side/
  def replaceElements(arr: Array[Int]): Array[Int] = {
    val arrWithIndex = arr.zipWithIndex
    arrWithIndex.map {
      case (_, i) =>
        val rightArr = arr.takeRight(arr.length - i - 1)
        if (rightArr.nonEmpty) rightArr.toList.max
        else -1
    }
  }

  // https://leetcode.com/problems/self-dividing-numbers/
  def selfDividingNumbers(left: Int, right: Int): List[Int] = {
    def isSelfDividing(n: Int): Boolean =
      n.toString.map(_.asDigit).forall(d => d != 0 && n % d == 0)
    (left to right).filter(isSelfDividing).toList
  }

  def sortByBits(nums: Array[Int]): Array[Int] = {
    def countOnes(n: Int): Int =
      n.toBinaryString.map(_.asDigit).sum
    nums.map(n => (n, countOnes(n)))
      .sortBy {
        case (n, numOnes) => (numOnes, n)
      }
      .map {
        case (n, _) => n
      }
  }

  // https://leetcode.com/problems/max-consecutive-ones/
  def findMaxConsecutiveOnes(nums: Array[Int]): Int = {
    def max(a: Int, b: Int): Int =
      if (a > b) a else b
    @tailrec
    def rec(nums: List[Int], currMax: Int, onesSeen: Int): Int = nums match {
      case Nil => max(currMax, onesSeen)
      case h :: t =>
        if (h == 1) rec(t, max(currMax, onesSeen + 1), onesSeen + 1)
        else rec(t, currMax, 0)
    }
    rec(nums.toList, 0, 0)
  }

  // https://leetcode.com/problems/fibonacci-number/
  def fib(n: Int): Int = {
    @tailrec
    def rec(n: Int, prev: Int, curr: Int): Int =
      if (n == 0) prev
      else rec(n - 1, curr, prev + curr)
    rec(n, 0 ,1)
  }

  // https://leetcode.com/problems/unique-number-of-occurrences/
  def uniqueOccurrences(nums: Array[Int]): Boolean = {
    val freq = nums.foldLeft(Map[Int, Int]())(counts)
    val occurrences = freq.values.toList
    val uniqueOccurrences = occurrences.toSet
    occurrences.length == uniqueOccurrences.size
  }

  // https://leetcode.com/problems/perfect-number/
  def checkPerfectNumber(num: Int): Boolean =
    if (num != 0) (1 until num).filter(num % _ == 0).sum == num
    else false

  // https://leetcode.com/problems/majority-element/
  def majorityElement(nums: Array[Int]): Int =
    nums.foldLeft(Map[Int, Int]())(counts)
      .collectFirst {
        case (n, freq) if freq > nums.length.toDouble / 2 => n
      }.getOrElse(-1)

  // https://leetcode.com/problems/number-of-steps-to-reduce-a-number-to-zero/
  def numberOfSteps(n: Int): Int = {
    @tailrec
    def rec(n: Int, steps: Int): Int =
      if (n == 0) steps
      else rec(if (n % 2 == 0) n / 2 else n - 1, steps + 1)
    rec(n, 0)
  }

  // https://leetcode.com/problems/intersection-of-two-arrays-ii/
  def intersect(nums1: Array[Int], nums2: Array[Int]): Array[Int] = {
    val num1Freq = nums1.foldLeft(Map[Int, Int]())(counts)
    val num2Freq = nums2.foldLeft(Map[Int, Int]())(counts)
    num1Freq.filter {
      case (n, _) => num2Freq.contains(n)
    }.flatMap {
      case (n, freqInFirst) =>
        val commonFreq = Math.min(freqInFirst, num2Freq.getOrElse(n, freqInFirst))
        (1 to commonFreq).map(_ => n)
    }.toArray
  }

  // https://leetcode.com/problems/string-matching-in-an-array/
  def stringMatching(words: Array[String]): List[String] =
    words.filter { prefix =>
      words.exists(word => word.contains(prefix) && word != prefix)
    }.toList

  // https://leetcode.com/problems/squares-of-a-sorted-array/
  def sortedSquares(A: Array[Int]): Array[Int] =
    A.map(n => n * n).sorted

  // https://leetcode.com/problems/detect-capital/
  def detectCapitalUse(word: String): Boolean = {
    if (word.length < 1) false
    else if (word(0).isUpper) {
      word.substring(1).forall(_.isLower) || word.forall(_.isUpper)
    } else {
      word.forall(_.isLower)
    }
  }

  // https://leetcode.com/problems/find-the-difference/
  def findTheDifference(s: String, t: String): Char = {
    def charFreq(acc: Map[Char, Int], c: Char): Map[Char, Int] =
      acc + (c -> (acc.getOrElse(c, 0) + 1))
    val sFreq = s.foldLeft(Map[Char, Int]())(charFreq)
    val tFreq = t.foldLeft(Map[Char, Int]())(charFreq)
    tFreq.collectFirst {
      case (c, freq) if sFreq.getOrElse(c, -1) != freq => c
    }.getOrElse(sys.error(s"$s and $t are identical strings"))
  }

  // https://leetcode.com/problems/happy-number/
  def isHappy(n: Int): Boolean = {
    @tailrec
    def rec(n: Int, visited: List[Int]): Boolean = {
      if (n == 1) true
      else if (visited.contains(n)) false
      else {
        val next = n.toString.map(num => num.asDigit * num.asDigit).sum
        rec(next, visited :+ n)
      }
    }
    rec(n, Nil)
  }

  // https://leetcode.com/problems/binary-number-with-alternating-bits/
  def hasAlternatingBits(n: Int): Boolean = {
    @tailrec
    def rec(bits: List[Int], prevBit: Int): Boolean = bits match {
      case Nil => true
      case h :: t =>
        if (h == prevBit) false
        else rec(t, h)
    }
    rec(n.toBinaryString.map(_.asDigit).toList, -1)
  }

  private[this] def buildFreqMap(acc: Map[Char, IndexFreq], charPair: (Char, Int)): Map[Char, IndexFreq] =
    charPair match {
      case (c, i) =>
        val count = acc.get(c).fold((i, 1)) {
          case (idx , freq) => (idx, freq + 1)
        }
        acc + (c -> count)
    }

  private[this] def counts(acc: Map[Int, Int], n: Int): Map[Int, Int] =
    acc + (n -> (acc.getOrElse(n, 0) + 1))
}
