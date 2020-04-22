object Medium {

  // https://leetcode.com/problems/group-anagrams/
  def groupAnagrams(strs: Array[String]): List[List[String]] = {
    type StringMap = Map[Char, Int]
    strs.foldLeft(Map[StringMap, List[String]]()) { (acc, word) =>
      val strMap = this.stringMap(word)
      acc + (strMap -> (acc.getOrElse(strMap, List[String]()) :+ word))
    }.values.toList
  }

  // https://leetcode.com/problems/reverse-words-in-a-string/
  def reverseWords(s: String): String = {
    val trimmed = s.trim
    trimmed.split(" ")
      .filter(_.nonEmpty)
      .foldLeft(Array[String]()) { (reversed, word) =>
        Array(word) ++ reversed
      }.mkString(" ")
  }

  // https://leetcode.com/problems/counting-bits/
  def countBits(num: Int): Array[Int] = {
    def countOnes(n: Int): Int =
      n.toBinaryString.map(_.asDigit).sum
    (0 to num).map(countOnes).toArray
  }

  // https://leetcode.com/problems/sort-characters-by-frequency/
  def frequencySort(s: String): String =
    stringMap(s).toList.sortBy {
      case (_, freq) => freq
    }(Ordering[Int].reverse).flatMap {
      case (c, freq) => (1 to freq).map(_ => c)
    }.mkString("")

  private[this] def stringMap(s: String): Map[Char, Int] = {
    s.foldLeft(Map[Char, Int]()) { (acc, c) =>
      acc + (c -> (acc.getOrElse(c, 0) + 1))
    }
  }
}
