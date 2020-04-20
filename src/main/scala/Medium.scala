object Medium {

  // https://leetcode.com/problems/group-anagrams/
  def groupAnagrams(strs: Array[String]): List[List[String]] = {
    type StringMap = Map[Char, Int]
    def toMap(s: String): StringMap = {
      s.foldLeft(Map[Char, Int]()) { (acc, c) =>
        acc + (c -> (acc.getOrElse(c, 0) + 1))
      }
    }
    strs.foldLeft(Map[StringMap, List[String]]()) { (acc, word) =>
      val strMap = toMap(word)
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

}
