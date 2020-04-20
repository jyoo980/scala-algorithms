object Easy {
  // https://leetcode.com/problems/number-of-segments-in-a-string/
  def countSegments(s: String): Int =
    s.split(" ").collect {
      case seg if seg.nonEmpty => 1
    }.sum
}
