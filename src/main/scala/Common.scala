object Common {

  def stringMap(s: String): Map[Char, Int] = {
    s.foldLeft(Map[Char, Int]()) { (acc, c) =>
      acc + (c -> (acc.getOrElse(c, 0) + 1))
    }
  }
}
