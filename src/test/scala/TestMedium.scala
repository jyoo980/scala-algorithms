import org.scalatest.FunSpec

class TestMedium extends FunSpec {

  describe("Medium::groupAnagrams") {

    it("should group words if none are anagrams of each other") {
      val words = Array("hello", "world")
      assert(Medium.groupAnagrams(words) == List(List("hello"), List("world")))
    }

    it("should group anagrams together") {
      val words = Array("iamlordvoldemort", "tommarvoloriddle", "no", "on")
      assert(
        Medium.groupAnagrams(words) == List(
          List("iamlordvoldemort", "tommarvoloriddle"),
          List("no", "on")
        )
      )
    }
  }

  describe("Medium::reverseWords") {

    it("should handle an empty string") {
      assert(Medium.reverseWords("") == "")
    }

    it("should handle a single string (no spaces)") {
      assert(Medium.reverseWords("hello") == "hello")
    }

    it("should reverse a longer string (with spaces)") {
      assert(Medium.reverseWords("the sky is blue") == "blue is sky the")
    }
  }

  describe("Medium::countBits") {

    it("should handle a range of a single number") {
      assert(Medium.countBits(0).sameElements(Array(0)))
    }

    it("should handle a range of more than one number") {
      assert(Medium.countBits(3).sameElements(Array(0, 1, 1, 2)))
    }
  }

  describe("Medium::frequencySort") {

    it("should evaluate for strings that have the same sorted result") {
      assert(Medium.frequencySort("cccaaa") == "cccaaa")
    }

    it("should evaluate for case-sensitive strings") {
      assert(Medium.frequencySort("Aabb") == "bbAa")
    }

    it("should evaluate for a default case") {
      assert(Medium.frequencySort("tree") == "eetr")
    }
  }
}
