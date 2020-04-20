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
}
