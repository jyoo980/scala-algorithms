import org.scalatest.FunSpec

class TestEasy extends FunSpec {

  describe("Easy::countSegments") {

    it("should handle an empty string") {
      assert(Easy.countSegments("") == 0)
    }

    it("should handle a string with 1 segment") {
      assert(Easy.countSegments("apple") == 1)
    }

    it("should handle a string with more than 1 segment") {
      assert(Easy.countSegments("hello world") == 2)
    }
  }

  describe("Easy::findNumbers") {

    it("should handle an empty array") {
      assert(Easy.findNumbers(Array()) == 0)
    }

    it("should handle arrays with numbers that all have have an odd number of digits") {
      assert(Easy.findNumbers(Array(1, 555, 12323)) == 0)
    }

    it("should handle arrays with a few numbers that have an even number of digits") {
      assert(Easy.findNumbers(Array(11, 3333, 5, 67546)) == 2)
    }
  }

  describe("Easy::twoSum") {

    it("should evaluate to None if there are two numbers that do not add to target") {
      val nums = Array(5, 4, 3, 0, 23)
      assert(Easy.twoSum(nums, -1) match {
        case Some(_) => false
        case None => true
      })
    }

    it("should evaluate to Some if there are two numbers that do add to target") {
      val nums = Array(4, 6, 11, 12, 9)
      assert(Easy.twoSum(nums, 23) match {
        case Some(indices) => indices == (2, 3)
        case None => false
      })
    }
  }
}
