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
}
