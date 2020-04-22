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

  describe("Easy::findLucky") {

    it("should evaluate to -1 if there is no lucky number") {
      val nums = Array(0)
      assert(Easy.findLucky(nums) == -1)
    }

    it("should evaluate to a correct lucky number") {
      val nums = Array(2, 2, 3, 4)
      assert(Easy.findLucky(nums) == 2)
    }

    it("should evaluate to the largest lucky number if there are more than one") {
      val nums = Array(1, 2, 2, 3, 3, 3)
      assert(Easy.findLucky(nums) == 3)
    }
  }

  describe("Easy::smallerNumbersThanCurrent") {

    it("should evaluate to an array of 0 given an array of the same numbers") {
      val nums = Array(1, 1, 1, 1, 1)
      assert(Easy.smallerNumbersThanCurrent(nums) == List(0, 0, 0, 0, 0))
    }

    it("should work for the general case") {
      val nums = Array(6, 5, 4, 8)
      assert(Easy.smallerNumbersThanCurrent(nums) == List(2, 1, 0, 3))
    }
  }

  describe("Easy::decompressRLE") {

    it("should work for an inefficient representation") {
      val nums = Array(1, 1, 1, 2, 1, 3)
      assert(Easy.decompressRLE(nums).sameElements(Array(1, 2, 3)))
    }

    it("should work for an efficient representation") {
      val nums = Array(3, 1, 2, 5)
      assert(Easy.decompressRLE(nums).sameElements(Array(1, 1, 1, 5, 5)))
    }
  }

  describe("Easy::addDigits") {

    it("should work for the base case") {
      val num = 1
      assert(Easy.addDigits(num) == 1)
    }

    it("should work for two-step numbers") {
      val num = 38
      assert(Easy.addDigits(num) == 2)
    }
  }

  describe("Easy::firstUniqueChar") {

    it("should work for a string with one character") {
      assert(Easy.firstUniqueChar("h") == 0)
    }

    it("should evaluate to -1 if there are no unique characters") {
      assert(Easy.firstUniqueChar("aa") == -1)
    }

    it("should evaluate to the index of a unique character") {
      assert(Easy.firstUniqueChar("loveleetcode") == 2)
    }
  }

  describe("Easy::sortArrayByParity") {

    it("should work for just even numbers") {
      val nums = (1 to 10).filter(_ % 2 == 0).toArray
      assert(Easy.sortArrayByParity(nums).sameElements(nums))
    }

    it("should work for just odd numbers") {
      val nums = (1 to 10).filter(_ % 2 != 0).toArray
      assert(Easy.sortArrayByParity(nums).sameElements(nums))
    }

    it("should work for a mix of even and odd numbers") {
      val nums = (1 to 10).toArray
      assert(Easy.sortArrayByParity(nums).sameElements(nums.filter(_ % 2 == 0) ++ nums.filter(_ % 2 != 0)))
    }
  }

  describe("Easy::replaceElements") {

    it("should maintain the same array with itself") {
      assert(Easy.replaceElements(Array(2)).sameElements(Array(-1)))
    }

    it("should replace elements for a longer array") {
      assert(Easy.replaceElements(Array(17, 18, 5, 4, 6, 1)).sameElements(Array(18, 6, 6, 6, 1, -1)))
    }
  }

  describe("Easy::selfDividingNumbers") {

    it("should reject 0 as a self diving number") {
      assert(Easy.selfDividingNumbers(0, 1) == List(1))
    }

    it("should evaluate to a range of valid self-dividing numbers from 1 to 22") {
      val selfDividingNums = Easy.selfDividingNumbers(1, 22)
      assert(selfDividingNums == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 15, 22))
    }
  }

  describe("Easy::sortByBits") {

    it("should evaluate to the same array given [10000, 10000]") {
      assert(Easy.sortByBits(Array(10000, 10000)).sameElements(Array(10000, 10000)))
    }

    it("should evaluate for larger arrays") {
      assert(
        Easy.sortByBits(Array(2, 3, 5, 7, 11, 13, 17, 19))
          .sameElements(Array(2, 3, 5, 17, 7, 11, 13, 19))
      )
    }
  }
}
