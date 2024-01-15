package manning.fp_in_scala.c3_functional_data_structures

import org.scalatest.funspec.AnyFunSpec

class ListSpec extends AnyFunSpec {
  describe("A List") {
    it("should build correctly") {
      assert(List(1,2,3) == Cons(1, Cons(2, Cons(3, Nil))))
      assert(List[Int](1,2,'a') == Cons(1, Cons(2, Cons('a', Nil))))
    }

    it("should sum correctly") {
      assertDoesNotCompile(List.sum(List(1,2,3)))
    }  
  }
}
