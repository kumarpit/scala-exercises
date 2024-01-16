package manning.fpinscala.datastructures

import org.scalatest.funspec.AnyFunSpec


class ListSpec extends AnyFunSpec {
  describe("A List") {
    it("should build correctly") {
      assert(List(1,2,3) == Cons(1, Cons(2, Cons(3, Nil))))
      assertTypeError("List[Int](1,2,\"a\")")
    }

    it("shuold return its length") {
      assert(List.len(Nil) == 0)
      assert(List.len(List(1)) == 1)
      assert(List.len(List(1,2)) == 2)
    }

    it("should be able to sum its elements") {
      assert(List.sum(List[Int](1,2,3)) == 1 + 2 + 3)
      assert(List.sum(List[Int](-23, 89, 1029)) == -23 + 89 + 1029)
      assertTypeError("List.sum(List(1,2,\"a\"))")
    }  

    it("should be able to calculuate the product of elements") {
      assert(List.product(List(1,2,3)) == 1 * 2 * 3)
      assert(List.product(List(20,3,9)) == 20 * 3 * 9)
      assertTypeError("List.sum(List(1,2,\"a\"))")
    }

    it("should be able to remove the tail element") {
      assertThrows[EmptyListException] {
        List.tail(Nil)
      }
      assert(List.tail(List(1,2,3)) match {
        case Nil => false
        case Cons(x, xs) => x == 2
      })
    }

    it("should be able to replace the head element") {
      assert(List.setHead(List(1,2,3), 4) match {
        case Nil => false
        case Cons(x, _) => x == 4
      })

      assert(List.setHead(Nil, 4) match {
        case Nil => true
        case Cons(_, _) => false
      })
    } 

    it("should be able to dropWhile correctly") {
      val xs: List[Int] = List(1,2,3,4,5)
      // because dropWhile is curried, we do not need to specify the type
      // of the parameter to the annonymous function
      val ex1 = List.dropWhile(xs)(x => x < 4)
      // val ex2 = List.dropWhile2(xs, x => x < 4)  throws a missing param type error
      assert(List.len(ex1) == 2)
    }

    it("should be able to remove the last element") {
      // TODO!!!
    }

    it("should be able to reverse") {
      assert(List.reverse(List(1,2,3)) == List(3,2,1))
    }
  }
}
