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

    it("should be able to access nth element") {
      val list = List[Int](1,2,3)
      assert(List.get(list, 0) == 1)
      assert(List.get(list, 1) == 2)
      assert(List.get(list, 2) == 3)
      assertThrows[OutOfBoundsException] {
        List.get(list, 3)
      }
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
      val nlist = List.pop(List(1,2,3))
      assert(List.len(nlist) == 2)
      assert(List.get(nlist, 0) == 1)
      assert(List.get(nlist, 1) == 2)
    }

    it("should be able to reverse") {
      assert(List.reverse(List(1,2,3)) == List(3,2,1))
    }

    it("should be able to append an element to a list") {
      val list = List[Int](1,2,3)
      val nlist = List.append(list, 4)
      assert(List.len(nlist) == 4)
      assert(List.get(nlist, 3) == 4)
      assertTypeError("List.append[Int](list, \"a\")")
    }

    it("should be able to concatenate a list of lists") {
      val list1 = List(1,2,3)
      val list2 = List(3,4,5)
      val list3 = List("a", "b", "c")
      val ll = List(list1, list2, list3)
      val  l = List.concat(ll)
      assert(l == List(1,2,3,3,4,5,"a","b","c"))
      assert(List.concat(List(List(1), Nil, List(2))) == List(1,2))
    }

    it("should be able to map") {
      val list = List(1,2,3)
      val mlist = List.map(list)(_+1)
      assert(List.len(mlist) == List.len(list))
      assert(mlist == List(2,3,4))
    }

    it("should be able to filter") {
      val list = List(1,2,3,4,5,6)
      val elist = List.filter(list)(_%2 == 0)
      val elist2 = List.filterViaFlatMap(list)(_%2 == 0)
      assert(elist  == List(2,4,6))
      assert(elist2 == List(2,4,6))
    }

    it("should be able to flatMap") {
      assert(List.flatMap(List(1,2,3))(i => List(i,i)) == List(1,1,2,2,3,3))
    }

    it("should be able to zip two lists") {
      assert(List.zipWith(List(1,2,3), List(4,5,6))(_+_) == List(5,7,9))
    }
  }
}
