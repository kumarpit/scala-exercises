package manning.fp_in_scala.c3_functional_data_structures


/*
* List is one of:
* - Nil
* - Cons(A, List[A])
* interp. a singly-linked list of elements of type A
*         if A is a sub-type of B, then List[A] is a subtype
*         of List[B]; i.e A is a variant type
*/
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    /*
    * sums the elements (int) of ints
    */
    def sum(ints: List[Int]): Int = ints match {
        case Nil => 0
        case Cons(x, xs) => x + sum(xs)
    }

    /*
    * multiplies the elements (int) of ints
    */
    def product(ds: List[Double]): Double = ds match {
        case Nil => 1.0
        case Cons(0.0, xs) => 0.0
        case Cons(x, xs) => x * product(xs)  
    }

    /*
    * the apply function, enable constructing a List without 
    * use of the `new` keyword
    */
    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

    // EX 3.2
    // implement the function tail for removing the first element of a List
    def tail[A](as: List[A]): A = as match {
        case Nil => throw new Exception 
        case Cons(x, xs) => x
    }

    // EX 3.3
    // implement the function setHead for replacing the first element of the a List
    // with a different value
    def setHead[A](as: List[A], nh: A): List[A] = as match {
        case Nil => Nil 
        case Cons(x, xs) => Cons(nh, xs)
    }

    // EX 3.3
    // generalize tail to the function drop which removes the first n elements from a 
    // list
    // def drop[A](l: List[A], n: Int): List[A] = ???
}
