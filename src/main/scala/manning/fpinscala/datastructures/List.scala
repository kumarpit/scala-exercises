package manning.fpinscala.datastructures

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

object List { // List companion object, contains functions for creating
              // working with lists
    
    // foldr implementation 
    // not stack-safe
    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
        as match {
            case Nil => z
            case Cons(x, xs) => f(x, foldRight(xs, z)(f)) // not tail recursive!!!
        }
    
    // foldl implementation
    @annotation.tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
        as match {
            case Nil => z
            case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
        }

    // returns the length of the list
    def len[A](l: List[A]): Int = {
        foldLeft(l, 0)((acc, _) => acc + 1)
    }

    // sum the elements of a list of ints
    def sum(ints: List[Int]): Int =
        foldLeft(ints, 0)(_ + _)
    

    // multiplies the elements of a list of ints
    def product(ds: List[Double]): Double = 
        foldLeft(ds, 1.0)(_ * _) 
    
    
    // the apply function, enable constructing a List without 
    // use of the `new` keyword
    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

    // EX 3.2
    // implement the function tail for removing the first element of a List
    def tail[A](as: List[A]): List[A] = as match {
        case Nil => throw new EmptyListException 
        case Cons(_, xs) => xs
    }

    // EX 3.3
    // replaces the first element of the a List
    // with a different value
    def setHead[A](as: List[A], nh: A): List[A] = as match {
        case Nil => Nil 
        case Cons(x, xs) => Cons(nh, xs)
    }

    // EX 3.3
    // generalizes tail to the function drop which removes the first n elements from a 
    // list
    def drop[A](l: List[A], n: Int): List[A] = {
        if (n == 0) l
        else l match {
            case Nil => throw new EmptyListException
            case Cons(x, xs) => drop(xs, n - 1)
        } 
    }

    // EX 3.5
    // removes elements from the list prefix as long as they match a predicate
    // we need to curry the implementation to maximize type inferrence
    def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = 
        l match {
            case Cons(x, xs) if f(x) => dropWhile(xs)(f)  // like #:when in Racket
            case _ => l
        }
    // not curried version
    // def dropWhile2[A](l: List[A], f: A => Boolean): List[A] = 
    // l match {
    //     case Cons(x, xs) if f(x) => dropWhile2(xs, f)  // like #:when in Racket
    //     case _ => l
    // }

    // EX 3.6
    // remove the last element of a list
    def init[A](l: List[A]): List[A] = l match {
        case Nil => throw new EmptyListException
        case Cons(_, Nil) => Nil
        case Cons(x, xs) => Cons(x, init(xs))
    }

    def reverse[A](l: List[A]): List[A] =
        foldLeft(l, Nil: List[A])((x, acc) => Cons(acc, x))
}
