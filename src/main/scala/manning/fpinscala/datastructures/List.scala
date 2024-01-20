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
    
    // access the nth element of a (0-indexed) list
    // EFFECTS: throws an OutOfBoundsException if an out of bound
    //          access is made
    def get[A](l: List[A], n: Int): A = 
        l match {
            case Nil => throw new OutOfBoundsException
            case Cons(h, t) => if (n == 0) h
                               else get(t, n - 1)
        }

    // the apply function, enable constructing a List without 
    // use of the `new` keyword
    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

    // EX 3.2
    // implement the function tail for removing the first element of a List
    // EFFECTS: throws an EmptyListException if given list is empty
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
    // EFFECTS: throws an EmptyListException if either initial list is empty or an attempt to 
    //          remove too many (more than there are in the list) is made
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
    // EFFECTS: throws an EmptyListException is given list is empty
    def pop[A](l: List[A]): List[A] = l match {
        case Nil => throw new EmptyListException
        case Cons(_, Nil) => Nil
        case Cons(x, xs) => Cons(x, pop(xs))
    }

    // EX 3.12
    def reverse[A](l: List[A]): List[A] =
        foldLeft(l, Nil: List[A])((x, acc) => Cons(acc, x))

    // EX 3.13
    // this gives a tail-recursive fold right
    def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
        foldLeft(reverse(l), z)((b, a) => f(a, b))

    // this is confusing
    // and also isn't stack-safe - to see why notice that it keeps building up a
    // context of lambdas 
    def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = 
        foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

    // EX 3.14
    // append an element to a list
    def append[A](l: List[A], r: A): List[A] = 
        foldRight(l, Cons(r, Nil))(Cons(_,_))

    // EX 3.15
    // concatenate a list of lists
    def concat[A](ll: List[List[A]]): List[A] =
        ll match {
            case Nil => Nil 
            case Cons(h, t) => foldRight(h, concat(t))(Cons(_,_))
        }
    
    // EX 3.18
    def map[A, B](as: List[A])(f: A => B): List[B] =
        foldRightViaFoldLeft(as, Nil: List[B])((h,t) => Cons(f(h), t))

    // EX 3.19
    def filter[A](as: List[A])(f: A => Boolean): List[A] = 
        foldRightViaFoldLeft(as, Nil: List[A])((h, acc) => if (f(h)) Cons(h, acc) else acc)

    // EX 3.20
    def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
        concat(map(as)(f))

    // EX 3.21
    def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
        flatMap(l)(a => if (f(a)) List(a) else Nil)

    // EX 3.23
    def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A,B) => C): List[C] =
        (l1, l2) match {
            case (Nil, _) => Nil
            case (_, Nil) => Nil
            case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
        }
 }
