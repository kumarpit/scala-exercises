package manning.fpinscala.laziness

sealed trait Stream[+A]
case object Empty extends Stream[Nothing]  // singleton
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
    // smart constructor, caches hd, tl values
    // the arrow => before a type indicates that this is a lazy parameter
    // i.e unevaluated parameters
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
        lazy val head = hd;
        lazy val tail = tl;
        Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = 
        if (as.isEmpty) empty[A]
        else cons(as.head, apply(as.tail: _*))

    def take[A](as: Stream[A], n: Int): Stream[A] = as match {
        case Cons(h, t) if n > 1 => cons(h(), take[A](t(), n - 1))
        case Cons(h, _) if n == 1 => cons(h(), Empty)
        case _ => empty[A]
    }
}

