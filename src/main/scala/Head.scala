package reinvent

object Head {
  def apply[A](input: List[A]): A = input match {
    case Nil ⇒ throw new RuntimeException("Head of empty list")
    case (h :: _) ⇒ h
  }

}

object Filter {

  //  Look at using Recursion, foldLeft, collect
  //  def apply[A](input: List[A], predicate: A ⇒ Boolean): List[A] = ???

  def apply[A](input: List[A], predicate: A ⇒ Boolean): List[A] = {
    def recurse(input: List[A], acc: List[A]): List[A] = {
      if (input.isEmpty) acc
      else if (predicate(input.head)) recurse(input.tail, acc :+ input.head)
      else recurse(input.tail, acc)
    }

    recurse(input, List())
  }

  def apply[A](input: List[A], predicate: A ⇒ Boolean): List[A] =
    for (a ← input if predicate(a)) yield a

  def apply[A](input: List[A], predicate: A ⇒ Boolean): List[A] =
    input.foldLeft(List[A]()) { (a: List[A], b: A) ⇒
      if (predicate(b)) b::a else a}.reverse

}
