abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

class NonEmptySet(i: Int, left: IntSet, right: IntSet) extends IntSet {
  override def contains(x: Int): Boolean =
    if (x < i) left contains x
    else if (x > i) right contains x
    else x == i

  override def incl(x: Int): IntSet =
    if (x < i) new NonEmptySet(i, left incl x, right)
    else if (x > i) new NonEmptySet(i, left, right incl x)
    else this

  override def toString: String = "{ " + left + ", " + i + ", " + right + " }"

  override def union(other: IntSet): IntSet =
    left union right union other incl i
}

object EmptySet extends IntSet {
  override def contains(x: Int): Boolean = false

  override def incl(x: Int): IntSet = new NonEmptySet(x, EmptySet, EmptySet)

  override def toString: String = "{None}"

  override def union(other: IntSet): IntSet = other
}


def main(args: Array[String]): Unit = {
  val currentSet = new NonEmptySet(2, EmptySet, EmptySet)
  println(currentSet)

  println("-----")
  val newSet = currentSet incl 10 incl 20
  println(newSet)

  println("-----")
  val out = currentSet union newSet
  println(out)

  println("-----")
  val s3 = new NonEmptySet(30, EmptySet, EmptySet)
  println(out union s3)

  println("-----")
  val s4 = new NonEmptySet(0, EmptySet, EmptySet)
  println(out union s3 union s4)

}

main(Array("this", "does"))
