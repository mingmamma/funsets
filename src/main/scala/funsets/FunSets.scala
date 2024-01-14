package funsets

/**
 * 2. Purely Functional Sets.
 */
trait FunSets extends FunSetsInterface:
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  override type FunSet = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: FunSet, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): FunSet = (otherInt:Int) => otherInt==elem
    
    // Equivalent to the anonymous function
    // def helper(otherInt:Int) = otherInt==elem
    // helper


  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: FunSet, t: FunSet): FunSet = (someInt:Int) => s(someInt) || t(someInt)

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: FunSet, t: FunSet): FunSet = (someInt:Int) => s(someInt) && t(someInt)

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: FunSet, t: FunSet): FunSet = (someInt:Int) => s(someInt) && !t(someInt)

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: FunSet, p: Int => Boolean): FunSet = intersect(s, p)


  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: FunSet, p: Int => Boolean): Boolean =
    def iter(a: Int): Boolean =
      if (s(a) && !p(a)) then
        false
      else if a > bound then 
        true
      else
        iter(a+1)
    iter(-bound)

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: FunSet, p: Int => Boolean): Boolean = 
    // def iter(a: Int): Boolean =
    //   if (s(a) && p(a)) then
    //     true
    //   else if a > bound then
    //     false
    //   else
    //     iter(a+1)
    // iter(-bound)

    // The grader asks for an implementation of exists in terms of forall
    // Hence there exists an element in s satisfying predicate p <=> Not all elements in s satisfying p's negation
    // Damn it! I had to look at somebody's else code to realize the way to express negated p: x => !p(x), given p: Int => Boolean, 
    !forall(s, x => !p(x))

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: FunSet, f: Int => Int): FunSet = 
    
    // Incorrect idea: This defined a set of Ints x s.t. f(x) is in s 
    // (someInt: Int) => s(f(someInt))

    // Correct idea 1: one conceptually correct representation, though not viable, is the following
    // Given the transformation f, say if we could obtain the inversion of f, denoted by f`,
    // Then the set obtained by applying f to s, can be decribed as the set of Ints, y
    // s.t. f`(y) is an element of s. The (hypothetical) code would be: (someInt: Int) => s(f`(someInt))

    // Correct idea 2: viable representation of this relation is that the element of the set by applying f to s
    // can be decribed as some Ints y s.t there EXISTS an Int x in s s.t. f(x) == y, hence making use the exists
    // implementing map
    (someInt: Int) => exists(s, (x: Int) => someInt == f(x))

  /**
   * Displays the contents of a set
   */
  def toString(s: FunSet): String =
    val xs = for i <- (-bound to bound) if contains(s, i) yield i
    xs.mkString("{", ",", "}")

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: FunSet): Unit =
    println(toString(s))

object FunSets extends FunSets
