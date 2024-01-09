package funsets

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite extends munit.FunSuite:

  import FunSets.*

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets:
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

  /**
   * This test is currently disabled (by using .ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * .ignore annotation.
   */

  test("singleton set one contains one") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets:
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(!contains(s1,2), "Singleton only contains one value")
  }

  test("union contains all elements of each set") {
    new TestSets:
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
  }

    test("intersect contains only elements belonging to all sets") {
    new TestSets:
      val s = union(s1, s2)
      assert(contains(intersect(s,s1), 1), "Intersect of s1 and union should contain 1")
      assert(!contains(intersect(s,s1), 2), "Should not contain others from the union")
      assert(!contains(intersect(s,s1), 3), "Should not contain any others")
  }

      test("diff contains the elements correct difference") {
    new TestSets:
      val s = union(s1, s2)
      assert(contains(diff(s,s1), 2), "Diff of s1 from union should contain 2")
      assert(!contains(diff(s,s1), 1), "Should not contain 1")
      assert(!contains(diff(s,s1), 3), "Should not contain any others")
  }

  test("filter contains correctly filtered elements") {
    new TestSets:
      val s = union(s1, s2)
      assert(contains(filter(s, (x: Int)=>x%2==0), 2), "filter of union to even number should contain 2")
      assert(!contains(filter(s, (x: Int)=>x%2==0), 1), "filter of union to even number should contain 2")
      assert(!contains(filter(s, (x: Int)=>x%2==0), 3), "Should not contain any others")
  }

  test("forAll checks if all elements in the FunSet satisfy the predicate") {
    new TestSets:
      assert(forall((x: Int) => true, (x: Int) => x<100 & x> -100)==false, 
      "FuncSet of all elements bounded by +/-1000 are NOT all bounded by +/-100")
      
      assert(forall((x: Int) => true, (x: Int) => x<2000 & x> -2000)==true, 
      "FuncSet of all elements bounded by +/-1000 are all bounded by +/-2000")
  }  

  test("exist checks if any element in the FunSet satisfies the predicate") {
    new TestSets:
      assert(exists((x: Int) => x%100==0&&x!=0, (x: Int) => x%1000==0)==true, 
      "FuncSet of the non-zero hundreds bounded by +/-1000 contains an element divisible by 1000")
      
      assert(exists((x: Int) => x%100==0&&x!=0, (x: Int) => x%11==0)==false, 
      "FuncSet of the non-zero hundreds bounded by +/-1000 does not contain an element divisible by 11")
  }

  test("map transforms the elements in the FunSet") {
    new TestSets:
      assert(exists(map((x: Int) => x>0, (x: Int) => -x), (x: Int) => x>=0)==false, 
      "FuncSet of positive integers by the negation map does NOT contain any gte 0")
      
      assert(forall(map((x: Int) => x>0, (x: Int) => -x), (x: Int) => x<0)==true,
      "FuncSet of positive integers by the negation map contain only negatives")
  }  

  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
