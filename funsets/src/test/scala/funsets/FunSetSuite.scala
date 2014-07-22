package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

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

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }
  
  test("intersect contains common elements") {
    new TestSets {
      val i1: Set = x => x >= 10 && x < 100
      val i2: Set = x => x >= 90 && x < 120
      val i = intersect(i1, i2)

      assert(contains(i, 90), "90 is in intersect(i1, i2)")
      assert(contains(i, 99), "99 is in intersect(i1, i2)")
      assert(!contains(i, 9), "9 is not in intersect(i1, i2)")
      assert(!contains(i, 100), "100 is not in intersect(i1, i2)")
      assert(!contains(i, 120), "120 is not in intersect(i1, i2)")
    }
  }
  
  test("diff does not contain common elements") {
    new TestSets {
      val i1: Set = x => x >= 10 && x < 100
      val i2: Set = x => x >= 90 && x < 120
      val i = diff(i1, i2)
      
      assert(!contains(i, 90), "90 is in diff(i1, i2)")
      assert(!contains(i, 99), "99 is in diff(i1, i2)")
      assert(!contains(i, 9), "9 is not in diff(i1, i2)")
      assert(contains(i, 10), "10 is in diff(i1, i2)")
      assert(!contains(i, 100), "100 is in diff(i1, i2)")
      assert(!contains(i, 119), "119 is in diff(i1, i2)")
      assert(!contains(i, 120), "120 is not in diff(i1, i2)")
      
      val i3: Set = x => Array(1,3,4,5,7,1000).contains(x)
      val i4: Set = x => Array(1,2,3,4).contains(x)
      val r = diff(i3, i4)
      assert(!contains(r, 1), "1 should not be in result")
      assert(!contains(r, 2), "2 should not be in result")
      assert(!contains(r, 3), "3 should not be in result")
      assert(!contains(r, 4), "4 should not be in result")
      assert(contains(r, 5), "5 should be in result")
      assert(contains(r, 7), "5 should be in result")
      assert(contains(r, 1000), "5 should be in result")
    }
  }
  
  test("filter is the intersection of set and predicate") {
    new TestSets {
      val t4: Set = x => (x >= 10 && x < 100)
      val p: Int => Boolean = x => (x % 2 ==0)
      val f = filter(t4, p)
      assert(!contains(f, 9), "9 is out (lower than lower bound) of set")
      assert(p(10), "10 is even")
      assert(t4(10), "10 i is in the set")
      assert(contains(f, 10), "10 is in the set and even")
      assert(!p(11), "11 is not even")
      assert(!contains(f, 11), "11 is in the set and odd")
      assert(contains(f, 98), "98 is in the set and even")
      assert(!contains(f, 99), "99 is in the set and odd")
      assert(!contains(f, 100), "100 is out (higher than upper bound) of set")
    }
  }
  
  test("forall expects all elements to satisfy the predicate") {
    new TestSets {
      val s: Set = x => (x >= 10 && x < 100)
      assert(!forall(s, x => (x % 2 ==0)), "not all integers between 10 and 100 are even")
      assert(forall(s, x => (x < 2000)), "all integers between 10 and 100 are lower than 2000")
      
      val v1 = x => Array(1,2,3,4).contains(x)
      assert(forall(v1, x => x < 5), "All elements in the set are strictly less than 5")
    }
  }
  
  test("exists expects all elements to satisfy the predicate") {
    new TestSets {
      val s: Set = x => (x >= 10 && x < 100)
      assert(exists(s, x => (x % 2 ==0)), "there's at least one even integer between 10 and 100")
      assert(exists(s, x => (x < 2000)), "there's at least one integer between 10 and 100 that is lower than 2000")
      assert(!exists(s, x => (x > 2000)), "there's no integer between 10 and 100 that is greater than 2000")
    }
  }
  
  test("map expects to apply the function to all elements of the set") {
    new TestSets {
      val evens: Set = x => (x % 2 == 0)
      val odds = map(evens, x => x + 1 )
      assert(contains(odds, 3), "3 is odd")
      assert(!contains(odds, 4), "4 is not odd")
      
      val s: Set = x => Array(1,3,4,5,7,1000).contains(x)
      val r = map(s, x => x * 2)
      assert(forall(r, x => (x % 2 == 0)), "The set obtained by doubling all numbers should contain only even numbers.")
    }
  }
}
