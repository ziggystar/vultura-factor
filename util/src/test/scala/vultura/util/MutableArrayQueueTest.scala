package vultura.util

import org.specs2._

/**
 * Created by thomas on 24.06.14.
 */
class MutableArrayQueueTest extends Specification {
  def queue(capacity: Int) = new MutableArrayQueue[Int](capacity)
  override def is =
    "dequeue first element" ! {
      val q = queue(2)
      q.enqueue(1)
      (q.dequeue === 1) and (q.isEmpty must beTrue)
    } ^
   "throw on empty dequeue" ! (queue(1).dequeue must throwA[NoSuchElementException]) ^
   "dequeue two elements" ! {
     val q = new MutableArrayQueue[Int](2)
     q.enqueue(1)
     q.enqueue(2)
     (q.dequeue === 1) and (q.dequeue === 2)
   } ^
  "full queue must not be empty" ! {
    val q = new MutableArrayQueue[Int](1)
    q.enqueue(1)
    q.isEmpty must beFalse
  } ^
  "empty queue must have correct size" ! (queue(2).size === 0) ^
  "queue must report correct initial capacity" ! (queue(2).capacity === 2) ^
  "non-empty queue must have correct size" ! {
    val q = queue(2)
    q.enqueue(1)
    q.size === 1
  } ^
  "full queue must have correct size" ! {
    val q = queue(2)
    q.enqueue(1)
    q.enqueue(2)
    q.size === 2
  } ^
  "grown queue must have correct size" ! {
    val q = queue(1)
    q.enqueue(1)
    q.enqueue(2)
    q.size === 2
  } ^
  "grow queue" ! {
    val q = queue(1)
    q.enqueue(1)
    q.enqueue(2)
    (q.dequeue === 1) and (q.dequeue === 2)
  } ^
  "check indexed access" ! {
    val q = queue(1)
    (0 until 10).foreach(q.enqueue)
    q.toList === (0 until 10).toList
  } ^
  "wrap around" ! {
    val q = queue(2)
    q.enqueue(1)
    q.enqueue(2)
    q.dequeue()
    q.enqueue(3)
    (q.capacity === 2) and (q.toList === List(2,3))
  } ^
  "correct size after wrap" ! {
    val q = queue(2)
    q.enqueue(1)
    q.enqueue(2)
    q.dequeue()
    q.enqueue(3)
    q.size === 2
  }
}
