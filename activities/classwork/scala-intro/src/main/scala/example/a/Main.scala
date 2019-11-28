package example.a

object Main {
  case class IntListResult(len: Int, sum: Int, mult: Int, str: String)

  sealed trait IntList {

    // Example method that uses pattern matching
    def map(f: Int => Int): IntList = this match {
      case Final => Final
      case Item(head, tail) => Item(f(head), tail.map(f))
    }

    // A) Implement the length method
    def length: Int = this match {
      case Final => 0
      case Item(head, tail) => tail.length + 1
    }

    // B) Implement the sum method
    def sum: Int = this match{
      case Final => 0
      case Item(head, tail) => head + tail.sum
    }

    // C) Implement a generalization of the above methods and call it fold.
    def fold(end: Int, f: (Int, Int) => Int): Int = this match {
      case Final => end
      case Item(head, tail) => f(head, tail.fold(end, f))
    }
    // para length es b + 1 y para sum es a + b cuando f es f(a,b)

    // D) Implement a generic fold (generalization over the fold on C).
    def genericFold[B](end: B, f: (Int, B) => B): B = this match {
      case Final => end
      case Item(head, tail) => f(head, tail.genericFold(end, f))
    }

  }

  final object Final extends IntList
  final case class Item(head: Int, tail: IntList) extends IntList

  val solution: Route = path("5") {
    get {
      parameters('a.as[Int], 'b.as[Int], 'c.as[Int]) {
        (a, b, c) => {

          val myList = Item(a, Item(b, Item(c, Final)))

          // E) Complete the challengeResponse with the inner multiplication of the list using the fold method.
          // F) Use the generic fold to create a string representation of the list.
          val challengeResponse: IntListResult = {
            val resultLen: Int = myList.length
            val resultSum: Int = myList.sum
            val resultMult: Int = myList.fold(end = 1, f = (num: Int, z: Int) => num*z)
            val resultStr: String = myList.genericFold(end = "", f = (num: Int, z: String) => num.toString + " -> " + z)
            IntListResult(len=resultLen,sum=resultSum,mult= resultMult,str=resultStr)
          }

          complete(challengeResponse)
        }
      }
    }
  }
}
package example.a.model

case class Timestamp(seconds: Int){
  def +(other: Timestamp): Timestamp =  //puedo sustituir add por +
    Timestamp(seconds + other.seconds)
}


object Timestamp { //metodos estaticos
  val secondsInHour: Int = 60 * 60
  val secondsInMinutes: Int = 60

  def apply(hours: Int, minutes: Int, seconds: Int): Timestamp =
    Timestamp(seconds = secondsInHour * hours + secondsInMinutes * minutes + seconds)

}
}