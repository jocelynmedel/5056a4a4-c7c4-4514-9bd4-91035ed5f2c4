package example.d

object Main {
  [19:11, 20/11/2019] Natasha: import example.a.model.Timestamp
  //comportamiento
  sealed trait Visitor {
    def id: String
    def createdAt: Timestamp

    def chose
  }
  //clase
  object Visitor{
    final case class Anonymous(id: String) extends Visitor
    final case class User(id:String, email: String, createdAt: Timestamp) extends Visitor

  }
  [19:45, 20/11/2019] Alisson: package example.d

  import java.util.UUID

  import example.a.model.Timestamp
  import example.d.model.Visitor

  object Main {

    def older(v1: Visitor, v2: Visitor): Boolean =
      v1.createdAt.seconds > v2.createdAt.seconds

    def getAnonymousUser(age: Int): Visitor = Visitor.Anonymous( //definimos el metodo
      UUID.randomUUID().toString,
      Timestamp(age))


    val getUser: Int => Visitor = (age: Int) => Visitor.User( //definimos la funcion
      id = UUID.randomUUID().toString,
      email = "email@example.com",
      createdAt = Timestamp(age)
    )

    // First case: sbt "runMain example.d.Main 100 90"
    // Second case: sbt "runMain example.d.Main 100 900"
    def main(args: Array[String]): Unit = {
      val Array(firstAge, secondAge) = args

      val a = getAnonymousUser(firstAge.toInt)
      val b = getUser(secondAge.toInt)

      // Print older user
      if (older(a, b)) a.show() else b.show()
    }
  }
}
