package com.iteso.maf.challenge.exercises.problems

import akka.http.scaladsl.server.Route

case object Problem4 extends Problem {

  /**
    * Introduction to options
    * Description:
    * This endpoint introduces the concept of scala Options, a commonly known data structure in functional programming.
    * The main goal is to provide a type safe endpoint that can perform an operation between two integers
    * (i.e. a, b) defined by the parameter "operation".
    * The variable "params" contains a scala immutable Map with the parameters of the get request. Use the typesafe
    * method get (e.g. params.get("key) : Option[String]) to extract the option value of the parameter. In order to
    * get familiar with the terms, follow these instructions:
    *
    * A) Create an implicit class named "StringOps" with a method: def asInt: Option[Int]. This method should
    * implement a typesafe conversion between a string to an Int (see Problem1 for an example).
    * - For further information on implicits see: <https://youtu.be/Oij5V7LQJsA>
    * - For further information on type/implicits classes see: <https://docs.scala-lang.org/overviews/core/implicit-classes.html>
    *
    * B) Create a method with the following signature: def calculate(ops: String, a: Int, b: Int): Option[Int].
    * This method should implement the operations {sum, subtraction, multiplication, division} with pattern matching
    * and return None for any other operation (string pattern).
    * - For further information on pattern matching see: <https://youtu.be/1vxIRkYZfmc>
    *
    * C) Use for comprehension and map/flatMap as needed in order to extract the values from the parameter map (params)
    * and perform the operation with the "calculate" method. Create the variable "challengeResponse"
    * as an Option[Calculation].
    * - For further information on for-comprehension/map/flatMap with Options see: <https://youtu.be/Mw_Jnn_Y5iA>
    * - For further information on for-comprehension see: <https://docs.scala-lang.org/tutorials/FAQ/yield.html>
    * - For further information on map and flatMap see: <book Essential Scala (5.5 Sequencing Computation)>
    *
    * Key Points:
    * > Use pattern matching for the "calculate" method.
    * > Use for comprehension and/or map and flatMap.
    * > Note the definition of map and flatMap for an object of type F[A]:
    * - map(f: A => B) => F[B]
    * - flatMap(f: A => F[B]) => F[B]
    *
    * Examples:
    * Get Request: /problems/4?a=1&b=2&operation=sum
    * Response: {"operation":"sum","a":1,"b":2,"result":3}
    * Get Request: /problems/4?a=1&b=2&operation=summ
    * Response (bad request): The request contains bad syntax or cannot be fulfilled.
    */

  case class Calculation(operation: String, a: Int, b: Int, result: Int)

  val solution: Route = path("4") {
    get {
      parameterMap {
        params => {
          // <---- Your code starts here. --->

          // A) Implicit class here with:
          // def asInt: Option[Int] = ???
          implicit class StringOps(String:String) {
            def asInt(): Option[Int] = scala.util.Try(String.toInt).toOption
          }

          // B) Implement the calculate method.
          // def calculate(ops: String, a: Int, b: Int): Option[Int] = ???
          def calculate(ops: String, a: Int, b:Int): Option[Int] = ops match {
            case "sum" => Some(a + b)
            case "subtraction" => Some(a - b)
            case "multiplication" => Some(a * b)
            case "division" => if(b != 0) Some (a/b) else None
            case _ => None
          }

          // C) Complete the challenge response variable.
          // val challengeResponse: Option[Calculation] = ???
          val optA: Option[Int] = params.get("a"). flatMap (a => a.asInt())
          val optB: Option[Int] = params.get("b"). flatMap (b => b.asInt())
          val optOp: Option[String] = params.get("operation")
          def result: Option[Calculation] =
            optA.flatMap(
              a => optB.flatMap(
                b => optOp.flatMap(
                  op => calculate(op,a,b).map(
                    r => Calculation(op,a,b,r)
                  )
                )
              )
            )
          val challengeResponse: Option [Calculation] = result
          // <---- Your code ends  here. ---->
          challengeResponse match {
            case None => badRequestResponse
            case Some(calc) => complete(calc)
          }
        }
      }
    }
  }

}