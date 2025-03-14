package com.iteso.maf.challenge.exercises.problems

import akka.http.scaladsl.server.Route

case object Problem1 extends Problem {


  implicit class StringOps(string: String) {
    def getStr(i: Int): String = scala.util.Try(string.charAt(i).toString).toOption.getOrElse("")
  }

  final case class MixedString(first: String, second: String, mixed: String)


  /**
    * Mixing Strings
    * Description:
    * This endpoint expects a get request with two strings as parameters (i.e. firstWord, secondWord).
    * Your goal is to take both words and create a third one by intercalating the letters
    * (e.g. "ABC" and "123" should be "A1B2C3").
    * - Note that the words might have different length.
    * - Your expression should evaluate to a MixedString object.
    * - Try to use scala's collections and methods (hint: map).
    *
    * Key Points:
    * > The code required to parse the request/response is provided to you.
    *   - For more information on the AKKA-HTTP API see: <https://doc.akka.io/docs/akka-http/current/server-side/index.html>
    * > You should complete the code in the block expression for the challengeSolution val.
    * > The "StringOps implicit class" is commonly known as a type class.
    *   - For more information on type classes see (advanced): <https://blog.scalac.io/2017/04/19/typeclasses-in-scala.html>
    * > The "final case class MixedString" must contain the resulting mixed string in the filed "mixed".
    *   - For more information about case classes see: <https://docs.scala-lang.org/tour/case-classes.html>
    *
    * Example:
    * Get request: /problems/1?firstWord=abcdef&secondWord=1234
    * Response: {"first":"abcdef","second":"1234","mixed":"a1b2c3d4ef"}
    */

  val var1 =  first.zip(second).map(p => p._1.toString + p._2.toString).mkString
  val comb = (first.length, second.length) match {
    case (primero, segundo) if primero > segundo =>  var1 + first.substring(segundo, primero)
    case (tercero, cuarto) if cuarto > tercero => var1 + second.substring(tercero, cuarto)
    case _ => var1
  }
  MixedString(first = first, second = second, mixed = mixed)


