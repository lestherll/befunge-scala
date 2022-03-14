package befunge

import befunge.*

import scala.io.StdIn.{readInt, readChar}
import scala.util.Random
import scala.collection.mutable.Stack
import scala.util.Try

@main def test(): Unit =
  Interpreter.interpret("hello.befunge")
  println()
