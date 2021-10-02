package befunge

import befunge.*

import scala.collection.mutable.Stack
import scala.util.Try

@main def test: Unit = 
  Interpreter.interpret("hello.befunge")

object Interpreter:
  private var stack: Stack[Int] = Stack[Int]()
  private var dir: Direction = Right
  private var program: Array[Array[Char]] = Array[Array[Char]]()
  var pc: (Int, Int) = (0, 0)

  def advance() = dir match
    case Up => updatePC(pc._1, pc._2-1)
    case Down => updatePC(pc._1, pc._2+1)
    case Right => updatePC(pc._1+1, pc._2)
    case Left => updatePC(pc._1-1, pc._2)

  private def updatePC(x: Int, y: Int): (Int, Int) = 
    pc = (x, y)
    pc

  private def useOperator(operator: Char) = operator match
    case '+' => (a: Int, b: Int) => a + b
    case '-' => (a: Int, b: Int) => b - a
    case '*' => (a: Int, b: Int) => a * b
    case '/' => (a: Int, b: Int) => b / a
    case '%' => (a: Int, b: Int) => b % a

  def readFromFile(filename: String) =
    val file = Try(io.Source.fromFile("./src/main/scala/resources/" +filename))                //open file
    val data = file.map(_.getLines().map(_.split("").map(_.head)).toArray) //split each line
                  .getOrElse(Array.empty[Array[Char]])       //into an Array
    file.fold(println, _.close())

    pc = (0, 0) // reset program counter
    program = data
    data

  def programToString = program foreach { row => row foreach print; println }

  def interpret(filename: String) = 
    val program = readFromFile(filename)

    def loop(x: Int, y: Int, _stringMode: Boolean = false): Int =
      var stringMode = _stringMode
      val currChar = program(pc._2)(pc._1)
//      println(currChar)
      currChar match {
        case '>' | 'v' | '<' | '^' => updateDir(currChar)
        case num if num.isDigit => stack.push(num.asDigit)
        case '+' | '-' | '/' | '*' => stack.push(useOperator(currChar)(stack.pop(), stack.pop()))
        case '"' => stringMode = !stringMode
        case '.' => println(stack.pop())
        case ',' => println(stack.pop().toChar)
        case ' ' if !stringMode =>
        case '@' => return 0
        case c => if stringMode then stack.push(c)
      }
      advance()
      loop(pc._2, pc._1, stringMode)
    end loop
    loop(pc._2, pc._1)
//    println(stack)S
  end interpret

  def updateDir(newDir: Char) = newDir match
    case '>' => dir = Right
    case 'v' => dir = Down
    case '<' => dir = Left
    case '^' => dir = Up
  end updateDir

end Interpreter