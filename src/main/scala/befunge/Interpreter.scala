package befunge

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.Stack
import scala.io.StdIn.{readChar, readInt}
import scala.util.{Random, Try}


trait Direction

case object Up extends Direction

case object Down extends Direction

case object Right extends Direction

case object Left extends Direction


var dir: Direction = Up

object Interpreter:
  private val directions: Array[Direction] = Array(Up, Down, Right, Left)
  var pc: (Int, Int) = (0, 0)
  private val stack: mutable.Stack[Int] = mutable.Stack[Int]()
  private var dir: Direction = Right
  private var program: Array[Array[Char]] = Array.fill(25) {
    Array.fill(80) {
      ' '
    }
  }

  def printProgram(program: Array[Array[Char]]): Unit = program foreach { row => row foreach print; println }

  def interpret(filename: String): Int =
    val program = readFromFile(filename)

    @tailrec
    def loop(x: Int, y: Int, _stringMode: Boolean = false, _program: Array[Array[Char]] = program): Int =
      var stringMode = _stringMode
      val program = _program
      val currChar = program(pc._2)(pc._1)

      (currChar, stringMode) match
        case (num, false) if num.isDigit => stack.push(num.asDigit)
        case ('>' | 'v' | '<' | '^', false) => updateDir(currChar)
        case ('+' | '-' | '/' | '*' | '%' | '`', false) => stack.push(useOperator(currChar)(stack.pop, stack.pop))
        case ('!', false) => stack.push(if stack.pop() > 0 then 0 else 1)
        case ('_', false) => if stack.pop == 0 then updateDir('>') else updateDir('<')
        case ('|', false) => if stack.pop == 0 then updateDir('v') else updateDir('^')
        case (':', false) => {
          if stack.isEmpty then
            stack.push(0)
            stack.push(0)
          else
            stack.push(stack.head)
          end if
        }
        case ('"', _) => stringMode = !stringMode
        case ('\\', false) =>
          val a = stack.pop
          val b = stack.pop
          stack.push(a)
          stack.push(b)
        case ('?', false) => directions(Random.nextInt(directions.length))
        case ('$', false) => stack.pop
        case ('#', false) => advance(program)
        case ('g', false) => {
          val y = stack.pop
          val x = stack.pop
          if y < program.length && x < program(y).length then
            stack.push(program(y)(x))
          else
            stack.push(0)
          end if
        }
        case ('p', false) => {
          val y = stack.pop
          val x = stack.pop
          println(s"x=$x y=$y")
          val v = stack.pop.toChar
          program(y)(x) = v
        }
        case ('&', false) => stack.push(readInt())
        case ('~', false) => stack.push(readChar())
        case ('.', false) => print(s"{stack.pop()}")
        case (',', false) => print(s"${stack.pop().toChar}")
        case ('@', false) => return 0
        case (c, true) => stack.push(c)
        case (' ', false) | (_, false) =>
      end match

      advance(program)
      loop(pc._2, pc._1, stringMode, program)
    end loop

    loop(pc._2, pc._1)

  end interpret

  def advance(program: Array[Array[Char]]): (Int, Int) =
    val height = program.length
    val width = program(0).length
    dir match
      case Up => updatePC(pc._1, wrap(pc._2 - 1, 0, height - 1))
      case Down => updatePC(pc._1, wrap(pc._2 + 1, 0, height - 1))
      case Right => updatePC(wrap(pc._1 + 1, 0, width - 1), pc._2)
      case Left => updatePC(wrap(pc._1 - 1, 0, width - 1), pc._2)
    end match
  end advance

  def wrap(num: Int, min: Int, max: Int): Int =
    if num > max then min
    else if num < min then max
    else num

  private def updatePC(x: Int, y: Int): (Int, Int) =
    pc = (x, y)
    pc
  end updatePC

  private def useOperator(operator: Char): (Int, Int) => Int = operator match
    case '+' => (a: Int, b: Int)
      => a + b
    case '-' => (a: Int, b: Int)
      => b - a
    case '*' => (a: Int, b: Int)
      => a * b
    case '/' => (a: Int, b: Int)
      => b / a
    case '%' => (a: Int, b: Int)
      => b % a
    case '`' => (a: Int, b: Int)
      => if b > a then 1 else 0
  end useOperator

  def readFromFile(filename: String): Array[Array[Char]] =
    val file = Try(io.Source.fromFile("./src/main/scala/resources/" + filename)) //open file
    val data = file.map(_.getLines().map(_.split("").map(_.head)).toArray) //split each line
      .getOrElse(Array.empty[Array[Char]]) //into an Array
    file.fold(println, _.close())

    pc = (0, 0) // reset program counter
    program = data
    data
  end readFromFile

  def updateDir(newDir: Char): Unit = newDir match
    case '>' => dir = Right
    case 'v' => dir = Down
    case '<' => dir = Left
    case '^' => dir = Up
  end updateDir

end Interpreter