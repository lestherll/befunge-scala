package befunge

import befunge.*

import scala.io.StdIn.{readInt, readChar}
import scala.util.Random
import scala.collection.mutable.Stack
import scala.util.Try

@main def test: Unit = 
  Interpreter.interpret("input.befunge")
  println()

object Interpreter:
  private var stack: Stack[Int] = Stack[Int]()
  private var dir: Direction = Right
  private var program: Array[Array[Char]] = Array.fill(25){Array.fill(80){' '}}
  private val directions: Array[Direction] = Array(Up, Down, Right, Left)
  var pc: (Int, Int) = (0, 0)

  def advance = dir match
    case Up => updatePC(pc._1, pc._2-1)
    case Down => updatePC(pc._1, pc._2+1)
    case Right => updatePC(pc._1+1, pc._2)
    case Left => updatePC(pc._1-1, pc._2)
  end advance

  private def updatePC(x: Int, y: Int): (Int, Int) = 
    pc = (x, y)
    pc
  end updatePC

  private def useOperator(operator: Char): (Int, Int) => Int = operator match
    case '+' => (a: Int, b: Int) => a + b
    case '-' => (a: Int, b: Int) => b - a
    case '*' => (a: Int, b: Int) => a * b
    case '/' => (a: Int, b: Int) => b / a
    case '%' => (a: Int, b: Int) => b % a
    case '`' => (a: Int, b: Int) => if b > a then 1 else 0
  end useOperator

  def readFromFile(filename: String) =
    val file = Try(io.Source.fromFile("./src/main/scala/resources/" +filename))                //open file
    val data = file.map(_.getLines().map(_.split("").map(_.head)).toArray) //split each line
                  .getOrElse(Array.empty[Array[Char]])       //into an Array
    file.fold(println, _.close())

    pc = (0, 0) // reset program counter
    program = data
    data
  end readFromFile

  def printProgram(program: Array[Array[Char]]) = program foreach { row => row foreach print; println }

  def interpret(filename: String) = 
    val program = readFromFile(filename)

    def loop(x: Int, y: Int, _stringMode: Boolean = false, _program: Array[Array[Char]] = program): Int =
      var stringMode = _stringMode
      var program = _program
      val currChar = program(pc._2)(pc._1)

      (currChar, stringMode) match
        case (num, false) if num.isDigit => stack.push(num.asDigit)
        case ('>' | 'v' | '<' | '^', false) => updateDir(currChar)
        case ('+' | '-' | '/' | '*' | '%' | '`', false) => stack.push(useOperator(currChar)(stack.pop, stack.pop))
        case ('!', false) => stack.push(if stack.pop() > 0 then 1 else 0)
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
        case ('\\', false) => {
          val a = stack.pop
          val b = stack.pop
          stack.push(a)
          stack.push(b)
        }
        case ('?', false) => directions(Random.nextInt(directions.length))
        case ('$', false) => stack.pop
        case ('#', false) => advance
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
          println(s"x=${x} y=${y}")
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

      advance
      loop(pc._2, pc._1, stringMode, program)
    end loop
    loop(pc._2, pc._1)
    
  end interpret

  def updateDir(newDir: Char) = newDir match
    case '>' => dir = Right
    case 'v' => dir = Down
    case '<' => dir = Left
    case '^' => dir = Up
  end updateDir

end Interpreter