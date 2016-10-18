import cats._
import cats.data._
import cats.implicits._
import cats.free.Free

object Ant {
  sealed trait Color extends Serializable with Product
  final case class White() extends Color
  final case class Black() extends Color

  sealed trait Direction extends Serializable with Product
  final case class North() extends Direction
  final case class South() extends Direction
  final case class West() extends Direction
  final case class East() extends Direction

  sealed trait TurnDirection extends Serializable with Product
  final case class Left() extends TurnDirection
  final case class Right() extends TurnDirection

  final case class Field(direction: Direction, cur: (Int, Int), black: Vector[(Int, Int)])

  sealed trait Instruction[A] extends Serializable with Product
  final case class Move() extends Instruction[Unit]
  final case class Turn(tD: TurnDirection) extends Instruction[Unit]
  final case class GetColor() extends Instruction[Color]
  final case class ChangeColor() extends Instruction[Unit]
  final case class GetField() extends Instruction[Field]

  type AntInstruction[A] = Free[Instruction, A]

  def changeColor(): AntInstruction[Unit] =
    Free.liftF(ChangeColor())

  def move(): AntInstruction[Unit] =
    Free.liftF(Move())

  def turn(tD: TurnDirection): AntInstruction[Unit] =
    Free.liftF(Turn(tD))

  def step(): AntInstruction[Unit] = {
    for {
      color <- getColor()
      _ <- turn(colorToTurnDirection(color))
      _ <- changeColor()
      _ <- move()
    } yield ()
  }

  def getColor(): AntInstruction[Color] =
    Free.liftF(GetColor())

  def getField(): AntInstruction[Field] =
    Free.liftF(GetField())

  def colorToTurnDirection(color: Color) = color match {
    case White() => Left()
    case Black() => Right()
  }
}

object AntImplementations {
  import Ant._

  def printField(field: Field): Unit = {
    val max = 10
    for (row <- 0 until max)
      for(col <- 0 until max) {
        val p = if (col == max - 1) (x: String) => println(x) else (x: String) => print(x)
        val elem = (row, col)
        if (field.black.contains(elem)) {
          if (field.cur == elem)
            p("A")
          else
            p("B")
        } else
          if (field.cur == elem)
            p("A")
          else
            p("W")
      }
  }

  val nextDirection = (direction: Direction, tD: TurnDirection) => tD match {
    case Left() =>
      direction match {
        case North() => West()
        case West() => South()
        case South() => East()
        case East() => North()
      }
    case Right() =>
      direction match {
        case North() => East()
        case East() => South()
        case South() => West()
        case West() => North()
      }
  }

  val nextLocation = (cur: (Int, Int), direction: Direction) => direction match {
    case North() => (cur._1 - 1, cur._2)
    case South() => (cur._1 + 1, cur._2)
    case West() => (cur._1, cur._2 - 1)
    case East() => (cur._1, cur._2 + 1)
  }

  val impureCompiler: Instruction ~> Id =
    new (Instruction ~> Id) {
      val field = collection.mutable.Set[(Int, Int)]()
      var cur = (3,3)
      var direction: Direction = North()

      def apply[A](fa: Instruction[A]): Id[A] = fa match {
        case GetColor() =>
          if (field.contains(cur))
            Black()
          else
            White()
        case ChangeColor() =>
          if (field.contains(cur))
            field -= cur
          else
            field += cur
          ()
        case Move() =>
          cur = nextLocation(cur, direction)
          ()
        case Turn(tD) =>
          direction = nextDirection(direction, tD)
          ()
        case GetField() =>
          Field(direction, cur, field.toVector)
      }
    }

  type AntInstructionState[A] = State[Field, A]

  val pureCompiler: Instruction ~> AntInstructionState =
    new (Instruction ~> AntInstructionState) {
      def apply[A](fa: Instruction[A]): AntInstructionState[A] =
        fa match {
          case GetColor() =>
            State.inspect(i =>
              if (i.black.contains(i.cur))
                Black()
              else
                White()
            )
          case ChangeColor() =>
            State.modify(field =>
              if (field.black.contains(field.cur))
                field.copy(black = field.black.filterNot(_ == field.cur))
              else
                field.copy(black = field.black :+ field.cur)
            )
          case Turn(tD) =>
            State.modify(i => i.copy(direction = nextDirection(i.direction, tD)))
          case Move() =>
            State.modify(i => i.copy(cur = nextLocation(i.cur, i.direction)))
          case GetField() =>
            State.inspect(i => i)
        }
    }
}

object FreeTest {
  def main(args: Array[String]): Unit = {
    import Ant._, AntImplementations._

    val program: AntInstruction[Field] =
      for {
        _ <- step()
        _ <- step()
        x <- getField()
      } yield x

    val impure:cats.Id[Field] = program.foldMap(impureCompiler)
    println(s"impure: $impure")

    val pure:Eval[Field] = program.foldMap(pureCompiler).runA(Field(North(), (3,3), Vector.empty))
    println(s"pure: ${pure.value}")

    printField(pure.value)
  }
}
