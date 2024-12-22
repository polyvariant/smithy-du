//> using dep software.amazon.smithy:smithy-model:1.53.0
//> using dep software.amazon.smithy:smithy-codegen-core:1.53.0
//> using dep org.typelevel::cats-core:2.12.0
//> using dep com.lihaoyi::os-lib:0.11.3
//> using dep org.jline:jline-terminal:3.28.0
//> using dep co.fs2::fs2-io::3.11.0
//> using options -Wunused:all, -no-indent
import cats.Show
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.kernel.Resource
import cats.kernel.Eq
import cats.kernel.Order
import cats.syntax.all.*
import org.jline.terminal.TerminalBuilder
import software.amazon.smithy.model.Model
import software.amazon.smithy.model.knowledge.NeighborProviderIndex
import software.amazon.smithy.model.neighbor.RelationshipDirection
import software.amazon.smithy.model.neighbor.Walker
import software.amazon.smithy.model.shapes.ShapeId
import software.amazon.smithy.model.shapes.ToShapeId

import scala.jdk.CollectionConverters.*

import util.chaining.*
import software.amazon.smithy.model.knowledge.KnowledgeIndex
import scala.reflect.ClassTag
import cats.effect.ExitCode

trait State {
  // may be empty in the beginning
  def path: List[ShapeId]
  def children: List[ShapeId]
  def siblings: List[(ShapeId, ShapeClosure)]
  def isCurrent(sibling: ShapeId): Boolean

  def currentShape: ShapeId

  def nextSibling: State
  def previousSibling: State
  def moveDown: State
  def moveUp: State
}

given Show[ShapeId] = Show.fromToString
given Order[ShapeId] = Order.by(_.toString())

case class ShapeClosure(size: Int)

def showRangedSiblings(
  state: State,
  range: Int,
) = ???

enum ScrollItem[+A] {
  case Ellipsis(count: Int)
  case Item(a: A)
}

def scrollable[A](
  items: List[A],
  isCurrent: A => Boolean,
  maxLeft: Int,
  maxRight: Int,
): List[ScrollItem[A]] = {

  val current = items.indexWhere(isCurrent)
  val begin = current - maxLeft
  val end = current + maxRight
  val newItems = items.slice(begin, end).map(ScrollItem.Item(_))

  val prefix =
    if begin > 0 then ScrollItem.Ellipsis(begin) :: Nil
    else
      Nil
  val suffix =
    if items.size > end then ScrollItem.Ellipsis(items.size - end) :: Nil
    else
      Nil

  prefix ++ newItems ++ suffix
}

extension (m: Model) {

  def knowledgeOf[T <: KnowledgeIndex: ClassTag](f: Model => T): T = m
    .getKnowledge(
      scala.reflect.classTag[T].runtimeClass.asInstanceOf[Class[T]],
      f(_),
    )

}

case class ShapeCountIndex private (c: Int) extends KnowledgeIndex

object ShapeCountIndex {

  def of(model: Model): Int =
    model.knowledgeOf { m =>
      ShapeCountIndex(
        m.getShapeIds()
          .asScala
          .filterNot(_.hasMember())
          .size
      )
    }.c

}

def render(state: State, model: Model): String = {

  val shapeCount = ShapeCountIndex.of(model)

  val pathStr = show"/ ${state.path.toNel.map(_.reverse.mkString_("-> ", " -> ", "")).orEmpty}"

  val siblingSelector = scrollable(
    items = state.siblings,
    isCurrent = state.isCurrent.compose(_._1),
    maxLeft = 5,
    maxRight = 5,
  )
    .map {
      case ScrollItem.Ellipsis(n) => show"  ...$n more..."
      case ScrollItem.Item((shapeId, closure)) =>
        val prefix =
          if state.isCurrent(shapeId) then show"${Console.GREEN}> "
          else
            " " * 2

        val suffix =
          if state.isCurrent(shapeId) then Console.RESET
          else
            ""

        val content = show"""$shapeId$suffix (${closure.size})"""

        show"$prefix$content"
    }
    .mkString("\n")

  val preview = scrollable(
    state.children,
    isCurrent = _ => true /* we want it to match immediately */,
    0,
    maxRight = 5,
  )
    .map {
      case ScrollItem.Ellipsis(n) => show"- ...$n more..."
      case ScrollItem.Item(child) => "- " ++ child.show
    }
    .mkString_("Preview:\n\n", "\n", "")

  show"""${Console.BLUE}smithy-du loaded with $shapeCount shapes.${Console.RESET}
        |Current path: $pathStr
        |
        |$siblingSelector
        |
        |$preview""".stripMargin
}

object State {

  case class Impl(
    index: Int,
    siblings: List[(ShapeId, ShapeClosure)],
    path: List[ShapeId],
    getChildren: ShapeId => List[ShapeId],
    getClosure: ShapeId => ShapeClosure,
    previous: Option[State],
  ) extends State {
    val currentShape: ShapeId = siblings(index)._1

    def isCurrent(sibling: ShapeId): Boolean = currentShape === sibling

    def children: List[ShapeId] = getChildren(siblings(index)._1)

    def nextSibling: State = copy(
      index = (index + 1) % siblings.size
    )

    def previousSibling: State = copy(
      index = (index - 1 + siblings.size) % siblings.size
    )

    def moveDown: State = {
      val self = siblings(index)

      if (children.isEmpty)
        this
      else
        Impl(
          index = 0,
          siblings = children.fproduct(getClosure),
          path = self.head :: path,
          getChildren = getChildren,
          getClosure = getClosure,
          previous = Some(this),
        )
    }

    def moveUp: State = previous.getOrElse(this)
  }

  def init(model: Model): State = {

    val siblings = topLevelShapes(model).fproduct(_.closure(model))

    Impl(
      index = 0,
      siblings = siblings.sortBy(-_._2.size),
      path = Nil,
      getChildren = _.children(model),
      getClosure = _.closure(model),
      previous = None,
    )

  }

}

extension (shape: ToShapeId) {

  def children(model: Model): List[ShapeId] = NeighborProviderIndex
    .of(model)
    .getProvider()
    .getNeighbors(model.expectShape(shape.toShapeId()))
    .asScala
    .map(_.getNeighborShapeId())
    .toList
    .flatMapOrKeep {
      case s if s.hasMember => model.expectShape(s).asMemberShape().get().getTarget() :: Nil
    }
    .distinct
    .sortBy(-_.closure(model).size)

  def closure(model: Model) = ShapeClosure(
    Walker(model)
      .walkShapeIds(model.expectShape(shape.toShapeId()))
      .asScala
      // member shapes aren't counted for closure sizes
      .filterNot(_.hasMember())
      .filterNot(_ == shape.toShapeId())
      .size
  )

}

given Eq[RelationshipDirection] = Eq.fromUniversalEquals

def topLevelShapes(model: Model): List[ShapeId] =
  // def isRecursive(id: ShapeId): Boolean = TopologicalIndex
  //   .of(model)
  //   .getRecursiveShapes
  //   .contains(model.expectShape(id))

  model
    .getShapeIds()
    .asScala
    .filterNot(_.getNamespace() === "smithy.api")
    .filterNot(_.hasMember())
    .filter { sh =>
      def isNotReferenced =
        NeighborProviderIndex
          .of(model)
          .getReverseProvider()
          .getNeighbors(model.expectShape(sh))
          .asScala
          .filterNot(_.getDirection() === RelationshipDirection.INVERTED)
          .isEmpty

      // isRecursive(sh) ||
      isNotReferenced
    }
    .filterNot(model.expectShape(_).isMemberShape())
    .toList

def marker(s: String) = println(Console.GREEN + s + Console.RESET)

object SmithyDu extends IOApp {

  enum Key {
    case Up, Down, Right, Left
  }

  def run(args: List[String]): IO[ExitCode] = IO
    .blocking {
      val p = os.FilePath(args.head).resolveFrom(os.pwd)

      val _ = println(s"Loading model from $p...")

      Model
        .assembler()
        .discoverModels()
        .addUnparsedModel(
          p.last,
          os.read(p),
        )
        .assemble()
        .unwrap()
    }
    .flatMap(runWithModel)

  def runWithModel(model: Model) = fs2
    .Stream
    .resource(
      Resource.fromAutoCloseable(IO.blocking(TerminalBuilder.builder().system(true).build()))
    )
    .evalMap { terminal =>
      IO.blocking(terminal.enterRawMode()).void
    }
    .flatMap { _ =>
      fs2.Stream.repeatEval(IO.blocking(System.in.read()))
    }
    .sliding(3)
    .map(_.toList)
    .collect {
      case List(27, 91, 65) => Key.Up
      case List(27, 91, 66) => Key.Down
      case List(27, 91, 67) => Key.Right
      case List(27, 91, 68) => Key.Left
    }
    .scan(State.init(model)) { (state, key) =>
      key match {
        case Key.Up    => state.previousSibling
        case Key.Down  => state.nextSibling
        case Key.Right => state.moveDown
        case Key.Left  => state.moveUp
      }
    }
    .map(render(_, model))
    .evalMap { current =>
      val clearScreen = "\u001b[H\u001b[2J"
      IO.println(clearScreen + current)
    }
    .compile
    .drain
    .as(ExitCode.Success)

}

// marker("base")
// render(State.init(model)).tap(println)

// marker("next")
// render(State.init(model).nextSibling).tap(println)

// marker("child")
// render(State.init(model).nextSibling.moveDown).tap(println)
