//> using dep software.amazon.smithy:smithy-model:1.53.0
//> using dep software.amazon.smithy:smithy-codegen-core:1.53.0
//> using dep org.typelevel::cats-core:2.12.0
//> using dep com.lihaoyi::os-lib:0.11.3
//> using dep org.jline:jline-terminal:3.28.0
//> using dep co.fs2::fs2-io::3.11.0
//> using options -Wunused:all, -no-indent
import cats.Show
import cats.data.NonEmptyList
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.kernel.Resource
import cats.kernel.Eq
import cats.kernel.Order
import cats.syntax.all.*
import fs2.concurrent.SignallingRef
import org.jline.terminal.Size
import org.jline.terminal.Terminal.Signal
import org.jline.terminal.TerminalBuilder
import software.amazon.smithy.model.Model
import software.amazon.smithy.model.knowledge.KnowledgeIndex
import software.amazon.smithy.model.knowledge.NeighborProviderIndex
import software.amazon.smithy.model.loader.ModelAssembler
import software.amazon.smithy.model.neighbor.RelationshipDirection
import software.amazon.smithy.model.neighbor.Walker
import software.amazon.smithy.model.shapes.ShapeId
import software.amazon.smithy.model.shapes.ToShapeId

import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag

import util.chaining.*
import scala.collection.concurrent.TrieMap

private trait State[A] {
  // may be empty in the beginning
  def path: List[A]
  def children: List[A]
  def siblings: List[(A, Closure)]
  def isCurrent(sibling: A): Boolean

  def currentShape: A

  def nextSibling: State[A]
  def previousSibling: State[A]
  def moveDown: State[A]
  def moveUp: State[A]
}

private given Show[ShapeId] = Show.fromToString
private given Order[ShapeId] = Order.by(_.toString())

private case class Closure(size: Int)

private enum ScrollItem[+A] {
  case Ellipsis(count: Int)
  case Item(a: A)
}

private def scrollable[A](
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

  private def knowledgeOf[T <: KnowledgeIndex: ClassTag](f: Model => T): T = m
    .getKnowledge(
      scala.reflect.classTag[T].runtimeClass.asInstanceOf[Class[T]],
      f(_),
    )

}

private case class ShapeCountIndex private (c: Int) extends KnowledgeIndex

private object ShapeCountIndex {

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

trait Ops[A] {
  extension (a: A) def trimForHistory: A
}

given Ops[ShapeId] with {

  extension (s: ShapeId) {

    def trimForHistory: ShapeId = s.withNamespace(
      s.getNamespace().split("\\.").map(_.head).mkString(".")
    )

  }

}

// todo: do something with terminal size
private def render[A: Show: Ops](model: Model)(state: State[A], terminalSize: Size): String = {

  val shapeCount = ShapeCountIndex.of(model)

  val pathStr =
    show"/ ${state
        .path
        .toNel
        .map { case NonEmptyList(head, tail) => NonEmptyList(head, tail.map(_.trimForHistory)) }
        .map(_.reverse.mkString_("-> ", " -> ", ""))
        .orEmpty}"

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

  private case class Impl[A: Eq](
    index: Int,
    siblingItems: List[A],
    path: List[A],
    getChildren: A => List[A],
    getClosure: A => Closure,
    parent: Option[State[A]],
  ) extends State[A] {
    lazy val siblingCount = siblingItems.size

    lazy val siblings: List[(A, Closure)] = siblingItems.fproduct(getClosure).sortBy(-_._2.size)

    lazy val currentShape: A = siblings(index)._1
    lazy val children: List[A] = getChildren(siblings(index)._1)

    def isCurrent(sibling: A): Boolean = currentShape === sibling

    def nextSibling: State[A] = copy(
      index = (index + 1) % siblingCount
    )

    def previousSibling: State[A] = copy(
      index = (index - 1 + siblingCount) % siblingCount
    )

    def moveDown: State[A] = {
      val self = siblings(index)

      if (children.isEmpty)
        this
      else
        Impl(
          index = 0,
          siblingItems = children,
          path = self.head :: path,
          getChildren = getChildren,
          getClosure = getClosure,
          parent = Some(this),
        )
    }

    def moveUp: State[A] = parent.getOrElse(this)
  }

  def init(model: Model): State[ShapeId] = Impl(
    index = 0,
    siblingItems = topLevelShapes(model),
    path = Nil,
    getChildren = _.children(model),
    getClosure = _.closure(model),
    parent = None,
  )

}

case class ClosureIndex private (model: Model) extends KnowledgeIndex {
  private val closureCache: TrieMap[ShapeId, Closure] = TrieMap.empty

  def forShape(shapeId: ShapeId): Closure = closureCache.getOrElseUpdate(
    shapeId,
    Closure(
      Walker(model)
        .walkShapeIds(model.expectShape(shapeId))
        .asScala
        // member shapes aren't counted for closure sizes
        .filterNot(_.hasMember())
        .filterNot(_ === shapeId)
        .size
    ),
  )

}

object ClosureIndex {

  def of(model: Model): ClosureIndex = model.knowledgeOf(ClosureIndex(_))
}

extension (shape: ToShapeId) {

  private def children(model: Model): List[ShapeId] = NeighborProviderIndex
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

  private def closure(model: Model) = ClosureIndex.of(model).forShape(shape.toShapeId())

}

given Eq[RelationshipDirection] = Eq.fromUniversalEquals

private def topLevelShapes(model: Model): List[ShapeId] =
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

def log(s: String) = IO.blocking(os.write.append(os.pwd / "log.txt", s + "\n"))

def logUnsafe(s: String): Unit = {
  import cats.effect.unsafe.implicits.global

  log(s).unsafeRunAndForget()
}

object SmithyDu extends IOApp {

  enum Key {
    case Up, Down, Right, Left
  }

  private def loadModel(args: List[String]) = IO.blocking {
    val paths = args.map(os.FilePath(_).resolveFrom(os.pwd))

    val _ = println(s"Loading model from ${args.mkString(", ")}...")

    Model
      .assembler()
      .discoverModels()
      .putProperty(ModelAssembler.ALLOW_UNKNOWN_TRAITS, true)
      .tap { modass =>
        paths.foreach { p =>
          modass.addImport(p.toNIO)
        }
      }
      .disableValidation()
      .assemble()
      .unwrap()
  }

  def run(args: List[String]): IO[ExitCode] = loadModel(args).flatMap(runWithModel)

  private def decode[F[_]]: fs2.Pipe[F, Int, Key] =
    _.sliding(3)
      .map(_.toList)
      .collect {
        case List(27, 91, 65) => Key.Up
        case List(27, 91, 66) => Key.Down
        case List(27, 91, 67) => Key.Right
        case List(27, 91, 68) => Key.Left
      }

  private def applyKey[A]: Key => State[A] => State[A] = {
    case Key.Up    => _.previousSibling
    case Key.Down  => _.nextSibling
    case Key.Right => _.moveDown
    case Key.Left  => _.moveUp
  }

  private def runWithModel(model: Model) = fs2
    .Stream
    .resource(
      Resource.fromAutoCloseable(IO.blocking(TerminalBuilder.builder().system(true).build()))
    )
    .evalTap { terminal =>
      IO.blocking(terminal.enterRawMode())
    }
    .flatMap { term =>
      val mkSizeSignal = fs2
        .Stream
        .eval(
          IO(term.getSize())
            .flatMap(SignallingRef[IO].of)
            .flatTap { ref =>
              IO(
                term.handle(Signal.WINCH, s => ref.set(term.getSize()).unsafeRunSync()(runtime))
              )
            }
            .widen[fs2.concurrent.Signal[IO, Size]]
        )

      val mkStateRef = fs2.Stream.eval(SignallingRef[IO].of(State.init(model)))

      (mkSizeSignal, mkStateRef).flatMapN { (size, stateRef) =>
        val applyKeys = fs2
          .Stream
          .repeatEval(IO.blocking(System.in.read()))
          .through(decode[IO])
          .map(applyKey)
          .foreach(stateRef.update)

        val renderState =
          (stateRef, size)
            .mapN(render(model))
            .discrete

        renderState
          .changes
          .concurrently(applyKeys)
      }
    }
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
