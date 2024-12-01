package myorg

import cats.effect.IO
import tyrian.Html.*
import tyrian.*
import tyrian.SVG.*

import org.scalajs.dom.document
import org.scalajs.dom.KeyboardEvent
import tyrian.cmds.{LocalStorage, Random}
import scala.concurrent.duration.DurationInt

object PageGame {

  case class Model(start: Int, width: Int, position: Double, running: Boolean,
  didHit: Boolean, level: Int, hs: Int)

  enum Msg:
    case NoOp,RandomiseBar, Restart
    case KeyPressed(k: String)
    case GotRandomWidth(w: Int)
    case GotRandomPosition(p: Int)
    case GotHS(hs: Int)
    case Tick(t: Double)

  val WIDTH: Int = 500
  val SPEED = 200

  def waitSec(s: Int) = Cmd.emitAfterDelay[IO, Msg](Msg.Restart, s.seconds)

  def getLocalStorageHS = LocalStorage.getItem[IO, Msg]("hs") {
    case Right(LocalStorage.Result.Found(value)) => Msg.GotHS(value.toInt)
    case Left(LocalStorage.Result.NotFound(e)) => Msg.NoOp
  }

  def saveHS(hs: Int) = LocalStorage.setItem[IO, Msg]("hs", hs.toString)(_ => Msg.NoOp)

  def currentSpeed(model: Model): Int = SPEED + model.level * 25

  def randomiseWidth: Cmd[IO, Msg] = Random.int[IO](40).map(x => Msg.GotRandomWidth.apply(30 + x.value))

  def randomisePosition(width: Int): Cmd[IO, Msg] = Random.int[IO](WIDTH - width).map(x => Msg.GotRandomPosition.apply(x.value))

  def isHit(model: Model): Boolean =
    val tickTimesSpeed = (model.position * currentSpeed(model)).round
    val direction = (tickTimesSpeed / WIDTH) % 2
    val position = if (direction == 0) (tickTimesSpeed % WIDTH) else (WIDTH - (tickTimesSpeed % WIDTH))
    position >= model.start && position <= (model.start + model.width)
  def init:(Model, Cmd[IO, Msg]) = ( Model(0,0,0,true,false,0,0)  ,
    Cmd.Batch[IO,Msg](randomiseWidth,getLocalStorageHS)
  )


  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) =
    case Msg.KeyPressed(k) => if (model.running)
        (model.copy(running = false, didHit = isHit(model)), waitSec(1))
       else (model, Cmd.None)
    case Msg.GotRandomWidth(w) => (model.copy(width = w), randomisePosition(w))
    case Msg.GotRandomPosition(p) => (model.copy(start = p), Cmd.None)
    case Msg.RandomiseBar => (model, randomiseWidth)
    case Msg.Restart => (model.copy(running = true, level = if (model.didHit) model.level + 1 else 0, hs = Math.max(model.level + 1, model.hs)), if (model.didHit && model.level + 1 > model.hs) Cmd.Batch(randomiseWidth, saveHS(model.level + 1)) else randomiseWidth)
    case Msg.GotHS(hs) => (model.copy(hs = hs), Cmd.None)
    case Msg.NoOp => (model,Cmd.None)
    case Msg.Tick(t: Double) => (model.copy( position = if (model.running) t else model.position), Cmd.None)


  def view(model: Model) =
    val tickTimesSpeed = (model.position*currentSpeed(model)).round
    val direction =( tickTimesSpeed / WIDTH) % 2
    val position = if (direction==0)  (tickTimesSpeed % WIDTH) else (WIDTH - (tickTimesSpeed % WIDTH))
    div(cls:="flex flex-col gap-2 p-4")(
      div()(s"Level ${model.level}. Local High Score: ${model.hs}"),
      div()(if (model.running) "Try to hit the black box" else if (model.didHit) "You hit it and gained a level, will be faster now" else "You missed, back to 0"),
      div()(
        svg(viewBox := s"0, 0, $WIDTH, 20", width := s"${WIDTH}px")(
          rect(x:="0",y:="0",width:=WIDTH.toString,height:="40", stroke := "#eeeeee", fill :="#eeeeee"),
          rect(x:= model.start.toString , y:="0",  width:=model.width.toString, height:="20"
            , stroke := "#023963"),
          line(x1:=position.toString,x2:=position.toString, y1:="0", y2:="20", stroke:="#ff3333")
        )
        ,
        button(onClick(Msg.RandomiseBar))("Randomise"),

      )
    )

  def subscriptions(model: Model): Sub[IO, Msg] =
      Sub.fromEvent("keydown", document) {
        case k: KeyboardEvent =>
          Option(Msg.KeyPressed(k.key))
      }

}
