package myorg

import cats.effect.IO
import myorg.Page.MainPage
import tyrian.Html.*
import tyrian.*

import scala.scalajs.js.annotation.*

@JSExportTopLevel("TyrianApp")
object JanTyrianApp extends TyrianIOApp[Msg, JanTyrianApp.Model]:
  case class Model(tick: Double,  page: Page,
                   dndModel: PageDnd.Model,
                   counterModel: PageCounter.Model,
                   gameModel: PageGame.Model
                  )

  def router: Location => Msg =
    case loc: Location.Internal =>
      loc.pathName match
        case "/" => Msg.NavigateTo(Page.MainPage)
        case "/counter" => Msg.NavigateTo(Page.CounterPage)
        case "/dnd" => Msg.NavigateTo(Page.DndPage)
        case "/clock" => Msg.NavigateTo(Page.ClockPage)
        case "/game" => Msg.NavigateTo(Page.GamePage)
        case _ => Msg.NoOp

    case loc: Location.External =>
      Msg.NoOp


  def init(flags: Map[String, String]): (Model, Cmd[IO, Msg]) =
    val (gameModel, gameEff) = PageGame.init
    (Model(0,  MainPage, PageDnd.init, PageCounter.init, gameModel),
      gameEff.map(Msg.UpdatePageGame.apply)
    )

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) =

    case Msg.NoOp => (model, Cmd.None)
    case myorg.Msg.UpdatePageDnd(m) =>
      val (newModel, newCMD) = PageDnd.update(model.dndModel)(m)
      (model.copy(dndModel = newModel), newCMD.map(Msg.UpdatePageDnd.apply))
    case myorg.Msg.UpdatePageCounter(m) => (model.copy(counterModel = PageCounter.update(model.counterModel)(m)), Cmd.None)
    case Msg.UpdatePageGame(m) =>
      val (newModel, eff) = PageGame.update(model.gameModel)(m)
      (model.copy(gameModel = newModel), eff.map(Msg.UpdatePageGame.apply))
    case Msg.NavigateTo(p) => (model.copy(page = p), Cmd.None)
    case Msg.Tick(t: Double) => (model.copy(tick = t, gameModel = PageGame.update(model.gameModel)(PageGame.Msg.Tick(t))._1  ), Cmd.None)


  def view(model: Model): Html[Msg] =
    div(cls:="flex flex-col gap-2 p-10")(
      div(cls:="flex gap-4")(
        a(href:="/")("Main"),
        a(href:="/counter")("Counter"),
        a(href:="/dnd")("Dnd"),
        a(href:="/clock")("Clock"),
        a(href:="/game")("Game"),
      ),
      model.page match
        case Page.MainPage =>
          div()(span()(model.toString))
        case Page.DndPage => PageDnd.view(model.dndModel).map(Msg.UpdatePageDnd.apply)
        case Page.ClockPage => PageClock.view(model.tick)
        case Page.CounterPage => PageCounter.view(model.counterModel).map(Msg.UpdatePageCounter.apply)
        case Page.GamePage => PageGame.view(model.gameModel).map(Msg.UpdatePageGame.apply)
    )

  def subscriptions(model: Model): Sub[IO, Msg] =
    Sub.Batch[IO, Msg](
      Sub.animationFrameTick("tick")(Msg.Tick.apply),
      model.page match
        case Page.GamePage => PageGame.subscriptions(model.gameModel).map(Msg.UpdatePageGame.apply)
        case _ => Sub.None
    )


enum Msg:
  case NoOp
  case UpdatePageDnd(m: PageDnd.Msg)
  case UpdatePageCounter(m: PageCounter.Msg)
  case UpdatePageGame(m: PageGame.Msg)
  case NavigateTo(p: Page)
  case Tick(t: Double)


enum Page:
  case MainPage, DndPage, ClockPage, CounterPage, GamePage