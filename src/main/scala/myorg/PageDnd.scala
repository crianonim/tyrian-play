package myorg


import cats.effect.IO
import cats.syntax.either.*
import io.circe.parser.*
import tyrian.Html.*
import tyrian.*
import tyrian.http.*
import myorg.PageDnd.Msg.{DecoderError, GetDragon, NoOp}


object PageDnd {
  case class Model(monster: Option[Monster], inputValue: String, error: Option[String])

  def init: Model =
    Model(None,"",None)

  def view(model: Model): Html[Msg] = div(cls:="flex flex-col gap-2")(

    div(cls:="flex gap-2")(ui.Input.interactive(model.inputValue,Msg.Input.apply),
    ui.Button.interactive("Get Monster",GetDragon)),
    div()(model.monster.fold(text(""))(x=>viewMonster(x))),
    div()(model.error.fold(text(""))(text))
  )

  def viewMonster(monster:Monster): Html[Msg] =
    div(cls:="border p-2 border-grey-500 flex flex-col gap-2") (
      div()(s"Name: ${monster.name}"),
      div()(s"Index: ${monster.index}"),
        div()(s"Type/Subtype: ${monster.type_}/ ${monster.subtype.getOrElse("-")}"),
    )
  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) =
    case NoOp => (model, Cmd.None)
    case GetDragon => (model, getDragonCommand(model.inputValue))
    case Msg.GotMonster(monster: Monster) => (model.copy(monster = Some(monster)), Cmd.None)
    case DecoderError(err) => (model.copy(error = Some(err)), Cmd.None)
    case Msg.Input(i) => (model.copy(inputValue = i),Cmd.None)

  enum Msg:
    case NoOp, GetDragon
    case GotMonster(m: Monster)
    case DecoderError(err: String)
    case Input(i:String)

  private def getDragonCommand(name:String): Cmd[IO, Msg] = Http.send(Request.get(s"https://www.dnd5eapi.co/api/monsters/$name"), fromHttpResponse)


  private val onError: HttpError => Msg =
    _ => Msg.NoOp

  private val onResponse: Response => Msg = { response =>

    parse(response.body)
      .leftMap(_.message)
      .flatMap(j =>
        val hCursor = j.hcursor

        val res= for {
          name <- hCursor.downField("name").as[String]
          index <- hCursor.downField("index").as[String]
          type_ <- hCursor.downField("type").as[String]
          subtype <- hCursor.downField("subtype").as[Option[String]]

        } yield Monster(name,index,type_,subtype)

        res

      )
      .leftMap(_.toString)
      .fold(Msg.DecoderError(_), Msg.GotMonster(_))
  }


  def fromHttpResponse: Decoder[Msg] = Decoder[Msg](onResponse, onError)



}

case class Monster(name: String, index: String, type_ :String, subtype: Option[String] )



