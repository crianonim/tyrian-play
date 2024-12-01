package ui

import tyrian.Html
import tyrian.Html.*
import tyrian.*

object Button {
  def interactive[A](label:String,msg:A): Html[A] =
    button(onClick(msg),cls:="p-2 rounded border border-black")(label)
}

object Input {
  def interactive[A](v:String, msg:String=>A): Html[A] =
    input(value:= v ,onInput(msg),cls:="p-2 rounded border border-black")
}