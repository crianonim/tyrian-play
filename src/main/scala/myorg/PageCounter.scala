package myorg

import tyrian.Html.*

object PageCounter {
  type Model = Int
  enum Msg:
    case Increment, Decrement
    
  def init: Model = 0
  
  def update(model: Model): Msg => Model =
    case Msg.Increment => model+1
    case Msg.Decrement => model-1
    
  def view(model: Model)=
    div(
      button(onClick(Msg.Decrement))("-"),
      div(model.toString),
      button(onClick(Msg.Increment))("+")
    )
  
}


