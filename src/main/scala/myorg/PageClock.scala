package myorg

import tyrian.Html.*
import tyrian.SVG.*

object PageClock {
  def view(tick: Double) =
    val angle = tick * 2 * math.Pi / 60 - math.Pi / 2
    val handX = 50 + 40 * math.cos(angle)
    val handY = 50 + 40 * math.sin(angle)
    svg(viewBox := "0, 0, 100, 100", width := "300px")(
            circle(
              cx   := "50",
              cy   := "50",
              r    := "45",
              fill := "#0B79CE"
            ),
            line(
              x1     := "50",
              y1     := "50",
              x2     := handX.toString,
              y2     := handY.toString,
              stroke := "#023963"
            )
        )

}
 
