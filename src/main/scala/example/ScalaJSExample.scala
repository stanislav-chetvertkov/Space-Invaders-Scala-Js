package example

import org.scalajs.dom
import org.scalajs.dom.html
import scala.scalajs.js.annotation.JSExport

@JSExport
object ScalaJSExample {

  @JSExport
  def main(canvas: html.Canvas): Unit = {

    implicit val canv = canvas

    val game = new Game

    canvas.onmousemove = {
      (e: dom.MouseEvent) =>
        game.player.x = e.clientX.toInt
    }

    canvas.onmousedown = {
      (e:dom.MouseEvent) => game.fire
    }

    def run = {
      game.draw()
      game.update()
    }

    dom.setInterval(() => run, 50)
  }
}
