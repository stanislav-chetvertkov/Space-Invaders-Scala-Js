package example
import org.scalajs.dom
import org.scalajs.dom.html
import collection.mutable.ArrayBuffer

import scala.scalajs.js.annotation.JSExport

case class Point(x: Int, y: Int){
  def +(p: Point) = Point(x + p.x, y + p.y)
  def /(d: Int) = Point(x / d, y / d)
}

@JSExport
object ScalaJSExample {

  case class Enemy(var x:Int, y:Int)
  case class EnemyBullet(x: Int, var y:Int)

  case class Player(var x:Int, var alive:Boolean)
  case class Bullet(x:Int, var y:Int)
  case class Explosion(x:Int, y:Int, var phase:Int = 20)

  case class EnemyCluster(enemies:ArrayBuffer[Enemy]){
    var direction = "left"
    var stepsLeft = 40

    def move = {
      if (stepsLeft==0){
        if (direction=="left") direction = "right" else direction = "left"
        stepsLeft = 40
      } else {
        if (direction == "left") enemies.foreach( e => e.x -= 1) else enemies.foreach( e => e.x += 1)
        stepsLeft -= 1
      }
    }

    def attack = {
      val int = scala.util.Random.nextInt(20)
      if (int==5){

        val enemyIndex: Int = scala.util.Random.nextInt(enemies.size)
        val enemy: Enemy = enemies(enemyIndex)
        enemyBullets = enemyBullets :+ Bullet(enemy.x, enemy.y)
      }

    }

  }

//  var enemies:ArrayBuffer[Enemy] =
  var enemyCluster = new EnemyCluster(ArrayBuffer(
                              Enemy(50, 50), Enemy(100, 50), Enemy(150, 50), Enemy(200, 50), Enemy(250, 50), Enemy(300, 50),
                              Enemy(50, 150), Enemy(100, 150), Enemy(150, 150), Enemy(200, 150), Enemy(250, 150), Enemy(300, 150),
                              Enemy(50, 250), Enemy(100, 250), Enemy(150, 250), Enemy(200, 250), Enemy(250, 250), Enemy(300, 250)
    )
  )
  val player = Player(0, true)
  var bullets:ArrayBuffer[Bullet] = ArrayBuffer()
  var enemyBullets:ArrayBuffer[Bullet] = ArrayBuffer()
  var explosions:ArrayBuffer[Explosion] = ArrayBuffer[Explosion]()
  var score = 0

  @JSExport
  def main(canvas: html.Canvas): Unit = {
    val renderer = canvas.getContext("2d")
                    .asInstanceOf[dom.CanvasRenderingContext2D]

    def fire = {
      if (bullets.size < 3){
        bullets :+= Bullet(player.x+5, canvas.height-20)
      }
    }

    canvas.width = canvas.parentElement.clientWidth
    canvas.height = canvas.parentElement.clientHeight

    renderer.fillRect(0, 0, canvas.width, canvas.height)

    canvas.onmousemove = {
      (e: dom.MouseEvent) =>
        player.x = e.clientX.toInt
    }

    canvas.onmousedown = {
      (e:dom.MouseEvent) => fire
    }

    def clear() = {
      renderer.fillStyle = "black"
      renderer.fillRect(0, 0, canvas.width, canvas.height)
    }

    def run = {
      draw()
      update()
    }

    def draw() = {
      clear()
      drawPlayer()
      drawBullets()
      drawText()
      drawInvaders()
      drawExplosions()
    }

    def update() = {
      updateBullets()
      detectCollisions()
      enemyCluster.move
      enemyCluster.attack
      updateExplosions()
    }


    def detectCollisions() = {
      def distance(x1:Int, y1:Int, x2:Int, y2:Int):Int = {
        val side1 = Math.abs(x2 - x1)
        val side2 = Math.abs(y2 - y1)
        Math.sqrt(side1*side1 + side2*side2).toInt
      }

      for (
        bullet:Bullet <- bullets;
        enemy:Enemy <- enemyCluster.enemies
      ) {
        val dist = distance(bullet.x, bullet.y, enemy.x, enemy.y)
        if (dist < 25){
          bullets = bullets - bullet
          enemyCluster.enemies -= enemy
          explosions = explosions :+ Explosion(enemy.x, enemy.y)
          score += 1
        }
      }

      for (bullet:Bullet <- enemyBullets) {
        val dist = distance(bullet.x, bullet.y, player.x, canvas.height-20)
        if (dist < 25){
          bullets = bullets - bullet
          player.alive = false
          explosions = explosions :+ Explosion(player.x, canvas.height-20)
        }
      }

    }

    def drawBullets() = {
      renderer.fillStyle = "yellow"
      bullets.foreach( b =>{
        renderer.fillRect(b.x, b.y, 5, 10)
      })

      renderer.fillStyle = "blue"
      enemyBullets.foreach( b  => {
        renderer.fillRect(b.x, b.y, 5, 10)
      })
    }

    def drawText() = {
      renderer.fillStyle = "white"
      renderer.strokeStyle = "#F00"
      renderer.font = "italic 30pt Arial"
      renderer.fillText(score.toString, 500, 50)

      if (!player.alive){
        renderer.font = "bold 30px sans-serif"
        renderer.strokeText("Game over", 500, 20)
      }
    }

    def updateBullets() = {
      bullets.map(b => b.y -= 15)
      bullets = bullets.filter(b => b.y > 0)

      enemyBullets.map(b => b.y += 15)
      enemyBullets = enemyBullets.filter(b => b.y < canvas.height)
    }

    def updateExplosions() = {
      explosions.foreach(e => e.phase += 1)
      explosions = explosions.filterNot(e => e.phase > 50)
    }

    def drawPlayer() = {
      if (player.alive){
        renderer.fillStyle = "green"
        val rect = canvas.getBoundingClientRect()
        renderer.fillRect(player.x - rect.left, canvas.height - 20, 20, 20)
      }
     }

    def drawInvaders() = {
      enemyCluster.enemies.foreach( e => {
        renderer.fillStyle = "red"
        renderer.fillRect(e.x, e.y, 20, 20)
      })
    }

    def drawExplosions() = {
      explosions.foreach( e => {
        renderer.beginPath()
        renderer.arc(e.x, e.y, e.phase, 0, 2 * Math.PI, anticlockwise = false)
        renderer.fillStyle = "green"
        renderer.fill()
        renderer.lineWidth = 5
        renderer.strokeStyle = "#003300"
        renderer.stroke()
      })
    }


    dom.setInterval(() => run, 50)
  }
}
