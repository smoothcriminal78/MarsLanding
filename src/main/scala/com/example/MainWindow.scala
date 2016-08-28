package com.example

import java.awt.Color

import scala.collection.mutable
import scala.swing.Swing._
import scala.swing.BorderPanel.Position._
import scala.swing._
import scala.swing.event.ButtonClicked

import scala.util.Random

/**
  * Created by Dima on 20.08.2016.
  */
object MainWindow extends SimpleSwingApplication {
  //easy on the right
  //val ground = List(0, 100, 1000, 500, 1500, 1500, 3000, 1000, 4000, 150, 5500, 150, 6999, 800)

  //high ground
//  val ground = List(0,1000, 300,1500, 350,1400, 500,2100, 1500,2100, 2000,200, 2500,500, 2900,300, 3000,200, 3200,1000, 3500,500, 3800,800, 4000,200, 4200,800, 4800,600, 5000,1200, 5500,900, 6000,500, 6500,300, 6999,500)

  var routes = Map[Int, List[LandingData]]()

  override def top = new MainFrame {
    title = "Mars Landing"

    val button = new Button {
      text = "Start"
    }
    val canvas = new Panel {
      background = Color.black
      preferredSize = (1000, 500)

      override def paintComponent(g: Graphics2D): Unit = {
        super.paintComponent(g)
        //g.setColor(Color.WHITE)
        //Seq.fill(100)(Random.nextInt(1000)).sliding(2).foreach(l=>if(l(1)<350)g.drawOval(l(0), l(1), 1, 1))

        g.setColor(Color.RED)
        MarsLander.segments.sliding(2).foreach(l => g.drawLine(l(0)._1/10, size.height- l(0)._2/10, l(1)._1/10, size.height - l(1)._2/10))
        routes.foreach {e =>
          g.setColor(new Color(Random.nextFloat, Random.nextFloat, Random.nextFloat))
          e._2.sliding(2).foreach(l => g.drawLine(l.head.x/10, size.height - l.head.y/10, l.last.x/10, size.height - l.last.y/10))
          val ld = e._2.last
          g.drawString("x: %d, y: %d, hs: %d, vs %d, ang %d, th %d".format(ld.x, ld.y, ld.hs, ld.vs, ld.ang, ld.th), ld.x/10, size.height - ld.y/10)
        }
      }
    }

    contents = new BorderPanel {
      layout(button) = North
      layout(canvas) = Center
    }

    listenTo(button)

    reactions += {
      case ButtonClicked(component) if component == button => {
        val ml = new MarsLander
        val r = ml.plotRoute(MarsLander.init).flatMap(List(_)).toList
        routes += (if (routes.isEmpty) 0 else routes.keys.max + 1) -> r
        println("---------ROUTE---------")
        r.foreach(println)

        repaint()
      }
    }
  }
}
