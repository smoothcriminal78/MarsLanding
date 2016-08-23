package com.example

import scala.collection.mutable._
import scala.math._

/*
  * Created by Dima on 20.08.2016.
  */
class MarsLander {
  val G = 3.711

  val minX = 4000
  val maxX = 5500
  val maxY = 150

  def plotRoute(q: Queue[LandingData]): Queue[LandingData] = {
    val e = q.last.copy()
    import e._
      if (y > maxY) {
        ang += calcAngle(e)
        th = if (vs < -39) 4 else 3

        //level-off correction
        //val lo = levelOff(q)
        //if(lo.isEmpty) plotRoute(q) else q

        val ab = applyBraking(Queue(e.copy(t=2)), -ang)
        if(ab._1.isEmpty) {
          q += nextPoint(e)
          println("last q " + q.last)
          plotRoute(q)
        } else {
          q++=ab._1
        }
      } else {
        q
      }
    }

  def levelOff(q: Queue[LandingData]): Queue[LandingData] = {
    val e = q.last.copy()
    e.ang += (
      if (e.ang < 15) -e.ang
      else if (e.ang >= 15) 15
      else if (e.ang > -15) -e.ang
      else if (e.ang <= -15) -15
      else 0
      )
    val np = nextPoint(e)
    import np._
    ang match {
      case 0 => if (x >= minX || y <= maxY) q else Queue()
      case _ => {
        q += e.copy(t=1)
        levelOff(q)
      }
    }
  }

  def applyBraking(q: Queue[LandingData], a:Int): (Queue[LandingData], Int) = {
    val e = q.last.copy()
    println("last: " + e)

    e.ang=if(a>0) e.ang+15 else e.ang-15 
    if((a>0 && e.ang>a) || (a<0 && e.ang<a)) e.ang=a 
    
    val np = nextPoint(e)
    import np._
    if(abs(hs)<20){
      if(x>=minX) ((q,a)) else ((Queue(), -1))
    } else {
      q += LandingData(x, y, hs, vs, ang, th, 2);
      applyBraking(q,a)
    }
  }

  def nextPoint(data: LandingData) = {
    import data._
    val incX = round(sin(toRadians(ang)) * th).toInt
    val incY = round(cos(toRadians(ang)) * th - G).toInt

    LandingData(x + hs - incX / 2, y + vs + incY / 2, hs - incX, vs + incY, ang, th)
  }

  def calcAngle(data: LandingData) = {
    import data._
    //val hyp = sqrt(cY*cY+cX*cX)
    val dx: Double = x - minX
    val dy: Double = y - maxY

    val tarAng = round(toDegrees(asin(sin(dx / dy)))).toInt - ang

    if (tarAng > 15) 15
    else if (tarAng < -15) -15
    else tarAng
  }
}

case class LandingData(var x: Int = 0, var y: Int = 0, var hs: Int = 0, var vs: Int = 0, var ang: Int = 0, var th: Int = 0, var t: Int = 0)

object MarsLander {
  def main(args: Array[String]): Unit = {
    val ml = new MarsLander
    val q1 = ml.plotRoute(Queue(LandingData(2500, 2700)))
    //q1.foreach(println)
    //    println(ml.calcAngle(LandingData(4500, 2700)))
  }
}
