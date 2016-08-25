package com.example

import scala.collection.mutable._
import scala.math._

/*
  * Created by Dima on 20.08.2016.
  */
class MarsLander {
  val G = 3.711

//  val p1 = 4000
//  val p2 = 5500
//  val alt = 150

  val p1 = 500
  val p2 = 1500
  val alt = 2100

  val segments = ArrayBuffer((0,1000), (300,1500), (350,1400), (500,2100), (1500,2100), (2000,200), (2500,500), (2900,300), (3000,200), (3200,1000), (3500,500), (3800,800), (4000,200), (4200,800), (4800,600), (5000,1200), (5500,900), (6000,500), (6500,300), (6999,500))

  val ground = segments.sliding(2).filter{case(ArrayBuffer(v1,v2)) => v1._2==v2._2}.toList
  val groundY = ground.map{case(ArrayBuffer(v1,v2))=>v1._2}.max
  val groundX = ground.map{case(ArrayBuffer(v1,v2))=>(v1._1, v2._1)}.max

  //val p1 = groundX._1
  //val p2 = groundX._2
  //val alt = groundY

  def plotRoute(q: Queue[LandingData]): Queue[LandingData] = {
    val e = q.last.copy()
    println(e)
    import e._
      if (y > alt) {
        ang += calcAngle(e)
        //th = if (vs < -39) 4 else 3
        th = if(ang>40 || vs < -39) 4 else 3
        if(x<p1 || x>p2){
          val ab = applyBraking(Queue(e.copy(t=2)), -ang)
          if(ab._1.isEmpty) {
            q += nextPoint(e)
          } else {
            q++=ab._1
          }
          plotRoute(q)
          } else if(ang!=0) {
          //level-off correction
          val lo = levelOff(Queue(q.last.copy(t=1)))
          q++=lo
          plotRoute(q)
        } else {
          q += nextPoint(e)
          plotRoute(q)
        }
      } else {
        q
      }
    }

  def levelOff(q: Queue[LandingData]): Queue[LandingData] = {
    val e = q.last.copy()
    e.ang += (
      if (e.ang < 15) -e.ang
      else if (e.ang >= 15) -15
      else if (e.ang > -15) e.ang
      else if (e.ang <= -15) 15
      else 0
      )
    val np = nextPoint(e)
    import np._
    q += np
    ang match {
      case 0 => if (x >= p1 || x <= p2 || y <= alt) q else Queue()
      case _ => levelOff(q)
    }
  }

  def applyBraking(q: Queue[LandingData], a:Int): (Queue[LandingData], Int) = {
    val e = q.last.copy()
    println(e)
    e.ang=if(a>0) e.ang+15 else e.ang-15
    if((a>0 && e.ang>a) || (a<0 && e.ang<a)) e.ang=a

    e.th = if(e.ang>40 || e.vs < -39) 4 else 3

    val np = nextPoint(e)
    import np._
    if(abs(hs)<20){
      if(y>=alt && x>=p1 && x<=p2) ((q,a)) else ((Queue(), -1))
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
    val dx: Double = x - (if(x<p1) p1 else if(x>p2) p2 else p1+(p2-p1)/2)
    val dy: Double = y - alt

    val tarAng = round(toDegrees(atan(dy/dx))).toInt - ang
    println(s"$tarAng")
    if (tarAng > 0 && tarAng < 15) tarAng
    else if (tarAng < 0 && tarAng > -15) -tarAng
    else if (tarAng >= 15) 15
    else if (tarAng <= -15) -15
    else 0
  }
}

case class LandingData(var x: Int = 0, var y: Int = 0, var hs: Int = 0, var vs: Int = 0, var ang: Int = 0, var th: Int = 0, var t: Int = 0)

object MarsLander {
  def main(args: Array[String]): Unit = {
    val ml = new MarsLander
    val q1 = ml.plotRoute(Queue(LandingData(6500, 2700, hs=50, ang=90)))
    //val q1 = ml.plotRoute(Queue(LandingData(2500, 2700)))
    q1.foreach(println)
    //    println(ml.calcAngle(LandingData(4500, 2700)))
  }
}
