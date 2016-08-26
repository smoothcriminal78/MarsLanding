package com.example

import scala.collection.mutable._
import scala.math._

/*
  * Created by Dima on 20.08.2016.
  */
class MarsLander {
  val G = 3.711

  //val p1 = 4000
  //val p2 = 5500
  //val alt = 150

  val segments = ArrayBuffer((0, 1000), (300, 1500), (350, 1400), (500, 2100), (1500, 2100), (2000, 200), (2500, 500), (2900, 300), (3000, 200), (3200, 1000), (3500, 500), (3800, 800), (4000, 200), (4200, 800), (4800, 600), (5000, 1200), (5500, 900), (6000, 500), (6500, 300), (6999, 500))

  val ground = segments.sliding(2).filter { case (ArrayBuffer(v1, v2)) => v1._2 == v2._2 }.toList
  val groundY = ground.map { case (ArrayBuffer(v1, v2)) => v1._2 }.max
  val groundX = ground.map { case (ArrayBuffer(v1, v2)) => (v1._1, v2._1) }.max

  val p1 = groundX._1
  val p2 = groundX._2
  val alt = groundY

  /**
    * Calculates shift in degrees - by 15 degrees max for turn
    *
    * @param a1 current angle
    * @param a2 target angle
    **/
  def rangeAng(a1: Int, a2: Int) = {
    val delta = if (a1 > 0 && a2 > 0) if (a1 > a2) a2 - a1 else a1 - a2
    else if (a1 > 0 && a2 < 0) -1 * (a1 + abs(a2))
    else if (a1 < 0 && a2 < 0) if (a1 > a2) -1 * (abs(a2) - abs(a1)) else abs(a2) - abs(a1)
    else if (a1 < 0 && a2 > 0) abs(a1) + a2
    else if (a1 == 0 && a2 > 0) a2 else if (a1 == 0 && a2 < 0) a2 else if (a1 > 0 && a2 == 0) -a1 else if (a1 < 0 && a2 == 0) abs(a1) else 0

    delta match {
      case d if d >= 15 => 15
      case d if d <= -15 => -15
      case d if (d > 0 && d < 15) || (d > -15 && d < 0) => d
      case 0 => 0
    }
  }

  /**
    * Calculates throttle shift - by 1 for turn
    *
    * @param th1 current throttle
    * @param th2 target throttle
    **/
  def rangeThr(th1: Int, th2:Int) = {
    if(th1 > th2 && th1 > 0) -1 else if(th1 < th2 && th1 < 4) 1 else 0
  }

  def plotRoute(q: Queue[LandingData]): Queue[LandingData] = {
    val e = q.last.copy()
    println(e)

    import e._

    if (y > alt) {
      ang += rangeAng(ang, calcAngle(e))
      th+=rangeThr(th, calcThrottle(e))
      if (x < p1 || x > p2) {
        val ba = if(ang< -45) -22 else if (ang>45) 22 else ang
        val ab = applyBraking(Queue(e.copy(t = 2)), -ba)
        if (ab._1.isEmpty) {
          q += nextPoint(e)
        } else {
          q ++= ab._1
        }
        plotRoute(q)
      } else if (ang != 0) {
        //level-off correction
        val lo = levelOff(Queue(q.last.copy(t = 1)))
        q ++= lo
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
    //println(s"leveloff: $e")
    e.ang += rangeAng(e.ang, 0)

    val np = nextPoint(e)

    import np._

    q += np
    ang match {
      case 0 => if (x >= p1 || x <= p2 || y <= alt) q else Queue()
      case _ => levelOff(q)
    }
  }

  def applyBraking(q: Queue[LandingData], a: Int): (Queue[LandingData], Int) = {
    val e = q.last.copy()
    println(s"braking: $e")
    e.ang += rangeAng(e.ang, a)
    e.th+=rangeThr(e.th, calcThrottle(e))

    val np = nextPoint(e)

    import np._

    if (abs(hs) <= 20 || y <= alt) {
      if (y >= alt && x >= p1 && x <= p2) ((q, a)) else ((Queue(), -1))
    } else {
      q += LandingData(x, y, hs, vs, ang, th, 2);
      applyBraking(q, a)
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

    val dx: Double = x - (if (x < p1) p1 else if (x > p2) p2 else p1 + (p2 - p1) / 2)
    val dy: Double = y - alt

    val a = round(toDegrees(atan(dx / dy))).toInt
    if (a > 45) 22 else if (a < -45) -22 else a
  }

  def calcThrottle(data: LandingData) = {
    import data._
    if (ang <= -22 || ang >= 22 || vs < -39) 4 else if(ang>22 || ang< -22) 2 else 3
  }
}

case class LandingData(var x: Int = 0, var y: Int = 0, var hs: Int = 0, var vs: Int = 0, var ang: Int = 0, var th: Int = 0, var t: Int = 0)

object MarsLander {
  def main(args: Array[String]): Unit = {
    val ml = new MarsLander
    val q1 = ml.plotRoute(Queue(LandingData(6500, 2700, hs = -50, ang = 90)))
    //val q1 = ml.plotRoute(Queue(LandingData(2500, 2700)))
    //q1.foreach(println)
    //    println(ml.calcAngle(LandingData(4500, 2700)))
  }
}
