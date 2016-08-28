package com.example

import scala.collection.mutable._
import scala.math._
import MarsLander._

/*
  * Created by Dima on 20.08.2016.
  */

class MarsLander {
  val G = 3.711

  /**
    * Calculates shift in degrees - by 15 degrees max for turn
    *
    * @param a1 current angle
    * @param a2 target angle
    **/
  def rangeAng(a1: Int, a2: Int) = {
    val delta = if (a1 > 0 && a2 > 0) a2 - a1
    else if (a1 > 0 && a2 < 0) -1 * (a1 + abs(a2))
    else if (a1 < 0 && a2 < 0) if (a1 > a2) -1 * (abs(a2) - abs(a1)) else abs(a1) - abs(a2)
    else if (a1 < 0 && a2 > 0) abs(a1) + a2
    else if (a1 == 0) a2
    else if (a1 > 0 && a2 == 0) -a1
    else if (a1 < 0 && a2 == 0) abs(a1)
    else 0

    val res = delta match {
      case d if d >= 15 => a1 + 15
      case d if d <= -15 => a1 - 15
      case d if d > -15 && d < 15 => a1 + d
      case 0 => a1
    }

    //    if (res > 90) 90 else if (res < -90) -90 else res
    if (res > 45) 45 else if (res < -45) -45 else res
  }

  /**
    * Calculates throttle shift - by 1 for turn
    *
    * @param th1 current throttle
    * @param th2 target throttle
    **/
  def rangeThr(th1: Int, th2: Int) = {
    if (th1 > th2 && th1 > 0) th1 - 1 else if (th1 < th2 && th1 < 4) th1 + 1 else th1
  }

  def plotRoute(q: Queue[LandingData]): Queue[LandingData] = {
    val e = q.last.copy()
    println(e)

    import e._

    if (y > alt) {
      val ia = calcAngle(e)
      val ca = if (x >= p1 && x <= p2 && ang == 0) 0 else if (ia > 45) 22 else if (ia < -45) -22 else ia
      ang = rangeAng(ang, ca)
      th = rangeThr(th, calcThrottle(e))
      if (x < p1 || x > p2) {

        val ba = ang
        val ab = braking(Queue(e.copy(t = 2)), -ba)

//        val ab = braking(Queue(e.copy(t = 2)), -ca)
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
    e.ang = rangeAng(e.ang, 0)

    val np = nextPoint(e)

    import np._

    q += np
    ang match {
      case 0 => if (x >= p1 || x <= p2 || y <= alt) q.tail else Queue()
      case _ => levelOff(q)
    }
  }

  def braking(q: Queue[LandingData], a: Int): (Queue[LandingData], Int) = {
    val e = q.last.copy()
    println(s"braking -------------------------------------------> $e")
    e.ang = rangeAng(e.ang, a)
    e.th = rangeThr(e.th, calcThrottle(e))
    val np = nextPoint(e)

    import np._
    q += LandingData(x, y, hs, vs, ang, th, 2)
//    (hs <= 20 && x >= p1 && x + hs <= p2) || (hs >= -20 && hs < 0 && x <= p2 && x + hs >= p1) ||
    if (hs == 0 || e.x == np.x || e.x == np.x + np.hs || y <= alt || x < 0) {
      //      println(s"braking -------------------------------------------> $e")
      //      println(s"braking -------------------------------------------> $np")
      if (y >= alt && x >= p1 + 100 && x <= p2 - 100) (q.tail, a) else (Queue(), -1)
    }
    else braking(q, a)
  }

  def nextPoint(data: LandingData) = {

    import data._

    val incX = round(sin(toRadians(ang)) * th).toInt
    val incY = round(cos(toRadians(ang)) * th - G).toInt

    LandingData(x + hs - incX / 2, y + vs + incY / 2, hs - incX, vs + incY, ang, th, t)
  }

  def calcAngle(data: LandingData) = {

    import data._

    val dx: Double = x - (if (x < p1) p1 else if (x > p2) p2 else p1 + (p2 - p1) / 2)
    val dy: Double = y - alt

    round(toDegrees(atan(dx / dy))).toInt
  }

  def calcThrottle(data: LandingData) = {
    import data._
    if (ang <= -22 || ang >= 22 || vs < -39) 4 else if (ang < 22 || ang > -22) 3 else 2
  }
}

case class LandingData(var x: Int = 0, var y: Int = 0, var hs: Int = 0, var vs: Int = 0, var ang: Int = 0, var th: Int = 0, var t: Int = 0)

object MarsLander {
  //easy on the right
//    val segments = ArrayBuffer((0, 100), (1000, 500), (1500, 1500), (3000, 1000), (4000, 150), (5500, 150), (6999, 800))
//    val init = Queue(LandingData(2500, 2700))

  //initial speed, correct side
//  val segments = ArrayBuffer((0, 100), (1000, 500), (1500, 100), (3000, 100), (3500, 500), (3700, 200), (5000, 1500), (5800, 300), (6000, 1000), (6999, 2000))
//  val init = Queue(LandingData(6500, 2800, hs = -100, ang = 90))

  //initial speed, wrong side
//      val segments = ArrayBuffer((0, 100), (1000, 500), (1500, 1500), (3000, 1000), (4000, 150), (5500, 150), (6999, 800))
//      val init = Queue(LandingData(6500, 2800, hs = -90, ang = 90))

  //Deep canyon
//            val segments = ArrayBuffer((0, 1000), (300, 1500), (350, 1400), (500, 2000), (800, 1800), (1000, 2500), (1200, 2100), (1500, 2400), (2000, 1000), (2200, 500), (2500, 100), (2900, 800), (3000, 500), (3200, 1000), (3500, 2000), (3800, 800), (4000, 200), (5000, 200), (5500, 1500), (6999, 2800))
//            val init = Queue(LandingData(500, 2700, hs = 100, ang = -90))

  //high ground
    val segments = ArrayBuffer((0, 1000), (300, 1500), (350, 1400), (500, 2100), (1500, 2100), (2000, 200), (2500, 500), (2900, 300), (3000, 200), (3200, 1000), (3500, 500), (3800, 800), (4000, 200), (4200, 800), (4800, 600), (5000, 1200), (5500, 900), (6000, 500), (6500, 300), (6999, 500))
    val init = Queue(LandingData(6500, 2700, hs = -50, ang = 90))

  val landSpot = segments.sliding(2).filter { case (ArrayBuffer(v1, v2)) => v1._2 == v2._2 }.toList
  val landSpotY = landSpot.map { case (ArrayBuffer(v1, v2)) => v1._2 }.max
  val landSpotX = landSpot.map { case (ArrayBuffer(v1, v2)) => (v1._1, v2._1) }.max

  val p1 = landSpotX._1
  val p2 = landSpotX._2
  val alt = landSpotY

  def main(args: Array[String]): Unit = {
    println(s"p1 $p1 p2 $p2 alt $alt")
    val ml = new MarsLander
    val q1 = ml.plotRoute(init)

    //    val q2 = ml.braking(Queue(LandingData(1472,816,-206,-106,-15,4,0)), )
    //    q2._1.foreach(println)
    q1.foreach(println)
  }
}
