package com.example

import scala.math._
import MarsLander._

/*
  * Created by Dima on 20.08.2016.
  */

class MarsLander(segments: List[(Int, Int)]) {
  val G = 3.711

  val (alt, p1, p2, mid) = segments.sliding(2).filter { case Seq(v1, v2) => v1._2 == v2._2 }.map { case Seq(v1, v2) => (v1._2, v1._1, v2._1, v1._1 + (v2._1 - v1._1) / 2) }.max

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

  def plotRoute(q: List[LandingData]): List[LandingData] = {
    val e = q.last.copy()
    e.ang = rangeAng(e.ang, angle(e))
    e.th = rangeThr(e.th, throttle(e))
    val np = nextPoint(e)
    import np._
    if (y > alt) {
      if (x < p1 || x > p2 || abs(hs) > 10) {
        // braking
        val ga = (if (hs < 0) -1 else 1) * (if (abs(e.ang) < 22) 45 else if (abs(e.ang) < 55) 36 else 22)
        val bq = braking(List(e.copy(t = 1)), ga)
        if(bq._1.nonEmpty) plotRoute(q ++ bq._1) else plotRoute(q :+ np)
      } else if (x >= p1 && x <= p2 && (ang != 0 || hs != 0)) {
        //leveloff
        e.ang = if (e.hs > 0) rangeAng(e.ang, 15) else if (e.hs < 0) rangeAng(e.ang, -15) else rangeAng(e.ang, 0)
        plotRoute(q :+ np.copy(t=2))
      } else {
        plotRoute(q :+ np)
      }
    } else q :+ np
  }

  def braking(q: List[LandingData], a:Int): (List[LandingData], Int) = {
    val e = q.last.copy()
    e.ang = rangeAng(e.ang, a)
    e.th = rangeThr(e.th, throttle(e))
    val np = nextPoint(e)

    import np._
    if (hs == 0 || e.x == np.x || y <= alt || x < 0) {
      if (y >= alt && x >= p1 && x <= p2) (q, a) else (List(), a)
    }
    else braking(q :+ np, a)
  }

  def nextPoint(data: LandingData) = {

    import data._

    val incX = round(sin(toRadians(ang)) * th).toInt
    val incY = round(cos(toRadians(ang)) * th - G).toInt

    LandingData(x + hs - incX / 2, y + vs + incY / 2, hs - incX, vs + incY, ang, th, t)
  }

  def angle(data: LandingData) = {

    import data._

    val dx: Double = x - mid
    val dy: Double = y - alt

    (if (abs(hs) > 40) -1 else 1) * (round(toDegrees(atan(dx / dy))).toInt match {
      case a if ang == 0 && x >= p1 && x <= p2 => 0
      case a if a > 39 => 22
      case a if a < -39 => -22
      case a => a
    })
  }

  def throttle(data: LandingData) = {

    import data._

    if (vs > 39 && ang == 0) 3 else 4
  }
}

case class LandingData(var x: Int = 0, var y: Int = 0, var hs: Int = 0, var vs: Int = 0, var ang: Int = 0, var th: Int = 0, var t: Int = 0)

object MarsLander {
  //easy on the right
  val segments = List((0, 100), (1000, 500), (1500, 1500), (3000, 1000), (4000, 150), (5500, 150), (6999, 800))
  val init = List(LandingData(2500, 2700))

  //initial speed, correct side
  //  val segments = List((0, 100), (1000, 500), (1500, 100), (3000, 100), (3500, 500), (3700, 200), (5000, 1500), (5800, 300), (6000, 1000), (6999, 2000))
  //  val init = Queue(LandingData(6500, 2800, hs = -100, ang = 90))

  //initial speed, wrong side
  //                  val segments = List((0, 100), (1000, 500), (1500, 1500), (3000, 1000), (4000, 150), (5500, 150), (6999, 800))
  //                  val init = Queue(LandingData(6500, 2800, hs = -90, ang = 90))

  //Deep canyon
  //          val segments = List((0, 1000), (300, 1500), (350, 1400), (500, 2000), (800, 1800), (1000, 2500), (1200, 2100), (1500, 2400), (2000, 1000), (2200, 500), (2500, 100), (2900, 800), (3000, 500), (3200, 1000), (3500, 2000), (3800, 800), (4000, 200), (5000, 200), (5500, 1500), (6999, 2800))
  //          val init = Queue(LandingData(500, 2700, hs = 100, ang = -90))

  //high ground
  //      val segments = List((0, 1000), (300, 1500), (350, 1400), (500, 2100), (1500, 2100), (2000, 200), (2500, 500), (2900, 300), (3000, 200), (3200, 1000), (3500, 500), (3800, 800), (4000, 200), (4200, 800), (4800, 600), (5000, 1200), (5500, 900), (6000, 500), (6500, 300), (6999, 500))
  //      val init = Queue(LandingData(6500, 2700, hs = -50, ang = 90))
}
