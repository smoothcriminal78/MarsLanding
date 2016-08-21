package com.example

import math._
import scala.util._
import scala.collection.mutable._

/**
  * Created by Dima on 20.08.2016.
  */
class MarsLander {
  val G = 3.711

  val minX = 4000
  val maxX = 5500
  val maxY = 150

  def plotRoute(q: Queue[LandingData]): Queue[LandingData] = {
    val l = q.last
    import l._
    if (y > maxY) {
      if (x < minX) {
        ang += (if (hs > 19 && ang > -90) 15 else calcRouteTrajectory(l))
      } else {
        ang += (if (hs < -19 && ang > -30) -15 else if (hs > 19 && ang < 30) 15 else 0)
      }
      th += (if (vs > -39 && th < 4) 1 else if (vs < -39 && th > 0) -1 else 0)
      q+=nextPoint(l)
      //level-off correction
//      levelOff(q)
      plotRoute(q)
    } else {
      q
    }
  }

  def levelOff(q: Queue[LandingData]): Queue[LandingData] = {
    q.last.ang += (if (q.last.ang < 0) 15 else if (q.last.ang > 0) -15 else 0)
    val np = nextPoint(q.last)
    import np._
    ang match {
      case 0 => if (x >= minX || y <= maxY) q else Queue()
      case _ => {
        q += LandingData(x, y, hs, vs, ang, th, 1);
        levelOff(q)
      }
    }
  }

  //x = x+vx-0.5*sin(angle*M_PI/180.)*thrust;
  //y = y+vy+0.5*(cos(angle*M_PI/180.)*thrust-G);
  //vx = vx-1.*sin(angle*M_PI/180.)*thrust;
  //vy = vy+1.*(cos(angle*M_PI/180.)*thrust-G);

  def nextPoint(data: LandingData) = {
    import data._
    val incX = round(sin(toRadians(ang)) * th).toInt
    val incY = round(cos(toRadians(ang)) * th - G).toInt

    LandingData(x + hs - incX / 2, y + vs + incY / 2, hs - incX, vs + incY, ang, th)
  }

  def calcRouteTrajectory(data: LandingData) = {
    import data._
    //val hyp = sqrt(cY*cY+cX*cX)
    val dx: Double = abs(x - minX)
    val dy: Double = abs(y - maxY)

    val tilt = (if (x < minX) -1 else 1) * (toDegrees(asin(sin(dx / dy))) + 14) / 15 * 15
    if (ang < tilt) 15 else if (ang > tilt) -15 else 0
  }
}

case class LandingData(var x: Int = 0, var y: Int = 0, var hs: Int = 0, var vs: Int = 0, var ang: Int = 0, var th: Int = 0, var t: Int = 0)

object MarsLander {
  def main(args: Array[String]): Unit = {
    val ml = new MarsLander
    val q1 = ml.plotRoute(Queue(LandingData(2500, 2700)))
    q1.foreach(println)
    val cgq = Queue(LandingData(2500, 2700, 0, 0, 0, 0, 0), LandingData(2500, 2699, 0, -3, -15, 1, 0), LandingData(2501, 2695, 1, -5, -30, 2, 0), LandingData(2503, 2689, 3, -6, -45, 3, 0), LandingData(2508, 2682, 7, -8, -60, 4, 0), LandingData(2517, 2674, 10, -9, -45, 4, 0), LandingData(2528, 2664, 13, -11, -60, 4, 0), LandingData(2543, 2653, 16, -12, -45, 4, 0), LandingData(2560, 2641, 19, -13, -60, 4, 0), LandingData(2581, 2627, 22, -14, -45, 4, 0), LandingData(2604, 2613, 24, -14, -30, 4, 0), LandingData(2629, 2598, 25, -14, -15, 4, 0), LandingData(2654, 2584, 25, -14, 0, 4, 0), LandingData(2679, 2571, 24, -14, 15, 4, 0), LandingData(2702, 2557, 22, -14, 30, 4, 0), LandingData(2723, 2542, 19, -15, 45, 4, 0), LandingData(2742, 2527, 17, -15, 30, 4, 0), LandingData(2759, 2512, 16, -15, 15, 4, 0), LandingData(2775, 2497, 16, -15, 0, 4, 0), LandingData(2792, 2483, 17, -15, -15, 4, 0), LandingData(2810, 2468, 19, -15, -30, 4, 0), LandingData(2831, 2453, 22, -16, -45, 4, 0), LandingData(2854, 2437, 24, -16, -30, 4, 0), LandingData(2879, 2421, 25, -16, -15, 4, 0), LandingData(2905, 2406, 25, -15, 0, 4, 0), LandingData(2929, 2390, 24, -15, 15, 4, 0))
    println("----------------------")
    cgq.foreach(println)
    for (i <- 0 to 24) {
      println("q1: " + q1(i))
      println("cg: " + cgq(i))
    }
    //    ml.plotFall(q1).foreach(println)
  }
}
